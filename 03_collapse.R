# ==============================================================================
# Script: 03_collapse.R
#
# Takes the processed worker-day panel, filters to valid workers (workers within valid components of mobility graph for which the AKM FE were calculated, including non-movers), defines
# worker mobility (spells, movedate, post), aggregates to worker-plant-period,
# and computes dist.
#
# Input:  Data/Analysis_data/processed/data_processed.dta
#         Data/Analysis_data/valid_workers.dta  (from AKM worker FE output)
# Output: Data/Analysis_data/processed/collapsed_[period].dta
# ==============================================================================

library(haven)    # read/write .dta files
library(dplyr)    # data manipulation
library(lubridate) # floor_date() for period binning
library(data.table)


# ==============================================================================
# 1. SETTINGS
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

# --- Period choice -----------------------------------------------------------
# Controls how worker-day data is aggregated. One of: "week", "month".
# This label also appears in the output filename.
PERIOD <- "week"

# --- Column names ------------------------------------------------------------
# These should match what prepare_data.R produced.
COL_WORKER  <- "id_employee"
COL_PLANT   <- "camion"
COL_DATE    <- "date"
COL_DAY     <- "day"

# Absence columns: adjust names if data uses different ones.
COL_ABSENT_IND  <- "absent"          # binary: 1 if absent that day

# Shift timing columns
COL_START_HOUR       <- "start_hour"        # actual start hour (categorical/numeric)
COL_SHIFT_START_TIME <- "shift_start_time"  # scheduled start time (stored as string or numeric)

# --- Paths -------------------------------------------------------------------
input_data    <- file.path(script_dir, "Data", "Analysis_data", "processed", "data_processed.dta")
input_workers <- file.path(script_dir, "Outputs", "AKM", "worker_fe_all_week.dta")
output_dir    <- file.path(script_dir, "Outputs", "Collapsed")
output_path   <- file.path(output_dir, paste0("collapsed_", PERIOD, ".dta"))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 2. LOAD DATA AND FILTER TO VALID WORKERS
# ==============================================================================

df      <- read_dta(input_data)
summary(df)
workers <- read_dta(input_workers)

message("Loaded data: ", nrow(df), " rows.")
message("Valid workers file: ", nrow(workers), " workers.")

# The valid worker list comes from the AKM worker FE output -- we only keep
# workers who were part of a connected component and had their FE estimated.
# We use only the worker ID column from that file; the FE values themselves
# are not used here.
valid_ids <- unique(workers[[COL_WORKER]])
df        <- df %>% filter(.data[[COL_WORKER]] %in% valid_ids)

message("After filtering to valid workers: ", nrow(df), " rows, ",
        n_distinct(df[[COL_WORKER]]), " workers.")


# ==============================================================================
# 3. DEFINE EMPLOYMENT SPELLS AND MOVEDATE
# ==============================================================================
# A spell is a consecutive sequence of days at the same plant.
# We detect spell boundaries by checking whether the plant changed compared
# to the previous day for the same worker (when sorted by date).
#
# MODE SELECTION:
#   SPELL_MODE = "single"  → original behaviour: one movedate per worker
#                            (start of longest spell; non-movers get NA)
#   SPELL_MODE = "multi"   → new behaviour: all spells >= MIN_SPELL_LENGTH
#                            are selected per worker; each generates a movedate
#                            and a symmetric window of observations
#
# For "multi" mode the analyst should first run the diagnostics block below
# (which prints the spell-duration distribution and coverage statistics),
# decide on MIN_SPELL_LENGTH, then let the rest of the section run.
# ------------------------------------------------------------------------------

SPELL_MODE       <- "multi"   # "single" | "multi"
WINDOW_MODE <- "on"  # "on" | "off"
MIN_SPELL_LENGTH <- 2L        # [ANALYST DECISION – multi mode only]
# minimum spell duration (days) to be selected
# Set after inspecting the diagnostics below.

# ------------------------------------------------------------------------------
# 3.0  Parse dates and compute spells
# ------------------------------------------------------------------------------
df[[COL_DATE]] <- as.Date(df$day, origin = "1960-01-01")

df <- df %>%
        arrange(.data[[COL_WORKER]], .data[[COL_DATE]]) %>%
        group_by(.data[[COL_WORKER]]) %>%
        mutate(
                spell_start = (.data[[COL_PLANT]] != lag(.data[[COL_PLANT]], default = NA)) |
                        is.na(lag(.data[[COL_PLANT]])),
                spell_id    = cumsum(spell_start)
        ) %>%
        ungroup()

summary(df[[COL_DATE]])

# Compute duration (in days) of each spell
spell_summary <- df %>%
        group_by(.data[[COL_WORKER]], spell_id) %>%
        summarise(
                spell_plant    = first(.data[[COL_PLANT]]),
                spell_start_dt = min(.data[[COL_DATE]]),
                spell_end_dt   = max(.data[[COL_DATE]]),
                spell_duration = as.integer(spell_end_dt - spell_start_dt) + 1L,
                .groups = "drop"
        )

n_spells_per_worker <- spell_summary %>%
        group_by(.data[[COL_WORKER]]) %>%
        summarise(n_spells = n(), .groups = "drop")

# ==============================================================================
# 3.1 DEFINE ORIGIN / DESTINATION PLANTS PER (WORKER, SPELL)
# ==============================================================================

move_plants <- spell_summary %>%
        arrange(.data[[COL_WORKER]], spell_start_dt) %>%
        group_by(.data[[COL_WORKER]]) %>%
        mutate(
                dest_plant = spell_plant,
                dep_plant  = lag(spell_plant)          # plant of the spell just before
        ) %>%
        ungroup() %>%
        # Each row here is one (worker, spell); rename start date to movedate
        # so it matches the key used downstream.
        rename(movedate = spell_start_dt) %>%
        select(all_of(COL_WORKER), movedate, dest_plant, dep_plant) %>%
        # First spell per worker has no predecessor → dep_plant = NA (already NA
        # from lag); keep it so non-movers are represented but both cols are NA.
        filter(!is.na(dep_plant))                  # drop first-spell rows (no move)

message("\nOrigin/destination plant summary:")
message("  (worker, movedate) pairs with dest & dep plant : ", nrow(move_plants))
message("  Workers covered                                : ",
        n_distinct(move_plants[[COL_WORKER]]))

summary(move_plants)

# ==============================================================================
# 3.2 IDENTIFY MOVERS / NON-MOVERS
# ==============================================================================

all_movers <- n_spells_per_worker %>%
        filter(n_spells > 1) %>%
        pull(.data[[COL_WORKER]])

non_movers <- n_spells_per_worker %>%
        filter(n_spells == 1) %>%
        pull(.data[[COL_WORKER]])

non_mover_obs <- df %>%
        filter(.data[[COL_WORKER]] %in% non_movers)

message("\nNon-movers identified: ", length(non_movers),
        " workers with ", nrow(non_mover_obs), " observations total")

# ==============================================================================
if (SPELL_MODE == "single") {
        # ==============================================================================
        # ------------------------------------------------------------------------------
        # 3.A  SINGLE mode – original behaviour
        # ------------------------------------------------------------------------------
        longest_spell <- spell_summary %>%
                group_by(.data[[COL_WORKER]]) %>%
                arrange(desc(spell_duration), spell_start_dt) %>%
                slice(1) %>%
                ungroup() %>%
                select(-n_spells) %>%   # n_spells will be joined below
                left_join(n_spells_per_worker, by = COL_WORKER) %>%
                mutate(
                        treated = if_else(n_spells > 1, 1L, 0L),
                        movedate = if_else(treated == 1L,
                                           spell_start_dt,
                                           as.Date(NA)),
                        movedate_id = if_else(
                                treated == 1L,
                                row_number(),
                                NA_integer_
                        )
                )        
        message("\nMobility summary (single mode):")
        message("  Total workers : ", n_distinct(df[[COL_WORKER]]))
        message("  Movers        : ", sum(longest_spell$n_spells > 1))
        message("  Non-movers    : ", sum(longest_spell$n_spells == 1))
        
        # Merge movedate back onto the main dataset
        df <- df %>%
                left_join(
                        longest_spell %>%
                                select(
                                        all_of(COL_WORKER),
                                        movedate,
                                        movedate_id,
                                        treated
                                ),
                        by = COL_WORKER
                )
        
        # ==============================================================================
} else if (SPELL_MODE == "multi") {
        # ==============================================================================
        
        # ============================================================================
        # 3.B.1  DIAGNOSTICS – spell-duration distribution
        #        Run this, inspect the output, then set MIN_SPELL_LENGTH above.
        # ============================================================================
        
        message("\n── Spell duration diagnostics ──────────────────────────────────────────")
        
        # Overall distribution
        dur_vec <- spell_summary$spell_duration
        message("\nSpell duration summary (days):")
        print(summary(dur_vec))
        
        quants <- quantile(dur_vec, probs = c(.25, .50, .75, .90, .95, .99))
        message("\nPercentiles:")
        print(quants)
        
        # Distribution table (binned) – useful for picking a threshold
        breaks <- c(1, 2, 3, 5, 7, 10, 14, 21, 30, 60, 90, Inf)
        spell_summary <- spell_summary %>%
                mutate(dur_bin = cut(spell_duration,
                                     breaks = breaks,
                                     right  = FALSE,
                                     labels = paste0("[", head(breaks, -1), ",",
                                                     ifelse(is.finite(tail(breaks, -1)),
                                                            tail(breaks, -1), "∞"), ")")))
        bin_table <- spell_summary %>%
                count(dur_bin) %>%
                mutate(pct = round(100 * n / sum(n), 1))
        
        message("\nSpell-duration frequency table:")
        print(as.data.frame(bin_table), row.names = FALSE)
        
        # For a *grid* of candidate thresholds, report coverage statistics
        thresholds <- c(2L, 3L, 5L, 7L, 10L, 14L, 21L, 30L)
        message("\nCoverage by candidate MIN_SPELL_LENGTH:")
        message(sprintf("  %-12s  %-30s  %-30s",
                        "Threshold", "% spells >= threshold",
                        "% workers with >= 1 such spell"))
        for (thr in thresholds) {
                pct_spells   <- round(100 * mean(dur_vec >= thr), 1)
                workers_with <- spell_summary %>%
                        group_by(.data[[COL_WORKER]]) %>%
                        summarise(has_long = any(spell_duration >= thr), .groups = "drop") %>%
                        pull(has_long) %>% mean() %>% `*`(100) %>% round(1)
                message(sprintf("  %-12d  %-30s  %-30s",
                                thr,
                                paste0(pct_spells, "%"),
                                paste0(workers_with, "%")))
        }
        message("\n── End diagnostics – set MIN_SPELL_LENGTH above, then re-run ───────────")
        
        # ============================================================================
        # 3.B.2  Select all qualifying spells per worker
        #        (using MIN_SPELL_LENGTH set at the top of section 3)
        # ============================================================================
        
        qualifying_spells <- spell_summary %>%
                filter(spell_duration >= MIN_SPELL_LENGTH) %>%
                # For workers with multiple qualifying spells keep them all;
                # they are already ordered by spell_start_dt within each worker.
                arrange(.data[[COL_WORKER]], spell_start_dt)
        
        # Drop first spell for every worker
        # (only keep spells corresponding to actual moves)
        qualifying_spells <- qualifying_spells %>%
                group_by(.data[[COL_WORKER]]) %>%
                filter(row_number() > 1) %>%
                ungroup()
        
        
        n_workers_any <- n_distinct(qualifying_spells[[COL_WORKER]])
        
        message("\nMulti-spell selection (MIN_SPELL_LENGTH = ", MIN_SPELL_LENGTH, " days):")
        message("  Workers with >= 1 qualifying spell : ", n_workers_any,
                " / ", n_distinct(df[[COL_WORKER]]))
        message("  Total qualifying spells after dropping non-movers  : ", nrow(qualifying_spells))
        
        # Workers who are movers overall but have no qualifying move-spells
        movers_no_qualifying_spells <- setdiff(
                all_movers,
                unique(qualifying_spells[[COL_WORKER]])
        )
        
        message("  Movers with NO qualifying move-spells   : ",
                length(movers_no_qualifying_spells))
        
        # ==============================================================================
        # 3.B.2  ASSIGN MOVEDATE IDs
        # ==============================================================================
        # Each unique movedate gets an ID. If two workers share the same calendar
        # movedate they get different IDs because the event is worker-specific.
        movedate_ids <- qualifying_spells %>%
                select(all_of(COL_WORKER), movedate = spell_start_dt) %>%
                arrange(.data[[COL_WORKER]], movedate) %>%
                mutate(movedate_id = row_number())
        
        message("Total (worker, movedate) events: ", nrow(movedate_ids))
        
        # ============================================================================
        # 3.B.3  Diagnostics – dispersion of selected movedates within workers
        # ============================================================================
        
        # For each worker with > 1 qualifying spell, compute gaps between consecutive
        # spell-start dates (movedates)
        gaps <- qualifying_spells %>%
                group_by(.data[[COL_WORKER]]) %>%
                filter(n() > 1) %>%
                arrange(spell_start_dt) %>%
                mutate(gap_to_prev = as.integer(spell_start_dt - lag(spell_start_dt))) %>%
                filter(!is.na(gap_to_prev)) %>%
                ungroup()
        
        if (nrow(gaps) > 0) {
                min_gap<-min(gaps$gap_to_prev)
                message("\nGap between consecutive selected movedates (days), across all workers:")
                print(min_gap)
                message("  Minimum gap observed : ", min(gaps$gap_to_prev), " days")
                message("  Median gap observed  : ", median(gaps$gap_to_prev), " days")
        } else {
                message("\nNo worker has more than one qualifying spell – gap diagnostics skipped.")
        }
        
        # ============================================================================
        # 3.B.4  Define window half-width and check for overlap
        # ============================================================================
        
        # Default window: MIN_SPELL_LENGTH / 2 (rounded down) on each side,
        # so total window width < MIN_SPELL_LENGTH → windows cannot overlap when
        # spells are >= MIN_SPELL_LENGTH apart.
        WINDOW_HALF <- 3 # floor(min_gap / 2)
        
        message("\nWindow half-width : ", WINDOW_HALF, " days  (total window = ",
                2 * WINDOW_HALF + 1, " days, centred on movedate)")
        
        if (nrow(gaps) > 0 && min(gaps$gap_to_prev) < 2 * WINDOW_HALF + 1) {
                warning("Some consecutive movedates are fewer than ", 2 * WINDOW_HALF + 1,
                        " days apart – windows will overlap for those pairs. ",
                        "Consider increasing MIN_SPELL_LENGTH or reducing WINDOW_HALF manually.")
        } else {
                message("  Windows are guaranteed non-overlapping for all workers.")
        }
        
        # ============================================================================
        # 3.B.5  Assign movedates and subset observations within windows
        # ============================================================================
        
        # Build a lookup: worker × movedate → window [lo, hi]
        movedate_lookup <- qualifying_spells %>%
                select(all_of(COL_WORKER), movedate = spell_start_dt) %>%
                mutate(
                        win_lo = movedate - WINDOW_HALF,
                        win_hi = movedate + WINDOW_HALF
                )
        
        summary(movedate_lookup)
        summary(df)
        
        # ==============================================================================
        # SAVE ORIGINAL FULL PANEL
        # Needed later for control sampling
        # ==============================================================================
        df_full <- df
        
        # Cross-join each observation with the movedates for its worker
        # One sub-panel per movedate with size window/half*2, centered around movedate
        df <- df %>%
                left_join(movedate_lookup, by = COL_WORKER,
                          relationship = "many-to-many") %>%
                filter(!is.na(movedate)) %>%
                { if (is.finite(WINDOW_HALF)) {
                        filter(., .data[[COL_DATE]] >= win_lo,
                               .data[[COL_DATE]] <= win_hi)
                } else . } %>%
                select(-win_lo, -win_hi) %>%
                arrange(.data[[COL_WORKER]], movedate, .data[[COL_DATE]])
            
 
        
        # ── attach origin/destination plants (runs for both window and random) ──
        df <- df %>%
                left_join(move_plants, by = c(COL_WORKER, "movedate"))
        summary(df)
        
        df <- df %>%
                mutate(treated = 1L)
        
        message("  dep_plant / dest_plant joined: ",
                sum(is.na(!df$dest_plant)), " rows with non-NA dest_plant")
        
        # ==============================================================================
        # 3.B.6  ATTACH MOVEDATE ID TO MOVER PANEL
        # ==============================================================================
        df <- df %>%
                left_join(movedate_ids, by = c(COL_WORKER, "movedate"))
        
        message("\nPost-windowing dataset:")
        message("  Rows in multi-panel : ", nrow(df))
        message("  Workers treated : ", n_distinct(df[[COL_WORKER]]))
        message("  Unique movedates assigned : ", n_distinct(df$movedate_id))
        
        # ==============================================================================
        # 3.B.7  DEFINE CONTROL WORKER POOL
        # ==============================================================================
        
        # Default:
        # Controls = non-movers + movers with no qualifying move-spells
        
        control_workers <- union(
                non_movers,
                movers_no_qualifying_spells
        )
        
        # Alternative:
        # control_workers <- non_movers
        
        message("\nControl worker pool:")
        message("  Eligible control workers : ", length(control_workers))
        
        # ==============================================================================
        # 3.B.8  SAMPLE CONTROLS FOR EACH MOVEDATE_ID
        # ==============================================================================
        
        CONTROL_SHARE <- 0.10
        
        set.seed(12345)
        
        # One row per movedate_id
        event_table <- movedate_ids %>%
                distinct(movedate_id, movedate)
        
        # For each event, randomly sample controls
        control_assignments <- event_table %>%
                rowwise() %>%
                mutate(
                        sampled_controls = list(
                                sample(
                                        control_workers,
                                        size = ceiling(CONTROL_SHARE * length(control_workers)),
                                        replace = FALSE
                                )
                        )
                ) %>%
                tidyr::unnest(sampled_controls) %>%
                rename(
                        !!COL_WORKER := sampled_controls
                ) %>%
                ungroup()
        
        message("  Total sampled (event, control worker) pairs : ",
                nrow(control_assignments))
        
        # ==============================================================================
        # 3.B.9  BUILD STACKED CONTROL PANEL
        # ==============================================================================
        
        control_panel <- df_full %>%
                inner_join(
                        control_assignments,
                        by = COL_WORKER,
                        relationship = "many-to-many"
                ) %>%
                mutate(
                        win_lo = movedate - WINDOW_HALF,
                        win_hi = movedate + WINDOW_HALF
                ) %>%
                filter(
                        .data[[COL_DATE]] >= win_lo,
                        .data[[COL_DATE]] <= win_hi
                ) %>%
                select(-win_lo, -win_hi) %>%
                arrange(movedate_id, .data[[COL_WORKER]], .data[[COL_DATE]])
        
        control_panel <- control_panel %>%
                mutate(
                        movedate = as.Date(NA),
                        treated  = 0L
                )
        
        message("\nConstructed stacked control panel:")
        message("  Observations : ", nrow(control_panel))
        message("  Control workers appearing : ",
                n_distinct(control_panel[[COL_WORKER]]))
        
        
        summary(control_panel)
        
        df <- bind_rows(
                df,
                control_panel
        )
        
} else {
        stop("SPELL_MODE must be 'single' or 'multi'. Got: '", SPELL_MODE, "'")
}


# ==============================================================================
# 4. CREATE POST INDICATOR
# ==============================================================================

# post = 1 for all observations on or after movedate, 0 before.
# Non-movers (movedate is NA) get post = 0 everywhere.
df <- df %>%
        mutate(post = as.integer(!is.na(movedate) & .data[[COL_DATE]] >= movedate))

message("\nPost indicator: ", sum(df$post), " worker-day obs flagged as post-move.")

# ==============================================================================
# 5. AGGREGATE FROM WORKER-DAY TO WORKER-PLANT-PERIOD
# ==============================================================================

# Create the period variable by flooring the date to the chosen frequency.
# floor_date() from lubridate handles week and month boundaries correctly.
df <- df %>%
        mutate(period = floor_date(.data[[COL_DATE]], unit = PERIOD))
message("\nPeriod variable created: ", nrow(df), " rows.")


# --- Helper: statistical mode ------------------------------------------------
# R has no built-in mode function. This returns the most frequent value,
# breaking ties by taking the first one in sorted order.
get_mode <- function(x) {
        x   <- x[!is.na(x)]
        if (length(x) == 0) return(NA)
        tbl <- sort(table(x), decreasing = TRUE)
        names(tbl)[1]
}

# --- Shift start time to decimal hours ---------------------------------------
# shift_start_time may be stored as "HH:MM" strings or a numeric.
# We convert to decimal hours (e.g. "08:30" -> 8.5) before taking the mode.
to_decimal_hours <- function(x) {
        if (is.numeric(x)) return(x)
        
        x <- gsub("[^0-9:]", "", as.character(x))
        parts <- strsplit(x, ":", fixed = TRUE)
        
        sapply(parts, function(p) {
                if (length(p) < 2) return(NA_real_)
                as.numeric(p[1]) + as.numeric(p[2]) / 60
        })
}

df <- df %>%
        mutate(shift_start_decimal = to_decimal_hours(.data[[COL_SHIFT_START_TIME]]))

print(
        head(
                df %>%
                        select(-starts_with("log_")),
                n = 10
        ),
        width = Inf
)

# --- Aggregate ---------------------------------------------------------------
# One row per (worker, plant, period).
# Each variable type has its own aggregation rule as described in the design.

collapsed <- df %>%
        group_by(.data[[COL_WORKER]],
                 .data[[COL_PLANT]], 
                 period,
                 movedate_id,
                 treated,
                 movedate) %>%
        summarise(
                # Log outcome variables: mean within period
                log_rev_per_hour = mean(log_rev_per_hour, na.rm = TRUE),
                log_rev          = mean(log_rev,          na.rm = TRUE),
                log_quant        = mean(log_quant,        na.rm = TRUE),
                log_tottime      = mean(log_tottime,      na.rm = TRUE),
                log_avgtime      = mean(log_avgtime,      na.rm = TRUE),
                log_value        = mean(log_value,        na.rm = TRUE),
                
                # Overtime: sum (total overtime within period) and mean (avg per day)
                overtime_total   = sum(overtime,          na.rm = TRUE),
                overtime_mean    = mean(overtime,         na.rm = TRUE),
                
                # Turnover: max within period (1 if the worker's last day falls in this period)
                turnover         = max(turnover,          na.rm = TRUE),
                
                # Absence: indicator (any absence in period) and total days absent
                absent_any       = max(.data[[COL_ABSENT_IND]],  na.rm = TRUE),
                absent_days      = sum(.data[[COL_ABSENT_IND]],  na.rm = TRUE),

                # Shift timing: mode of actual start hour; mode of scheduled start (decimal)
                start_hour_mode        = as.numeric(get_mode(.data[[COL_START_HOUR]])),
                shift_start_mode       = get_mode(shift_start_decimal),
                
                # Post indicator: max within period (1 if any day in the period is post-move)
                post             = max(post, na.rm = TRUE),
                
                # Carry through worker-level move information
                d1_moved_to   = first(dest_plant),
                d1_moved_from = first(dep_plant),
                movedate        = first(movedate),
                
                # Keep date of first day in the period for reference
                date_first       = min(.data[[COL_DATE]]),
                
                .groups = "drop"
        ) %>%
        mutate(shift_start_mode = as.numeric(shift_start_mode))

message(
        "\nCollapsed dataset created with variables:\n  ",
        paste(names(collapsed), collapse = ", ")
)

# on_time: 1 if actual start hour matches scheduled start (after aggregation)
collapsed <- collapsed %>%
        mutate(on_time = as.integer(
                !is.na(start_hour_mode) & !is.na(shift_start_mode) &
                        round(start_hour_mode, 2) == round(shift_start_mode, 2)
        ))

# changed_start_hour: 1 if start_hour_mode differs from the previous period
# for the same worker-plant pair, 0 otherwise. Missing for the first period.
collapsed <- collapsed %>%
        arrange(.data[[COL_WORKER]], period, .data[[COL_PLANT]]) %>%
        group_by(.data[[COL_WORKER]], .data[[COL_PLANT]]) %>%
        mutate(
                changed_start_hour = as.integer(
                        !is.na(start_hour_mode) &
                                !is.na(lag(start_hour_mode)) &
                                start_hour_mode != lag(start_hour_mode)
                ),
                # First period within each worker-plant pair gets NA (no previous period)
                changed_start_hour = if_else(is.na(lag(start_hour_mode)), NA_integer_, changed_start_hour)
        ) %>%
        ungroup()

message("\nCollapsed to worker-plant-period: ", nrow(collapsed), " rows.")


# ==============================================================================
# 6. COMPUTE DIST (DISTANCE IN PERIODS TO MOVEDATE)
# ==============================================================================

# dist measures how many periods away the current observation is from the
# move date. Negative values are pre-move, positive are post-move.
# For non-movers (movedate is NA), dist is set to missing.

# We find the period that contains movedate for each worker, then subtract.
# floor_date applied to movedate gives the period bin it falls into.

collapsed <- collapsed %>%
        mutate(
                movedate_period = floor_date(movedate, unit = PERIOD),
                # Number of periods between current period and move period.
                # difftime gives days; divide by period length to get period count.
                dist = case_when(
                        is.na(movedate) ~ NA_real_,
                        PERIOD == "week"  ~ as.numeric(difftime(period, movedate_period, units = "weeks")),
                        PERIOD == "month" ~ as.numeric(
                                (as.integer(format(period, "%Y")) - as.integer(format(movedate_period, "%Y"))) * 12 +
                                        (as.integer(format(period, "%m")) - as.integer(format(movedate_period, "%m")))
                        )
                )
        ) %>%
        select(-movedate_period)  # drop helper column

message("dist range (movers only): ",
        round(min(collapsed$dist, na.rm = TRUE), 1), " to ",
        round(max(collapsed$dist, na.rm = TRUE), 1), " periods.")

collapsed <- collapsed %>%
        arrange(movedate_id, treated, .data[[COL_WORKER]], period, .data[[COL_PLANT]])

# ==============================================================================
# 7. SAVE
# ==============================================================================
print(
        head(
                collapsed %>%
                        select(-starts_with("log_")),
                n = 10
        ),
        width = Inf
)
write_dta(collapsed, output_path)
message("\nCollapsed dataset saved to: ", output_path)
message("Final dataset: ", nrow(collapsed), " rows, ", ncol(collapsed), " columns.")
