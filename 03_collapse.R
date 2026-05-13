# ==============================================================================
# Script 1a: collapse.R
#
# Takes the processed worker-day panel, filters to valid workers, defines
# worker mobility (spells, movedate, post), aggregates to worker-plant-period,
# and computes dist. (No regression and no plant FE merging)
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
COL_PLANT   <- "plant"
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
# Each new plant = new spell. We then find the longest spell per worker
# and define movedate as its start date.

df[[COL_DATE]] <- as.Date(df$day, origin = "1960-01-01")
df <- df %>%
        arrange(.data[[COL_WORKER]], .data[[COL_DATE]]) %>%
        group_by(.data[[COL_WORKER]]) %>%
        mutate(
                # A new spell starts whenever the plant changes from the previous row.
                # The very first row for each worker always starts a new spell.
                spell_start = (.data[[COL_PLANT]] != lag(.data[[COL_PLANT]], default = NA)) |
                        is.na(lag(.data[[COL_PLANT]])),
                # Give each spell a sequential ID within the worker
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

# For each worker, find the longest spell.
# If there is only one spell, the worker is a non-mover.
# Ties in spell duration are broken by taking the first one (earliest start).
longest_spell <- spell_summary %>%
        group_by(.data[[COL_WORKER]]) %>%
        arrange(desc(spell_duration), spell_start_dt) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(n_spells = NA_integer_)  # will fill below

# Count total spells per worker to identify non-movers (only 1 spell)
n_spells_per_worker <- spell_summary %>%
        group_by(.data[[COL_WORKER]]) %>%
        summarise(n_spells = n(), .groups = "drop")

longest_spell <- longest_spell %>%
        select(-n_spells) %>%
        left_join(n_spells_per_worker, by = COL_WORKER)

# movedate: start date of the longest spell.
# Non-movers (n_spells == 1) get movedate = NA.
longest_spell <- longest_spell %>%
        mutate(movedate = if_else(n_spells > 1, spell_start_dt, as.Date(NA)))

message("\nMobility summary:")
message("  Total workers:   ", n_distinct(df[[COL_WORKER]]))
message("  Movers:          ", sum(longest_spell$n_spells > 1))
message("  Non-movers:      ", sum(longest_spell$n_spells == 1))

# Merge movedate back onto the main dataset
df <- df %>%
        left_join(longest_spell %>% select(all_of(COL_WORKER), movedate), by = COL_WORKER)


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

# --- Aggregate ---------------------------------------------------------------
# One row per (worker, plant, period).
# Each variable type has its own aggregation rule as described in the design.


collapsed <- df %>%
        group_by(.data[[COL_WORKER]], .data[[COL_PLANT]], period) %>%
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
                
                # Carry through movedate (constant within worker)
                movedate         = first(movedate),
                
                # Keep date of first day in the period for reference
                date_first       = min(.data[[COL_DATE]]),
                
                .groups = "drop"
        ) %>%
        mutate(shift_start_mode = as.numeric(shift_start_mode))

# on_time: 1 if actual start hour matches scheduled start (after aggregation)
collapsed <- collapsed %>%
        mutate(on_time = as.integer(
                !is.na(start_hour_mode) & !is.na(shift_start_mode) &
                        round(start_hour_mode, 2) == round(shift_start_mode, 2)
        ))

# changed_start_hour: 1 if start_hour_mode differs from the previous period
# for the same worker-plant pair, 0 otherwise. Missing for the first period.
collapsed <- collapsed %>%
        arrange(.data[[COL_WORKER]], .data[[COL_PLANT]], period) %>%
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


# ==============================================================================
# 7. SAVE
# ==============================================================================
head(collapsed)
write_dta(collapsed, output_path)
message("\nCollapsed dataset saved to: ", output_path)
message("Final dataset: ", nrow(collapsed), " rows, ", ncol(collapsed), " columns.")