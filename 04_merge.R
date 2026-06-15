# ==============================================================================
# Script: 04_merge.R
#
# Takes the collapsed worker-unit-period dataset from Script 03_collapsed and the AKM
# unit FE files from akm.R. For each AKM outcome it defines the destination
# unit (moveto_unit) and an indicator for whether the worker moved to a
# high-FE unit (move_to_high_fe), then saves the enriched dataset.
#
# Two mobility definitions are supported (set MOVE_DEF below):
#   Definition 1 — moveto_unit is the unit observed on movedate. Identical
#                  across all outcomes.
#   Definition 2 — moveto_unit is the unit with the highest FE value for
#                  that outcome's FE measure, within a window of +/- WINDOW_PERIODS
#                  around movedate. Can differ across outcomes.
#
# Input:  Data/Analysis_data/processed/collapsed_[PERIOD].dta   (Script 1a)
#         Outputs/AKM/unit_fe_LEV_[outcome]_[CONTROLS_AKM].dta   (akm.R, one per outcome)
# Output: Data/Analysis_data/processed/event_study_[PERIOD]_def[MOVE_DEF].dta
# ==============================================================================

library(haven)      # read/write .dta files
library(dplyr)      # data manipulation
library(lubridate)  # floor_date() for period arithmetic


# ==============================================================================
# 1. SETTINGS
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

# --- Period choice -----------------------------------------------------------
# Must match the PERIOD used in collapse.R so that the correct input file is read.
PERIOD_COLLAPSED <- "week"   # "week" or "month"

# --- Mobility definition -----------------------------------------------------
# 1 = destination is the unit at movedate (same across all outcomes)
# 2 = destination is the highest-FE unit within WINDOW_PERIODS of movedate
MOVE_DEF <- 1

# Window half-width for Definition 2 (ignored when MOVE_DEF == 1).
# For PERIOD_COLLAPSED = "week"  a value of 4 means +/- 4 weeks around movedate.
# For PERIOD_COLLAPSED = "month" a value of 2 means +/- 2 months around movedate.
WINDOW_PERIODS <- 4

# --- AKM controls label ------------------------------------------------------
# Must match the CONTROLS_AKM used in akm.R so that the correct FE files
# are found (e.g. "week", "no_controls", "age_tenure").
CONTROLS_AKM <- "week_tenure"

# --- Outcomes ----------------------------------------------------------------
# Must match the outcomes vector in akm.R.
OUTCOMES <- c("log_rev_per_hour", "log_rev", "log_quant",
              "log_tottime",      "log_avgtime", "log_value")

# --- Column names ------------------------------------------------------------
# These must match the column names produced by collapse.R and akm.R.
COL_WORKER   <- "id_employee"
COL_UNIT    <- "camion"
COL_MOVEDATE <- "movedate"
COL_PERIOD   <- "period"

# Name of the unit FE column inside the unit FE files (from akm.R).
COL_FE_UNIT_STD <- "fe_unit_std"

# --- Paths -------------------------------------------------------------------
input_collapsed <- file.path(script_dir, "Outputs", "Collapsed",
                             paste0("collapsed_LEV_", COL_WORKER, "_", COL_UNIT, "_", PERIOD, ".dta"))
akm_dir         <- file.path(script_dir, "Outputs", "AKM")
output_dir      <- file.path(script_dir, "Outputs", "Merged")
output_path <- file.path(
        output_dir,
        paste0(
                "panel_data",
                "_lev_", COL_WORKER, "_", COL_UNIT, "_", PERIOD_COLLAPSED,
                "_akm-", CONTROLS_AKM,
                "_def-", MOVE_DEF,
                ".dta"
        )
)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 2. LOAD COLLAPSED DATASET
# ==============================================================================

df <- read_dta(input_collapsed)
print(head(df), width=Inf)

message("Loaded collapsed dataset: ", nrow(df), " rows, ",
        n_distinct(df[[COL_WORKER]]), " workers.")

# Ensure period and movedate are proper Date objects (haven may read them as
# numeric Stata dates).
if (!inherits(df[[COL_PERIOD]],   "Date")) df[[COL_PERIOD]]   <- as.Date(df[[COL_PERIOD]])
if (!inherits(df[[COL_MOVEDATE]], "Date")) df[[COL_MOVEDATE]] <- as.Date(df[[COL_MOVEDATE]])

# Identify mover workers (those with a non-missing movedate).
movers <- df %>%
        filter(!is.na(.data[[COL_MOVEDATE]])) %>%
        pull(.data[[COL_WORKER]]) %>%
        unique()

message("Movers: ", length(movers), " | Non-movers: ",
        n_distinct(df[[COL_WORKER]]) - length(movers))


# ==============================================================================
# 3. HELPER: COMPUTE MOVEDATE PERIOD
# ==============================================================================

# The period bin that contains movedate -- used by both definitions.
df <- df %>%
        mutate(movedate_period = floor_date(.data[[COL_MOVEDATE]], unit = PERIOD_COLLAPSED))

print(head(df), width=Inf)

# ==============================================================================
# 4. DEFINE DEPARTURE AND DESTINATION UNIT IF NOT YET (IF DEF TWO)
# ==============================================================================

# Load combined unit FE file once
fe_path <- file.path(
        akm_dir,
        paste0("unit_fe_LEV_", COL_WORKER, "_", COL_UNIT, "_CTRL_", CONTROLS_AKM, ".dta")
)

if (!file.exists(fe_path)) {
        stop("Combined unit FE file not found:\n", fe_path)
}

unit_fe_all <- read_dta(fe_path)


col_moveto <- paste0("moved_to")
col_movefrom <- paste0("moved_from")

if (MOVE_DEF == 1){
                df[[col_moveto]]   <- df[["d1_moved_to"]]
                df[[col_movefrom]] <- df[["d1_moved_from"]]
                
                df <- df %>%
                        select(-d1_moved_to, -d1_moved_from)
                
} else if (MOVE_DEF == 2) {                 # Definition 1 already computed in collapsed data
                
                # Definition 2: highest-FE unit within WINDOW_PERIODS of movedate,
                # using this outcome's unit FE values.
                #
                # For each mover we:
                #   (i)  restrict to periods within the window around movedate_period,
                #   (ii) join the unit FE values for each observed unit in that window,
                #   (iii) pick the unit with the maximum FE.
                
                # Compute the window boundaries in calendar units.
                period_width <- switch(PERIOD_COLLAPSED,
                                       week  = weeks(WINDOW_PERIODS),
                                       month = months(WINDOW_PERIODS),
                                       stop("PERIOD must be 'week' or 'month'."))
                
                moveto_def2 <- df %>%
                        filter(!is.na(.data[[COL_MOVEDATE]])) %>%
                        filter(
                                .data[[COL_PERIOD]] >= (movedate_period - period_width) &
                                        .data[[COL_PERIOD]] <= (movedate_period + period_width)
                        ) %>%
                        # Attach FE values for the unit observed in each row of the window
                        left_join(unit_fe %>% select(all_of(COL_UNIT), all_of(COL_FE_UNIT_STD)),
                                  by = COL_UNIT) %>%
                        group_by(.data[[COL_WORKER]]) %>%
                        # Pick the unit with the highest FE within the window.
                        # If multiple rows tie, slice_max keeps one deterministically.
                        slice_max(order_by = .data[[COL_FE_UNIT_STD]], n = 1, with_ties = FALSE) %>%
                        ungroup() %>%
                        select(all_of(COL_WORKER), !!col_moveto := all_of(COL_UNIT))
                
                movefrom_def2 <- df %>%
                        filter(!is.na(.data[[COL_MOVEDATE]])) %>%
                        filter(
                                .data[[COL_PERIOD]] >= (movedate_period - period_width) &
                                        .data[[COL_PERIOD]] <= (movedate_period + period_width)
                        ) %>%
                        left_join(unit_fe %>% select(all_of(COL_UNIT), all_of(COL_FE_UNIT_STD)),
                                  by = COL_UNIT) %>%
                        group_by(.data[[COL_WORKER]]) %>%
                        slice_min(order_by = .data[[COL_FE_UNIT_STD]], n = 1, with_ties = FALSE) %>%
                        ungroup() %>%
                        select(all_of(COL_WORKER), !!col_movefrom := all_of(COL_UNIT))
                
                message("  Definition 2 (window = +/-", WINDOW_PERIODS, " ", PERIOD_COLLAPSED,
                        "s): destination resolved for ", nrow(moveto_def2), " movers.")
                
                df[[col_moveto]] <- NA_real_   # initialise
                df <- df %>%
                        left_join(moveto_def2, by = COL_WORKER)
                
                dup_col <- paste0(col_moveto, ".y")
                if (dup_col %in% colnames(df)) {
                        df[[col_moveto]] <- df[[dup_col]]
                        df <- df %>% select(-all_of(paste0(col_moveto, ".x")),
                                            -all_of(dup_col))
                }
                
                # Attach FE-based departure unit
                df[[col_movefrom]] <- NA_real_
                
                df <- df %>%
                        left_join(movefrom_def2, by = COL_WORKER)
                
                dup_col <- paste0(col_movefrom, ".y")
                
                if (dup_col %in% colnames(df)) {
                        df[[col_movefrom]] <- df[[dup_col]]
                        
                        df <- df %>%
                                select(
                                        -all_of(paste0(col_movefrom, ".x")),
                                        -all_of(dup_col)
                                )
                }
                
        } 
        
        # Non-movers always have NA destination (movedate is NA so col_moveto stays NA).

# ==============================================================================
# 4. LOOP OVER OUTCOMES: MERGE UNIT FE AND CREATE MOVE_TO_HIGH_FE
# ==============================================================================

for (outcome in OUTCOMES) {
        message("\n--- Outcome: ", outcome, " ---")
        
        FE_COL_STD <- paste0("std_", make.names(outcome))
        
        #  --- 5a. Extract only id + current outcome FE
        
        unit_fe <- unit_fe_all %>%
                select(all_of(COL_UNIT),
                       all_of(FE_COL_STD)) %>%
                rename(
                        fe_unit_std = all_of(FE_COL_STD)
                )
        
        # Ensure the unit ID column has the same type as in df (igraph coerces to
        # character; akm.R restores numeric if the original was numeric).
        if (is.numeric(df[[COL_UNIT]]) && !is.numeric(unit_fe[[COL_UNIT]])) {
                unit_fe[[COL_UNIT]] <- as.numeric(unit_fe[[COL_UNIT]])
        }
        
        message("  Unit FE file loaded: ", nrow(unit_fe), " units.")
        
        # --- 5b. Resolve destination unit ----------------------------------------
        
        message("Outcome: ", outcome)
        
        # --- 5c. Merge unit FE for destination and departure units -------------
        # We want the FE of the *destination* unit, not the unit in each row.
        # Rename the unit ID in the FE file to col_moveto so the join matches on
        # the destination unit column.
        
        col_fe_dest_std <- paste0("FE_dest_", outcome)
        col_fe_dep_std  <- paste0("FE_dep_", outcome)
        
        # Destination unit FE
        unit_fe_renamed <- unit_fe %>%
                select(
                        !!col_moveto    := all_of(COL_UNIT),
                        !!col_fe_dest_std := all_of(COL_FE_UNIT_STD)
                )
        names(df)
        df <- df %>%
                inner_join(unit_fe_renamed, by = col_moveto)
        
        message("  Merged destination unit FE (", col_fe_dest_std, ").")
        
        # Departure unit FE (unit observed in each row, i.e. the origin unit)
        unit_fe_dep <- unit_fe %>%
                select(
                        !!col_movefrom    := all_of(COL_UNIT),
                        !!col_fe_dep_std := all_of(COL_FE_UNIT_STD)
                )
        
        df <- df %>%
                left_join(unit_fe_dep, by = col_movefrom)
        
        message("  Merged departure unit FE (", col_fe_dep_std, ").")
        
        # --- 5d. Create high-FE indicators for destination and departure ----------
        # 1 if destination unit FE is above the median of the full unit FE
        # distribution for this outcome (using the standardised FE for a
        # scale-invariant threshold). Missing for non-movers.
        
        fe_median <- median(unit_fe[[COL_FE_UNIT_STD]], na.rm = TRUE)
        
        col_high_fe <- paste0("moveTo_highFE_", outcome)
        col_high_fe_dep  <- paste0("moveFrom_highFE_", outcome)

        
        df <- df %>%
                mutate(
                        !!col_high_fe := case_when(
                        is.na(.data[[col_moveto]])    ~ NA_integer_,   # non-mover
                        is.na(.data[[col_fe_dest_std]]) ~ NA_integer_, # destination FE unavailable
                        .data[[col_fe_dest_std]] > fe_median ~ 1L,
                        TRUE ~ 0L),
                        !!col_high_fe_dep := case_when(
                                is.na(.data[[col_moveto]])     ~ NA_integer_,   # non-mover
                                is.na(.data[[col_fe_dep_std]]) ~ NA_integer_,
                                .data[[col_fe_dep_std]] > fe_median ~ 1L,
                                TRUE ~ 0L
                        )
                )
        
        n_high <- sum(df[[col_high_fe]] == 1L, na.rm = TRUE)
        n_low  <- sum(df[[col_high_fe]] == 0L, na.rm = TRUE)
        n_dep_high  <- sum(df[[col_high_fe_dep]]  == 1L, na.rm = TRUE)
        n_dep_low   <- sum(df[[col_high_fe_dep]]  == 0L, na.rm = TRUE)
        
        message("  move_to_high_fe_", outcome, ": ",
                n_high, " high-FE obs, ", n_low, " low-FE obs ")
        
        message("  move_from_high_fe_", outcome, ": ",
                n_dep_high,  " high-FE obs, ", n_dep_low,  " low-FE obs ",
                "(median FE_std threshold = ", round(fe_median, 4), ").")
}


# ==============================================================================
# 6. DROP HELPER COLUMN AND SAVE
# ==============================================================================

df <- df %>% select(-movedate_period)

write_dta(df, output_path)

print(
        head(
                df %>%
                        filter(!(.data[[COL_WORKER]] %in% c(5, 43, 48))) %>%
                        select(-starts_with("log_")),
                n = 10
        ),
        width = Inf
)
names(df)

repeated <- df %>%
        group_by(movedate_id) %>%
        summarise(n_workers = n_distinct(COL_WORKER), .groups = "drop") %>%
        filter(n_workers > 1)

repeated
message("\nFinal dataset: ", nrow(df), " rows, ", ncol(df), " columns.")
message("Saved to: ", output_path)