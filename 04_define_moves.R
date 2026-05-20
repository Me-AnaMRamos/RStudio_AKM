# ==============================================================================
# Script: 04_merge.R
#
# Takes the collapsed worker-plant-period dataset from Script 03_collapsed and the AKM
# plant FE files from akm.R. For each AKM outcome it defines the destination
# plant (moveto_plant) and an indicator for whether the worker moved to a
# high-FE plant (move_to_high_fe), then saves the enriched dataset.
#
# Two mobility definitions are supported (set MOVE_DEF below):
#   Definition 1 — moveto_plant is the plant observed on movedate. Identical
#                  across all outcomes.
#   Definition 2 — moveto_plant is the plant with the highest FE value for
#                  that outcome's FE measure, within a window of +/- WINDOW_PERIODS
#                  around movedate. Can differ across outcomes.
#
# Input:  Data/Analysis_data/processed/collapsed_[PERIOD].dta   (Script 1a)
#         Outputs/AKM/plant_fe_[outcome]_[CONTROLS_AKM].dta   (akm.R, one per outcome)
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
# 1 = destination is the plant at movedate (same across all outcomes)
# 2 = destination is the highest-FE plant within WINDOW_PERIODS of movedate
MOVE_DEF <- 1

# Window half-width for Definition 2 (ignored when MOVE_DEF == 1).
# For PERIOD_COLLAPSED = "week"  a value of 4 means +/- 4 weeks around movedate.
# For PERIOD_COLLAPSED = "month" a value of 2 means +/- 2 months around movedate.
WINDOW_PERIODS <- 4

# --- AKM controls label ------------------------------------------------------
# Must match the CONTROLS_AKM used in akm.R so that the correct FE files
# are found (e.g. "week", "no_controls", "age_tenure").
CONTROLS_AKM <- "week"

# --- Outcomes ----------------------------------------------------------------
# Must match the outcomes vector in akm.R.
OUTCOMES <- c("log_rev_per_hour", "log_rev", "log_quant",
              "log_tottime",      "log_avgtime", "log_value")

# --- Column names ------------------------------------------------------------
# These must match the column names produced by collapse.R and akm.R.
COL_WORKER   <- "id_employee"
COL_PLANT    <- "plant"
COL_MOVEDATE <- "movedate"
COL_PERIOD   <- "period"

# Name of the plant FE column inside the plant FE files (from akm.R).
COL_FE_PLANT_STD <- "fe_plant_std"

# --- Paths -------------------------------------------------------------------
input_collapsed <- file.path(script_dir, "Outputs", "Collapsed",
                             paste0("collapsed_", PERIOD_COLLAPSED, ".dta"))
akm_dir         <- file.path(script_dir, "Outputs", "AKM")
output_dir      <- file.path(script_dir, "Outputs", "Merged")
output_path <- file.path(
        output_dir,
        paste0(
                "panel_data",
                "_level-",PERIOD_COLLAPSED,
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
# 4. DEFINE DEPARTURE AND DESTINATION PLANT IF NOT YET (IF DEF TWO)
# ==============================================================================

# Load combined plant FE file once
fe_path <- file.path(
        akm_dir,
        paste0("plant_fe_all_", CONTROLS_AKM, ".dta")
)

if (!file.exists(fe_path)) {
        stop("Combined plant FE file not found:\n", fe_path)
}

col_moveto <- paste0("move_to_plant")
col_movefrom <- paste0("move_from_plant")

if(MOVE_DEF == 1){
                df[[col_moveto]]   <- df[["d1_move_to_plant"]]
                df[[col_movefrom]] <- df[["d1_move_from_plant"]]
} else if (MOVE_DEF == 2) {                 # Definition 1 already computed in collapsed data
                
                # Definition 2: highest-FE plant within WINDOW_PERIODS of movedate,
                # using this outcome's plant FE values.
                #
                # For each mover we:
                #   (i)  restrict to periods within the window around movedate_period,
                #   (ii) join the plant FE values for each observed plant in that window,
                #   (iii) pick the plant with the maximum FE.
                
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
                        # Attach FE values for the plant observed in each row of the window
                        left_join(plant_fe %>% select(all_of(COL_PLANT), all_of(COL_FE_PLANT_STD)),
                                  by = COL_PLANT) %>%
                        group_by(.data[[COL_WORKER]]) %>%
                        # Pick the plant with the highest FE within the window.
                        # If multiple rows tie, slice_max keeps one deterministically.
                        slice_max(order_by = .data[[COL_FE_PLANT_STD]], n = 1, with_ties = FALSE) %>%
                        ungroup() %>%
                        select(all_of(COL_WORKER), !!col_moveto := all_of(COL_PLANT))
                
                movefrom_def2 <- df %>%
                        filter(!is.na(.data[[COL_MOVEDATE]])) %>%
                        filter(
                                .data[[COL_PERIOD]] >= (movedate_period - period_width) &
                                        .data[[COL_PERIOD]] <= (movedate_period + period_width)
                        ) %>%
                        left_join(plant_fe %>% select(all_of(COL_PLANT), all_of(COL_FE_PLANT_STD)),
                                  by = COL_PLANT) %>%
                        group_by(.data[[COL_WORKER]]) %>%
                        slice_min(order_by = .data[[COL_FE_PLANT_STD]], n = 1, with_ties = FALSE) %>%
                        ungroup() %>%
                        select(all_of(COL_WORKER), !!col_movefrom := all_of(COL_PLANT))
                
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
                
                # Attach FE-based departure plant
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
# 4. LOOP OVER OUTCOMES: MERGE PLANT FE AND CREATE MOVE_TO_HIGH_FE
# ==============================================================================

for (outcome in OUTCOMES) {
        message("\n--- Outcome: ", outcome, " ---")
        
        FE_COL_STD <- paste0("std_", make.names(outcome))
        
        #  --- 5a. Extract only id + current outcome FE
        
        plant_fe <- plant_fe_all %>%
                select(all_of(COL_PLANT),
                       all_of(FE_COL_STD)) %>%
                rename(
                        fe_plant_std = all_of(FE_COL_STD)
                )
        
        # Ensure the plant ID column has the same type as in df (igraph coerces to
        # character; akm.R restores numeric if the original was numeric).
        if (is.numeric(df[[COL_PLANT]]) && !is.numeric(plant_fe[[COL_PLANT]])) {
                plant_fe[[COL_PLANT]] <- as.numeric(plant_fe[[COL_PLANT]])
        }
        
        message("  Plant FE file loaded: ", nrow(plant_fe), " plants.")
        
        # --- 5b. Resolve destination plant ----------------------------------------
        
        message("Outcome: ", outcome)
        
        # --- 5c. Merge plant FE for destination and departure plants -------------
        # We want the FE of the *destination* plant, not the plant in each row.
        # Rename the plant ID in the FE file to col_moveto so the join matches on
        # the destination plant column.
        
        col_fe_dest_std <- paste0("FEplant_des_", outcome)
        col_fe_dep_std  <- paste0("FEplant_dep_", outcome)
        
        # Destination plant FE
        plant_fe_renamed <- plant_fe %>%
                select(
                        !!col_moveto    := all_of(COL_PLANT),
                        !!col_fe_dest_std := all_of(COL_FE_PLANT_STD)
                )
        names(df)
        df <- df %>%
                inner_join(plant_fe_renamed, by = col_moveto)
        
        message("  Merged destination plant FE (", col_fe_dest_std, ").")
        
        # Departure plant FE (plant observed in each row, i.e. the origin plant)
        plant_fe_dep <- plant_fe %>%
                select(
                        !!col_movefrom    := all_of(COL_PLANT),
                        !!col_fe_dep_std := all_of(COL_FE_PLANT_STD)
                )
        
        df <- df %>%
                left_join(plant_fe_dep, by = col_movefrom)
        
        message("  Merged departure plant FE (", col_fe_dep_std, ").")
        
        # --- 5d. Create high-FE indicators for destination and departure ----------
        # 1 if destination plant FE is above the median of the full plant FE
        # distribution for this outcome (using the standardised FE for a
        # scale-invariant threshold). Missing for non-movers.
        
        fe_median <- median(plant_fe[[COL_FE_PLANT_STD]], na.rm = TRUE)
        
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

message("\nFinal dataset: ", nrow(df), " rows, ", ncol(df), " columns.")
message("Saved to: ", output_path)