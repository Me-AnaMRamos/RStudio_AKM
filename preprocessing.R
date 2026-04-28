# ==============================================================================
# Data Preparation Script
#
# Reads the raw panel dataset, inspects it, constructs outcome and worker-level
# variables, validates the panel structure, and saves the processed dataset.
#
# Input:  Data/Analysis_data/data.dta
# Output: Data/Analysis_data/processed/data_processed.dta
# ==============================================================================

library(haven)   # read/write Stata .dta files
library(dplyr)   # data manipulation


# ==============================================================================
# 1. PATHS AND SETTINGS
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

input_path  <- file.path(script_dir, "Data", "Analysis_data", "data.dta")
output_dir  <- file.path(script_dir, "Data", "Analysis_data", "processed")
output_path <- file.path(output_dir, "data_processed.dta")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Column names expected in the raw data -- change here if your data uses
# different names, everything downstream will update accordingly.
COL_WORKER   <- "id_employee"
COL_PLANT    <- "plant"
COL_MANAGER  <- "id_supervisor"
COL_DATE     <- "date"
COL_VALUE    <- "tkt_value"
COL_TIME     <- "tottime_tkt"
COL_QTY      <- "tkt_qty"
COL_VPQ      <- "value_per_quant"
COL_AVGTIME  <- "avg_time_tkt"
COL_HIRING   <- "employee_date_hiring"

required_cols <- c(COL_WORKER, COL_PLANT, COL_MANAGER, COL_DATE,
                   COL_VALUE, COL_TIME, COL_QTY, COL_VPQ, COL_AVGTIME, COL_HIRING)


# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

df <- read_dta(input_path)
message("Loaded data: ", nrow(df), " rows, ", ncol(df), " columns.")

# Check that all expected columns are present before doing anything else
missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0)
        stop("The following required columns are missing from the data: ",
             paste(missing_cols, collapse = ", "))


# ==============================================================================
# 3. INSPECT MISSING VALUES AND IDENTIFIER TYPES
# ==============================================================================

message("\n--- Missing values in required columns ---")
for (col in required_cols) {
        n_miss <- sum(is.na(df[[col]]))
        pct    <- round(100 * n_miss / nrow(df), 2)
        message(sprintf("  %-25s %d missing (%s%%)", col, n_miss, pct))
}

# Check that the three ID columns contain only integer values.
# Stata numeric IDs sometimes come in as doubles (e.g. 1.0, 2.0); we check
# whether all non-missing values are whole numbers, not just that the type is integer.
message("\n--- Identifier type checks ---")
for (col in c(COL_WORKER, COL_PLANT, COL_MANAGER)) {
        vals        <- df[[col]][!is.na(df[[col]])]
        is_int_vals <- all(vals == floor(vals))
        message(sprintf("  %-20s integer-valued: %s", col, is_int_vals))
}


# ==============================================================================
# 4. DATE AND TIME VARIABLE
# ==============================================================================

# Convert date column to R Date, then create a numeric day index.
# "day" counts days since the earliest date in the data, so the first date = 0.
# This integer is what AKM estimation uses as the time variable.

df <- df %>%
        mutate(
                date = as.Date(as.numeric(.data[[COL_DATE]]), origin = "1960-01-01"),  # Stata epoch
                day  = as.integer(date - min(date, na.rm = TRUE))
        )

message("\nDate range: ", min(df$date, na.rm = TRUE), " to ", max(df$date, na.rm = TRUE))
message("Day range:  ", min(df$day,  na.rm = TRUE), " to ", max(df$day,  na.rm = TRUE))


# ==============================================================================
# 5. OUTCOME VARIABLES
# ==============================================================================

# Compute log outcomes. We first check the raw inputs for values that would
# make the log undefined (missing, zero, or negative), report how many rows
# are affected, and then drop those rows.

message("\n--- Pre-drop checks for log outcomes ---")

# Each entry: list(new column name, expression as a string for the message, actual expression)
log_inputs <- list(
        list(col = "log_rev_per_hour", label = "tkt_value / tottime_tkt",
             raw  = df[[COL_VALUE]] / df[[COL_TIME]]),
        list(col = "log_rev",          label = COL_VALUE,   raw = df[[COL_VALUE]]),
        list(col = "log_quant",        label = COL_QTY,     raw = df[[COL_QTY]]),
        list(col = "log_tottime",      label = COL_TIME,     raw = df[[COL_TIME]]),
        list(col = "log_avgtime",      label = COL_AVGTIME, raw = df[[COL_AVGTIME]]),
        list(col = "log_value",        label = COL_VPQ,     raw = df[[COL_VPQ]])
)

for (item in log_inputs) {
        n_bad <- sum(is.na(item$raw) | item$raw <= 0)
        message(sprintf("  %-18s  %d rows with missing/zero/negative values in %s",
                        item$col, n_bad, item$label))
}

# Drop rows where any log input is invalid, then compute all log outcomes.
# We drop before taking logs so we never silently produce NaN or -Inf values.

n_before <- nrow(df)

df <- df %>%
        filter(
                !is.na(.data[[COL_VALUE]])   & .data[[COL_VALUE]]   > 0,
                !is.na(.data[[COL_TIME]])    & .data[[COL_TIME]]    > 0,
                !is.na(.data[[COL_QTY]])     & .data[[COL_QTY]]     > 0,
                !is.na(.data[[COL_AVGTIME]]) & .data[[COL_AVGTIME]] > 0,
                !is.na(.data[[COL_VPQ]])     & .data[[COL_VPQ]]     > 0
        ) %>%
        mutate(
                log_rev_per_hour = log(.data[[COL_VALUE]] / .data[[COL_TIME]]),
                log_rev          = log(.data[[COL_VALUE]]),
                log_quant        = log(.data[[COL_QTY]]),
                log_tottime      = log(.data[[COL_TIME]]),
                log_avgtime      = log(.data[[COL_AVGTIME]]),
                log_value        = log(.data[[COL_VPQ]])
        )

message(sprintf("\nDropped %d rows with invalid log inputs. %d rows remaining.",
                n_before - nrow(df), nrow(df)))


# ==============================================================================
# 6. WORKER-LEVEL VARIABLES
# ==============================================================================

# overtime: 1 if the worker worked more than 480 minutes (8 hours), else 0.
df <- df %>%
        mutate(overtime = as.integer(.data[[COL_TIME]] > 480))

# tenure: days since the worker was hired, then shifted so each worker's
# minimum observed tenure is 0. This normalizes across workers who may have
# been hired before the data window begins.
df <- df %>%
        mutate(
                hiring_date = as.Date(as.numeric(.data[[COL_HIRING]]), origin = "1960-01-01"),
                tenure_raw  = as.integer(date - hiring_date)
        ) %>%
        group_by(.data[[COL_WORKER]]) %>%
        mutate(tenure = tenure_raw - min(tenure_raw, na.rm = TRUE)) %>%
        ungroup() %>%
        select(-hiring_date, -tenure_raw)  # drop the helper columns

# turnover: 1 for the last observed period of each worker, 0 for all earlier ones.
# "Last observed" is defined by the maximum value of day for that worker.
df <- df %>%
        group_by(.data[[COL_WORKER]]) %>%
        mutate(turnover = as.integer(day == max(day))) %>%
        ungroup()

message("\nWorker-level variables created: overtime, tenure, turnover.")
message("  Overtime rate:  ", round(mean(df$overtime), 3))
message("  Mean tenure:    ", round(mean(df$tenure, na.rm = TRUE), 1), " days")
message("  Turnover obs:   ", sum(df$turnover), " (one per worker)")


# ==============================================================================
# 7. PANEL VALIDATION
# ==============================================================================

message("\n--- Panel validation ---")

# Check for missing values in the three panel-defining columns
for (col in c(COL_WORKER, COL_PLANT, "day")) {
        n_miss <- sum(is.na(df[[col]]))
        if (n_miss > 0) {
                warning(col, " has ", n_miss, " missing values after processing.")
        } else {
                message("  ", col, ": no missing values. OK")
        }
}

# Check for duplicate (worker, day) pairs -- each worker should appear at most
# once per time period in a valid panel.
n_dupes <- df %>%
        group_by(.data[[COL_WORKER]], day) %>%
        filter(n() > 1) %>%
        nrow()

if (n_dupes > 0) {
        warning("Found ", n_dupes, " rows involved in duplicate (id_employee, day) pairs. ",
                "Investigate before using this dataset.")
} else {
        message("  (id_employee, day) pairs: no duplicates. OK")
}


# ==============================================================================
# 8. SAVE PROCESSED DATASET
# ==============================================================================

write_dta(df, output_path)
message("\nProcessed dataset saved to: ", output_path)
message("Final dataset: ", nrow(df), " rows, ", ncol(df), " columns.")