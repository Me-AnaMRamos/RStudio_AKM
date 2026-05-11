# ==============================================================================
# Script 2: run_regressions.R
#
# Reads the event study dataset from Script 1b and runs two regression
# specifications for every (outcome variable) x (AKM FE classifier) combination:
#
#   Spec 1 — outcome ~ post + high + post*high | worker + period FEs | cluster
#   Spec 2 — outcome ~ post               | worker + period FEs | cluster
#
# Results from all runs are collected into a single tidy table and saved.
#
# Input:  Data/Analysis_data/processed/event_study_[PERIOD_COLLAPSED]_def[MOVE_DEF].dta
# Output: Outputs/Regressions/results_[PERIOD]_def[MOVE_DEF]_fe[FE_LABEL]_cl[CL_LABEL].csv
# ==============================================================================

library(haven)   # read .dta files
library(dplyr)   # data manipulation
library(lfe)     # felm(): fast two-way FE estimation with clustering


# ==============================================================================
# 1. SETTINGS  ← only section that normally needs editing
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

# --- Input file settings (must match Scripts 1a and 1b) ----------------------
PERIOD_COLLAPSED   <- "week"   # "week" or "month" — must match collapse.R / define_moves.R
MOVE_DEF <- 1        # 1 or 2            — must match define_moves.R

# --- AKM outcomes (must match akm.R and define_moves.R) ----------------------
# These drive the classifier loop: for each entry there must be a column
# move_to_high_fe_[entry] in the event study dataset.
AKM_OUTCOMES <- c("log_rev_per_hour", "log_rev", "log_quant",
                  "log_tottime",      "log_avgtime", "log_value")

CONTROLS_AKM <- "week"

# --- Regression outcome variables --------------------------------------------
# Full list of dependent variables to run regressions for.
REG_OUTCOMES <- c(
        "log_rev_per_hour",   # log revenue per hour
        "log_rev",            # log total revenue
        "log_quant",          # log quantity
        "log_tottime",        # log total time worked
        "log_avgtime",        # log average time per piece
        "log_value",          # log value added (or similar)
        "overtime_total",     # total overtime in period (sum)
        "overtime_mean",      # average overtime per day in period
        "turnover",           # turnover indicator
        "absent_any",         # absence indicator (any absence in period)
        "absent_days",        # total days absent in period
        "on_time",            # on-time start indicator
        "changed_start_hour"  # changed start hour relative to previous period
)

# --- Fixed effects -----------------------------------------------------------
# A character vector of column names to absorb as fixed effects in felm().
# Default: worker FE + period FE.
# felm syntax: list them separated by " + " in the | slot.
FE_VARS  <- c("id_employee", "period")
FE_LABEL <- "worker_period"   # used in output filename

# --- Clustering --------------------------------------------------------------
# Column name to cluster standard errors on.
CLUSTER_VAR <- "id_employee"
CL_LABEL    <- "worker"       # used in output filename

# --- Column names (must match collapse.R / define_moves.R) -------------------
COL_WORKER <- "id_employee"
COL_PERIOD <- "period"
COL_POST   <- "post"

# --- Paths -------------------------------------------------------------------
input_path <- file.path(script_dir, "Outputs", "Merged",
                        paste0("panel_data",
                               "_level-",PERIOD_COLLAPSED,
                               "_akm-", CONTROLS_AKM,
                               "_def-", MOVE_DEF,
                               ".dta"))

output_dir <- file.path(script_dir, "Outputs", "Regressions")
output_path <- file.path(output_dir,
                         paste0("results_", PERIOD_COLLAPSED,
                                "_def",  MOVE_DEF,
                                "_fe",   FE_LABEL,
                                "_cl",   CL_LABEL, ".csv"))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

df <- read_dta(input_path)

message("Loaded event study dataset: ", nrow(df), " rows, ",
        n_distinct(df[[COL_WORKER]]), " workers.")

# Ensure period is a proper Date (haven sometimes returns numeric Stata dates).
if (!inherits(df[[COL_PERIOD]], "Date"))
        df[[COL_PERIOD]] <- as.Date(df[[COL_PERIOD]])

# felm needs the FE variables as factors.
df[[COL_WORKER]] <- as.factor(df[[COL_WORKER]])
df[[COL_PERIOD]] <- as.factor(df[[COL_PERIOD]])


# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

# Build the felm formula string.
#   outcome  : dependent variable name (string)
#   rhs_vars : character vector of RHS variables (e.g. c("post", "high", "post_x_high"))
#   fe_vars  : character vector of FE variables   (e.g. c("id_employee", "period"))
#   cluster  : character vector of clustering variable(s)
build_formula <- function(outcome, rhs_vars, fe_vars, cluster) {
        rhs     <- paste(rhs_vars, collapse = " + ")
        fe_part <- paste(fe_vars,  collapse = " + ")
        cl_part <- paste(cluster,  collapse = " + ")
        as.formula(sprintf("%s ~ %s | %s | 0 | %s", outcome, rhs, fe_part, cl_part))
}

# Extract a tidy coefficient table from a felm fit.
# Returns a data frame with one row per coefficient.
tidy_felm <- function(fit, outcome, akm_outcome, spec) {
        s      <- summary(fit)
        coefs  <- s$coefficients          # matrix: rows = terms, cols = est/se/t/p
        terms  <- rownames(coefs)
        
        data.frame(
                outcome      = outcome,
                akm_outcome  = akm_outcome,
                spec         = spec,
                term         = terms,
                estimate     = coefs[, "Estimate"],
                std_error    = coefs[, "Cluster s.e."],
                t_stat       = coefs[, "t value"],
                p_value      = coefs[, "Pr(>|t|)"],
                n_obs        = nobs(fit),
                stringsAsFactors = FALSE,
                row.names    = NULL
        )
}


# ==============================================================================
# 4. MAIN LOOP
# ==============================================================================
# Outer loop : AKM FE classifiers  (move_to_high_fe_[akm_outcome])
# Middle loop: regression outcomes
# Inner block: two specifications per combination

all_results <- list()

for (akm_out in AKM_OUTCOMES) {
        
        col_high <- paste0("move_to_high_fe_", akm_out)
        
        if (!col_high %in% colnames(df)) {
                warning("Column '", col_high, "' not found in dataset — skipping AKM outcome.")
                next
        }
        
        message("\n=== AKM classifier: ", col_high, " ===")
        
        # Create a temporary "high" column and the interaction for this classifier.
        # We do this outside the outcome loop so it is computed only once per
        # classifier, then re-used across all outcome regressions.
        #
        # Note: "high" is time-invariant at the worker level and will be absorbed
        # by the worker FE in practice. It is included explicitly per the spec for
        # clarity and completeness.
        
        df_reg <- df %>%
                mutate(
                        high      = as.integer(.data[[col_high]]),
                        post_x_high = as.integer(.data[[COL_POST]]) * high
                )
        
        for (outcome in REG_OUTCOMES) {
                
                if (!outcome %in% colnames(df)) {
                        warning("Outcome column '", outcome, "' not found — skipping.")
                        next
                }
                
                # Drop rows where the outcome or post is missing.
                # For Spec 1 also drop rows where high is missing (non-movers).
                df_base <- df_reg %>% filter(!is.na(.data[[outcome]]), !is.na(.data[[COL_POST]]))
                df_sp1  <- df_base  %>% filter(!is.na(high))
                
                message("  Outcome: ", outcome,
                        " | n_base = ", nrow(df_base),
                        " | n_spec1 = ", nrow(df_sp1))
                
                # ------------------------------------------------------------------
                # Spec 1: outcome ~ post + high + post*high | FEs | cluster
                # ------------------------------------------------------------------
                res1 <- tryCatch({
                        f1  <- build_formula(outcome,
                                             rhs_vars = c(COL_POST, "high", "post_x_high"),
                                             fe_vars  = FE_VARS,
                                             cluster  = CLUSTER_VAR)
                        fit1 <- felm(f1, data = df_sp1)
                        tidy_felm(fit1, outcome, akm_out, spec = "spec1_post_high_interaction")
                }, error = function(e) {
                        warning("Spec 1 failed [outcome=", outcome, ", akm=", akm_out, "]: ", e$message)
                        NULL
                })
                
                # ------------------------------------------------------------------
                # Spec 2: outcome ~ post | FEs | cluster
                # ------------------------------------------------------------------
                res2 <- tryCatch({
                        f2   <- build_formula(outcome,
                                              rhs_vars = COL_POST,
                                              fe_vars  = FE_VARS,
                                              cluster  = CLUSTER_VAR)
                        fit2 <- felm(f2, data = df_base)
                        tidy_felm(fit2, outcome, akm_out, spec = "spec2_post_only")
                }, error = function(e) {
                        warning("Spec 2 failed [outcome=", outcome, ", akm=", akm_out, "]: ", e$message)
                        NULL
                })
                
                all_results <- c(all_results, list(res1), list(res2))
        }
}


# ==============================================================================
# 5. COMBINE AND SAVE
# ==============================================================================

results_tbl <- bind_rows(Filter(Negate(is.null), all_results))

# Attach run-level metadata columns so the file is self-documenting.
results_tbl <- results_tbl %>%
        mutate(
                period      = PERIOD_COLLAPSED,
                move_def    = MOVE_DEF,
                fe_spec     = FE_LABEL,
                cluster_var = CL_LABEL,
                .before     = 1
        )

message("\nResults table: ", nrow(results_tbl), " rows ",
        "(", n_distinct(results_tbl$outcome), " outcomes × ",
        n_distinct(results_tbl$akm_outcome), " classifiers × 2 specs).")

write.csv(results_tbl, output_path, row.names = FALSE)
message("Saved to: ", output_path)