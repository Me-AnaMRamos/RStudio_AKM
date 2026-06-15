# ==============================================================================
# AKM Two-Way Fixed Effects Estimation
#
# Structure:
#   1. run_akm()          - Estimates worker and unit fixed effects for one component
#   2. run_akm_pipeline() - Prepares data, finds components, loops estimation
#                           and standardizes unit FEs across all components
#   3. Main block         - Loads data, runs pipeline for each outcome, saves results
# ==============================================================================


# Required packages: haven, dplyr, igraph, lfe
# Install if needed: install.packages(c("haven", "dplyr", "igraph", "lfe"))
library(haven)   # read/write Stata .dta files
library(dplyr)   # data manipulation (filter, select, join, etc.)
library(igraph)  # build mobility graph and find connected components
library(lfe)     # felm(): fast two-way fixed effects estimation


# ==============================================================================
# 1. CORE ESTIMATION FUNCTION
#    Takes data for one connected component, estimates the AKM model,
#    and returns worker and unit fixed effects (on their raw estimated scale).
# ==============================================================================

run_akm <- function(df, outcome, worker_id, unit_id, time_var, controls = NULL) {
        
        rhs <- if (length(controls) > 0) paste(controls, collapse = " + ") else "1"
        
        # Build formula string, then convert to formula object
        # felm syntax: outcome ~ controls | FE1 + FE2 | iv | cluster
        # No IV and no clustering (0 | 0)
        # To add clustering: replace last 0 with cluster var (e.g., | 0 | unit_id)
        
        formula <- as.formula(sprintf("%s ~ %s | %s + %s | 0 | 0",
                                      outcome, rhs, worker_id, unit_id))
        
        fit    <- felm(formula, data = df)
        fe_raw <- getfe(fit)  # extract the fixed effects as a data frame
        
        # getfe() returns all FEs stacked in one data frame.
        # The "fe" column tells us which FE each row belongs to (worker or unit),
        # and "idx" holds the entity ID.
        worker_fe <- data.frame(
                id     = fe_raw$idx[fe_raw$fe == worker_id],
                effect = fe_raw$effect[fe_raw$fe == worker_id]
        )
        unit_fe <- data.frame(
                id     = fe_raw$idx[fe_raw$fe == unit_id],
                effect = fe_raw$effect[fe_raw$fe == unit_id]
        )
        
        # Rename the generic "id" column to the actual identifier name
        colnames(worker_fe)[1] <- worker_id
        colnames(unit_fe)[1]  <- unit_id
        
        # Give the effect columns informative names before returning
        names(worker_fe)[names(worker_fe) == "effect"] <- "fe_worker"
        names(unit_fe)[names(unit_fe)   == "effect"] <- "fe_unit"
        
        list(worker_fe = worker_fe, unit_fe = unit_fe, model = fit)
}


# ==============================================================================
# 2. PIPELINE FUNCTION
#    Validates inputs, builds the worker-unit graph, identifies connected
#    components, runs run_akm() on each valid component, and combines results.
#
#    cached_comps: pass the $components from a previous run to skip rebuilding
#    the graph (saves time when running multiple outcomes on the same data).
# ==============================================================================

run_akm_pipeline <- function(df, outcome, worker_id, unit_id, time_var, controls = NULL, comp_table,
                             valid_comps) {
        
        # --- Input checks ----------------------------------------------------------
        # Stop early with a clear message if something is wrong with the data.
        
        required_cols <- c(outcome, worker_id, unit_id, time_var, controls)
        print(required_cols)
        print(controls)
        
        
        missing       <- setdiff(required_cols, colnames(df))
        
        if (length(missing) > 0){
                print(names(df))
                print(required_cols)
                stop("These columns are missing from the data: ", paste(missing, collapse = ", "))}
        
        if (any(is.na(df[[worker_id]])))
                stop("Worker ID '", worker_id, "' has missing values.")
        
        if (any(is.na(df[[unit_id]])))
                stop("Plant ID '", unit_id, "' has missing values.")
        
        if (!is.numeric(df[[outcome]]))
                stop("Outcome variable '", outcome, "' must be numeric.")
        
        # --- Estimate AKM for each valid component ---------------------------------
        # We loop over components, run the estimator, and collect results in lists.
        
        all_worker_fe <- list()
        all_unit_fe  <- list()
        
        for (cid in valid_comps) {
                
                df_comp <- df %>% filter(comp_id == cid)
                
                message("Component ", cid, ": ",
                        nrow(df_comp), " obs | ",
                        n_distinct(df_comp[[worker_id]]), " workers | ",
                        n_distinct(df_comp[[unit_id]]),  " units")
                
                # tryCatch lets the loop continue even if one component fails
                res <- tryCatch(
                        run_akm(df_comp, outcome, worker_id, unit_id, time_var, controls),
                        error = function(e) { warning("Component ", cid, " failed: ", e$message); NULL }
                )
                
                if (!is.null(res)) {
                        res$worker_fe$comp_id <- cid
                        res$unit_fe$comp_id  <- cid
                        all_worker_fe <- c(all_worker_fe, list(res$worker_fe))
                        all_unit_fe  <- c(all_unit_fe,  list(res$unit_fe))
                }
        }
        
        # Stack all components into single data frames and return
        worker_fe_all <- bind_rows(all_worker_fe)
        unit_fe_all  <- bind_rows(all_unit_fe)
        
        # --- Standardize units FEs across all components --------------------------
        #  Standardizing (z-scoring) the
        # unit FEs after pooling all components puts them on a common scale so they can be compared across units
        # regardless of which component they came from.
        # Worker FEs are left on their raw scale
        
        unit_fe_all$fe_unit_std <- scale(unit_fe_all$fe_unit)[, 1]
        worker_fe_all$fe_worker_std <- scale(worker_fe_all$fe_worker)[, 1]
        
        
        message("Plant FEs standardized across all components (mean=0, sd=1).")
        
        # Return results and the component table (reusable for other outcomes)
        list(
                worker_fe  = worker_fe_all,
                unit_fe   = unit_fe_all,
                components = comp_table
        )
}


# ==============================================================================
# 3. MAIN EXECUTION
#    Loads data, runs the pipeline for each outcome variable, and saves results.
# ==============================================================================

# --- Paths -------------------------------------------------------------------
# script_dir is the folder where this script lives.
# Data is expected at Data/Analysis_data/data.dta relative to script_dir.
# Results are saved to Outputs/ relative to script_dir.

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
data_path  <- file.path(script_dir, "Data", "Analysis_data", "processed", "data_processed.dta")
output_dir <- file.path(script_dir, "Outputs", "AKM")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)  # create Outputs/ if it doesn't exist yet

df <- read_dta(data_path)
message("Loaded data: ", nrow(df), " rows, ", ncol(df), " columns.")

df <- df %>%
        mutate(
                date = as.Date(day, origin = "1960-01-01")
        )

# --- Settings ----------------------------------------------------------------
# i = worker identifier column, j = unit identifier column.
# Change these two lines if running AKM on different columns.

i <- "id_employee"   # worker ID column
j <- "camion"         # unit ID column
t <- "date"           # time variable column

# Controls: NULL for none, or "week" / "month" / "bimonth" for automatic time
# dummies, or a character vector of existing column names (e.g. c("age", "tenure")).
# The label below is used in output file names so different runs are easy to tell apart.

CONTROLS       <- c("week", "tenure")  
CONTROLS_LABEL <- "week_tenure"   # used in output filenames -- change to match CONTROLS

# Examples:
#   CONTROLS <- NULL  ;  CONTROLS_LABEL <- "no_controls"
#   CONTROLS <- c("week", "tenure")  ;  CONTROLS_LABEL <- "week_tenure"

# --- Optional: Auto-generate time dummies from a date variable ------------------
# detect whether CONTROLS contains a period keyword
period_keywords <- c("week", "month", "bimonth")
period_match    <- intersect(CONTROLS, period_keywords)

if (length(period_match) > 0) {
        
        # use the first matched keyword if somehow more than one is passed
        freq <- period_match[1]
        
        if (freq == "week") {
                df[["time_control"]] <- format(df[[t]], "%Y-%U")  # year-week
        } else if (freq == "month") {
                df[["time_control"]] <- format(df[[t]], "%Y-%m")  # year-month
        } else if (freq == "bimonth") {
                m <- as.integer(format(df[[t]], "%m"))
                y <- format(df[[t]], "%Y")
                bimonth <- (m - 1) %/% 2 + 1
                df[["time_control"]] <- paste0(y, "-", bimonth)         # year-bimonth
        }
        
        df[["time_control"]] <- as.factor(df[["time_control"]])

        # replace the keyword in CONTROLS with the new column name,
        # keeping any other column names that were passed alongside it
        CONTROLS <- c(setdiff(CONTROLS, period_keywords), "time_control")
        
        message("Created time dummies (", freq, ") from '", t, "'.")
        
} else if (is.null(CONTROLS)) {
        CONTROLS <- character(0)
}




message("Building worker-unit mobility graph...")

# --- Connected components --------------------------------------------------
# Worker and unit FEs are only jointly identified within connected components
# of the worker-unit mobility graph (groups linked by workers who switched units).
# We find these components using igraph, or reuse them from a previous run.


edges <- df %>%
        select(all_of(c(i, j))) %>%
        distinct() %>%
        mutate(
                across(all_of(i), ~ paste0("w_", .)),
                across(all_of(j), ~ paste0("p_", .))
        )

g <- graph_from_data_frame(edges, directed = FALSE)

comps <- components(g)

membership <- data.frame(
        vertex  = names(comps$membership),
        comp_id = as.integer(comps$membership)
)

worker_comps <- membership[startsWith(membership$vertex, "w_"), ]
unit_comps  <- membership[startsWith(membership$vertex, "p_"), ]

worker_comps[[i]] <- sub("^w_", "", worker_comps$vertex)
unit_comps[[j]]  <- sub("^p_", "", unit_comps$vertex)

if (is.numeric(df[[i]]))
        worker_comps[[i]] <- as.numeric(worker_comps[[i]])

if (is.numeric(df[[j]]))
        unit_comps[[j]] <- as.numeric(unit_comps[[j]])

comp_table <- df %>%
        left_join(worker_comps[, c(i, "comp_id")], by = i) %>%
        select(all_of(c(i, j, "comp_id"))) %>%
        distinct()

# --- Drop components with only one unit -----------------------------------
# AKM unit FEs are not identified without worker mobility across units.
# Any component where all workers stayed at the same unit is dropped.

units_per_comp <- comp_table %>%
        group_by(comp_id) %>%
        summarise(
                n_units = n_distinct(.data[[j]]),
                .groups = "drop"
        )

valid_comps <- units_per_comp$comp_id[
        units_per_comp$n_units > 1
]

dropped_comps <- units_per_comp %>%
        filter(n_units <= 1)

cat("\nDropped components:\n")
print(dropped_comps)

message(
        "Dropped ",
        nrow(dropped_comps),
        " component(s) with only one unit (FEs not identified)."
)

df <- df %>%
        left_join(
                comp_table %>%
                        select(all_of(c(i, "comp_id"))) %>%
                        distinct(),
                by = i
        )


df <- df %>% filter(comp_id %in% valid_comps)

if (nrow(df) == 0)
        stop("No observations left after dropping single-unit components.")



outcomes <- c("log_rev_per_hour", "log_rev", "log_quant",
              "log_tottime", "log_avgtime", "log_value")

# Store all results
all_worker_fe <- list()
all_unit_fe  <- list()

# --- Run AKM for each outcome -----------------------------------------------
for (outcome in outcomes) {
        
        message("\n--- Outcome: ", outcome, " ---")
        
        result <- run_akm_pipeline(
                df,
                outcome,
                i,
                j,
                t,
                CONTROLS,
                comp_table,
                valid_comps
        )
        
        names(result$unit_fe)
        
        # Worker FE
        all_worker_fe[[outcome]] <- result$worker_fe %>%
                select(all_of(i), fe_worker, fe_worker_std) %>%
                rename(!!outcome := fe_worker,
                       !!paste0("std_", outcome) := fe_worker_std
                       )
        
        # Plant FE
        all_unit_fe[[outcome]] <- result$unit_fe %>%
                select(all_of(j), fe_unit, fe_unit_std) %>%
                rename(
                        !!outcome := fe_unit,
                        !!paste0("std_", outcome) := fe_unit_std
                )
}

# Merge all worker FE tables by worker id
worker_fe_all <- Reduce(
        function(x, y) full_join(x, y, by = i),
        all_worker_fe
)

# Merge all unit FE tables by unit id
unit_fe_all <- Reduce(
        function(x, y) full_join(x, y, by = j),
        all_unit_fe
)

worker_fe_all[[i]] <- as.numeric(as.character(worker_fe_all[[i]]))
unit_fe_all[[j]] <- as.numeric(as.character(unit_fe_all[[j]]))

# Save combined files
write_dta(worker_fe_all,
          file.path(output_dir,
                    paste0("worker_fe_LEV_", i, "_", j, "_CTRL_", CONTROLS_LABEL, ".dta")))

write_dta(unit_fe_all,
          file.path(output_dir,
                    paste0("unit_fe_LEV_", i, "_", j, "_CTRL_", CONTROLS_LABEL, ".dta")))
summary(comp_table)

# Components only once
write_dta(comp_table,
          file.path(output_dir,
                    paste0("components_LEV_", i, "_", j, ".dta")))

message("\nDone. Results saved to: ", output_dir)

print(worker_fe_all)
print(unit_fe_all)
