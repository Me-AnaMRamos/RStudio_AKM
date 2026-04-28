# ==============================================================================
# AKM Two-Way Fixed Effects Estimation
#
# Structure:
#   1. run_akm()          - Estimates worker and plant fixed effects for one component
#   2. run_akm_pipeline() - Prepares data, finds components, loops estimation
#                           and standardizes plant FEs across all components
#   3. Main block         - Loads data, runs pipeline for each outcome, saves results
# ==============================================================================

library(haven)   # read/write Stata .dta files
library(dplyr)   # data manipulation (filter, select, join, etc.)
library(igraph)  # build mobility graph and find connected components
library(lfe)     # felm(): fast two-way fixed effects estimation


# ==============================================================================
# 1. CORE ESTIMATION FUNCTION
#    Takes data for one connected component, estimates the AKM model,
#    and returns worker and plant fixed effects (on their raw estimated scale).
# ==============================================================================

run_akm <- function(df, outcome, worker_id, plant_id, time_var, controls = NULL) {
        
        rhs <- if (length(controls) > 0) paste(controls, collapse = " + ") else "1"
        
        # Build formula string, then convert to formula object
        # felm syntax: outcome ~ controls | FE1 + FE2 | iv | cluster
        # No IV and no clustering (0 | 0)
        # To add clustering: replace last 0 with cluster var (e.g., | 0 | plant_id)
        
        formula <- as.formula(sprintf("%s ~ %s | %s + %s | 0 | 0",
                                      outcome, rhs, worker_id, plant_id))
        
        fit    <- felm(formula, data = df)
        fe_raw <- getfe(fit)  # extract the fixed effects as a data frame
        
        # getfe() returns all FEs stacked in one data frame.
        # The "fe" column tells us which FE each row belongs to (worker or plant),
        # and "idx" holds the entity ID.
        worker_fe <- data.frame(
                id     = fe_raw$idx[fe_raw$fe == worker_id],
                effect = fe_raw$effect[fe_raw$fe == worker_id]
        )
        plant_fe <- data.frame(
                id     = fe_raw$idx[fe_raw$fe == plant_id],
                effect = fe_raw$effect[fe_raw$fe == plant_id]
        )
        
        # Rename the generic "id" column to the actual identifier name
        colnames(worker_fe)[1] <- worker_id
        colnames(plant_fe)[1]  <- plant_id
        
        # Give the effect columns informative names before returning
        names(worker_fe)[names(worker_fe) == "effect"] <- "fe_worker"
        names(plant_fe)[names(plant_fe)   == "effect"] <- "fe_plant"
        
        list(worker_fe = worker_fe, plant_fe = plant_fe, model = fit)
}


# ==============================================================================
# 2. PIPELINE FUNCTION
#    Validates inputs, builds the worker-plant graph, identifies connected
#    components, runs run_akm() on each valid component, and combines results.
#
#    cached_comps: pass the $components from a previous run to skip rebuilding
#    the graph (saves time when running multiple outcomes on the same data).
# ==============================================================================

run_akm_pipeline <- function(df, outcome, worker_id, plant_id, time_var,
                             controls = NULL, cached_comps = NULL) {
        
        # --- Optional: auto-generate time dummies from a date variable ------------------
        # If controls is "week", "month", or "bimonth", create a categorical time variable
        # from the actual calendar date in time_var.
        # Otherwise, controls should be a character vector of column names (or NULL).
        
        if (is.character(controls) && controls %in% c("week", "month", "bimonth")) {
                
                if (controls == "week") {
                        df[["time_control"]] <- format(df[[time_var]], "%Y-%U")  # year-week
                } else if (controls == "month") {
                        df[["time_control"]] <- format(df[[time_var]], "%Y-%m")  # year-month
                } else if (controls == "bimonth") {
                        m <- as.integer(format(df[[time_var]], "%m"))
                        y <- format(df[[time_var]], "%Y")
                        bimonth <- (m - 1) %/% 2 + 1
                        df[["time_control"]] <- paste0(y, "-", bimonth)         # year-bimonth
                }
                
                df[["time_control"]] <- as.factor(df[["time_control"]])
                controls <- "time_control"
                
                message("Created time dummies (", controls, ") from '", time_var, "'.")
                
        } else if (is.null(controls)) {
                controls <- character(0)
        }
        
        # --- Input checks ----------------------------------------------------------
        # Stop early with a clear message if something is wrong with the data.
        
        required_cols <- c(outcome, worker_id, plant_id, time_var, controls)
        missing       <- setdiff(required_cols, colnames(df))
        if (length(missing) > 0)
                stop("These columns are missing from the data: ", paste(missing, collapse = ", "))
        
        if (any(is.na(df[[worker_id]])))
                stop("Worker ID '", worker_id, "' has missing values.")
        
        if (any(is.na(df[[plant_id]])))
                stop("Plant ID '", plant_id, "' has missing values.")
        
        if (!is.numeric(df[[outcome]]))
                stop("Outcome variable '", outcome, "' must be numeric.")
        
        # --- Connected components --------------------------------------------------
        # Worker and plant FEs are only jointly identified within connected components
        # of the worker-plant mobility graph (groups linked by workers who switched plants).
        # We find these components using igraph, or reuse them from a previous run.
        
        if (!is.null(cached_comps)) {
                
                message("Using cached connected components.")
                comp_table <- cached_comps
                
        } else {
                
                message("Building worker-plant mobility graph...")
                
                # Each unique worker-plant pair is an edge in the graph.
                # We prefix IDs with "w_" and "p_" so igraph doesn't confuse a worker
                # and a plant that happen to share the same numeric ID.
                edges <- df %>%
                        select(all_of(c(worker_id, plant_id))) %>%
                        distinct() %>%
                        mutate(across(all_of(worker_id), ~ paste0("w_", .)),
                               across(all_of(plant_id),  ~ paste0("p_", .)))
                
                g     <- graph_from_data_frame(edges, directed = FALSE)
                comps <- components(g)  # assigns each node a component number
                
                # Put component assignments back into a table with the original IDs.
                # We strip the "w_" / "p_" prefixes we added earlier.
                membership <- data.frame(vertex  = names(comps$membership),
                                         comp_id = as.integer(comps$membership))
                
                worker_comps <- membership[startsWith(membership$vertex, "w_"), ]
                plant_comps  <- membership[startsWith(membership$vertex, "p_"), ]
                
                worker_comps[[worker_id]] <- sub("^w_", "", worker_comps$vertex)
                plant_comps[[plant_id]]   <- sub("^p_", "", plant_comps$vertex)
                
                # Restore original numeric types if needed (igraph converts everything to string)
                if (is.numeric(df[[worker_id]])) worker_comps[[worker_id]] <- as.numeric(worker_comps[[worker_id]])
                if (is.numeric(df[[plant_id]]))  plant_comps[[plant_id]]   <- as.numeric(plant_comps[[plant_id]])
                
                # Join component IDs back to the obs-level data
                comp_table <- df %>%
                        left_join(worker_comps[, c(worker_id, "comp_id")], by = worker_id) %>%
                        select(all_of(c(worker_id, plant_id, "comp_id"))) %>%
                        distinct()
                
                message("Found ", comps$no, " connected component(s).")
        }
        
        # Merge component IDs into the main data frame
        df <- df %>%
                left_join(comp_table %>% select(all_of(c(worker_id, "comp_id"))) %>% distinct(),
                          by = worker_id)
        
        # --- Drop components with only one plant -----------------------------------
        # AKM plant FEs are not identified without worker mobility across plants.
        # Any component where all workers stayed at the same plant is dropped.
        
        plants_per_comp <- comp_table %>%
                group_by(comp_id) %>%
                summarise(n_plants = n_distinct(.data[[plant_id]]), .groups = "drop")
        
        valid_comps <- plants_per_comp$comp_id[plants_per_comp$n_plants > 1]
        n_dropped   <- n_distinct(comp_table$comp_id) - length(valid_comps)
        
        if (n_dropped > 0)
                message("Dropped ", n_dropped, " component(s) with only one plant (FEs not identified).")
        
        df <- df %>% filter(comp_id %in% valid_comps)
        
        if (nrow(df) == 0)
                stop("No observations left after dropping single-plant components.")
        
        # --- Estimate AKM for each valid component ---------------------------------
        # We loop over components, run the estimator, and collect results in lists.
        
        all_worker_fe <- list()
        all_plant_fe  <- list()
        
        for (cid in valid_comps) {
                
                df_comp <- df %>% filter(comp_id == cid)
                
                message("Component ", cid, ": ",
                        nrow(df_comp), " obs | ",
                        n_distinct(df_comp[[worker_id]]), " workers | ",
                        n_distinct(df_comp[[plant_id]]),  " plants")
                
                # tryCatch lets the loop continue even if one component fails
                res <- tryCatch(
                        run_akm(df_comp, outcome, worker_id, plant_id, time_var, controls),
                        error = function(e) { warning("Component ", cid, " failed: ", e$message); NULL }
                )
                
                if (!is.null(res)) {
                        res$worker_fe$comp_id <- cid
                        res$plant_fe$comp_id  <- cid
                        all_worker_fe <- c(all_worker_fe, list(res$worker_fe))
                        all_plant_fe  <- c(all_plant_fe,  list(res$plant_fe))
                }
        }
        
        # Stack all components into single data frames and return
        worker_fe_all <- bind_rows(all_worker_fe)
        plant_fe_all  <- bind_rows(all_plant_fe)
        
        # --- Standardize plant FEs across all components --------------------------
        # Raw plant FEs from different components are on incompatible scales because
        # felm can only pin the level within each component (the FEs are identified
        # up to an additive constant per component). Standardizing (z-scoring) the
        # plant FEs after pooling all components puts them on a common scale --
        # mean zero, standard deviation one -- so they can be compared across plants
        # regardless of which component they came from.
        # Worker FEs are left on their raw scale since cross-component comparisons
        # are not the goal here.
        
        plant_fe_all$fe_plant_std <- scale(plant_fe_all$fe_plant)[, 1]
        
        message("Plant FEs standardized across all components (mean=0, sd=1).")
        
        # Return results and the component table (reusable for other outcomes)
        list(
                worker_fe  = worker_fe_all,
                plant_fe   = plant_fe_all,
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
data_path  <- file.path(script_dir, "Data", "Analysis_data", "data.dta")
output_dir <- file.path(script_dir, "Outputs")

dir.create(output_dir, showWarnings = FALSE)  # create Outputs/ if it doesn't exist yet

df <- read_dta(data_path)
message("Loaded data: ", nrow(df), " rows, ", ncol(df), " columns.")

# --- Settings ----------------------------------------------------------------
# i = worker identifier column, j = plant identifier column.
# Change these two lines if running AKM on different columns.

i <- "employee_id"   # worker ID column
j <- "plant"         # plant ID column
t <- "day"           # time variable column

# Controls: NULL for none, or "week" / "month" / "bimonth" for automatic time
# dummies, or a character vector of existing column names (e.g. c("age", "tenure")).
# The label below is used in output file names so different runs are easy to tell apart.

CONTROLS       <- "week"
CONTROLS_LABEL <- "week"   # used in output filenames -- change to match CONTROLS

# Examples:
#   CONTROLS <- NULL  ;  CONTROLS_LABEL <- "no_controls"
#   CONTROLS <- c("age", "tenure")  ;  CONTROLS_LABEL <- "age_tenure"

outcomes <- c("log_rev_per_hour", "log_rev", "log_quant",
              "log_tottime", "log_avgtime", "log_value")

# --- Run first outcome and cache the connected components -------------------
# Computing the mobility graph is the slow step. We do it once and reuse
# the component assignments for all subsequent outcomes.

message("\n--- Outcome: ", outcomes[1], " ---")
first_result <- run_akm_pipeline(df, outcomes[1], i, j, t, CONTROLS)

# Output filenames include the outcome and the controls label, e.g.:
#   worker_fe_log_rev_no_controls.dta
#   plant_fe_log_rev_no_controls.dta
write_dta(first_result$worker_fe,
          file.path(output_dir, paste0("worker_fe_", outcomes[1], "_", CONTROLS_LABEL, ".dta")))
write_dta(first_result$plant_fe,
          file.path(output_dir, paste0("plant_fe_",  outcomes[1], "_", CONTROLS_LABEL, ".dta")))

cached_components <- first_result$components  # reuse for all remaining outcomes

# --- Loop over remaining outcomes using the cached components ---------------

for (outcome in outcomes[-1]) {
        
        message("\n--- Outcome: ", outcome, " ---")
        result <- run_akm_pipeline(df, outcome, i, j, t, CONTROLS,
                                   cached_comps = cached_components)
        
        write_dta(result$worker_fe,
                  file.path(output_dir, paste0("worker_fe_", outcome, "_", CONTROLS_LABEL, ".dta")))
        write_dta(result$plant_fe,
                  file.path(output_dir, paste0("plant_fe_",  outcome, "_", CONTROLS_LABEL, ".dta")))
}

message("\nDone. Results saved to: ", output_dir)
