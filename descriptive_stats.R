library(tidyverse)
library(haven)

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------

i <- "id_employee"
j <- "camion"
t <- "date"
CONTROLS_AKM <- "week_tenure"
PERIOD_COLLAPSED <- "week"
akm_dir <- "Outputs/AKM"

OUTCOMES_AKM <- c(
        "log_rev_per_hour",
        "log_rev",
        "log_quant",
        "log_tottime",
        "log_avgtime",
        "log_value"
)

STD_OUTCOMES_AKM <- paste0("std_", OUTCOMES_AKM)

OUTCOMES <- c(OUTCOMES_AKM,  
              "overtime",
              "turnover",
              "absent")

worker_fe <- read_dta(
        file.path(
                akm_dir,
                paste0("worker_fe_LEV_", i, "_", j,
                       "_CTRL_", CONTROLS_AKM, ".dta")
        )
)

unit_fe <- read_dta(
        file.path(
                akm_dir,
                paste0("unit_fe_LEV_", i, "_", j,
                       "_CTRL_", CONTROLS_AKM, ".dta")
        )
)

components <- read_dta(
        file.path(
                akm_dir,
                paste0("components_LEV_", i, "_", j, ".dta")
        )
)

data_processed <- read_dta(
        "Data/Analysis_data/processed/data_processed.dta"
)

merged <- read_dta(
        file.path(
        file.path(script_dir, "Outputs", "Merged"),
        paste0(
                "panel_data",
                "_lev_", i, "_", j, "_", PERIOD_COLLAPSED,
                "_akm-", CONTROLS_AKM,
                "_def-", 1,
                ".dta"
        ))
)

# ============================================
# ================= COMPONENTS STATISTICS ===================
# ============================================

n_components <- components %>%
        summarise(
                n_components = n_distinct(comp_id),
        )
n_components

selected_components <- components %>%
        right_join(unit_fe, by = j) %>%
        summarise(selected_components = n_distinct(comp_id))

selected_components

pct_kept <- selected_components / n_components * 100

message(sprintf("Total components: %s", n_components))
message(sprintf("Components kept: %s", selected_components))
message(sprintf("Percentage of components kept: %.2f%%", pct_kept))


# ============================================
# ================= MOVERS ===================
# ============================================

total_workers <- components %>%
        summarise(n = n_distinct(.data[[i]]))  
total_workers

workers_selected <- worker_fe %>% 
        left_join(components, by = i) %>%
        summarise(n = n_distinct(.data[[i]]))

workers_selected

movers <- components %>% 
        group_by(.data[[i]]) %>% 
        mutate( moves = n_distinct(.data[[j]])) %>%
        filter(moves>1) %>%
        ungroup()

movers
movers <- movers %>%
        summarise(n = n_distinct(.data[[i]]))  
movers
message(sprintf("Total workers: %s", total_workers))
message(sprintf("Workers kept: %s", workers_selected))
message(sprintf("Total movers: %s", movers))

pct_kept    <- 100 * workers_selected / total_workers
pct_dropped <- 100 - pct_kept
pct_movers  <- 100 * movers / workers_selected

message(sprintf("Percentage of workers kept: %.2f%%", pct_kept))
message(sprintf("Percentage of workers dropped: %.2f%%", pct_dropped))
message(sprintf("Percentage of selected workers who are movers: %.2f%%", pct_movers))

# ============================================
# ================= FE STATISTICS ===================
# ============================================


cat("\n==============================\n")
cat("TRUCK FE\n")
cat("==============================\n")

UNIT_FE_OUTCOMES <- c(
        "std_log_rev_per_hour",
        "std_log_rev",
        "std_log_quant",
        "std_log_tottime",
        "std_log_avgtime",
        "std_log_value"
)


summary_akm_outcomes <- unit_fe %>%
        summarise(
                across(
                        all_of(UNIT_FE_OUTCOMES),
                        list(
                                N      = ~sum(!is.na(.)),
                                Mean   = ~mean(., na.rm = TRUE),
                                SD     = ~sd(., na.rm = TRUE),
                                P10    = ~quantile(., .10, na.rm = TRUE),
                                Median = ~median(., na.rm = TRUE),
                                P90    = ~quantile(., .90, na.rm = TRUE),
                                Min    = ~min(., na.rm = TRUE),
                                Max    = ~max(., na.rm = TRUE)
                        ),
                        .names = "{.col}_{.fn}"
                )
        ) %>%
        pivot_longer(
                everything(),
                names_to = c("Outcome", ".value"),
                names_sep = "_(?=[^_]+$)"
        )

summary_akm_outcomes

# ============================================
# ================= OUTCOME STATISTICS ===================
# ============================================

summary_outcomes <- purrr::map_dfr(
        OUTCOMES,
        \(v) {
                
                x <- data_processed[[v]]
                
                tibble(
                        outcome = v,
                        N       = sum(!is.na(x)),
                        mean    = mean(x, na.rm = TRUE),
                        median  = median(x, na.rm = TRUE),
                        sd      = sd(x, na.rm = TRUE),
                        p10     = quantile(x, 0.10, na.rm = TRUE),
                        p90     = quantile(x, 0.90, na.rm = TRUE),
                        min     = min(x, na.rm = TRUE),
                        max     = max(x, na.rm = TRUE)
                )
        }
)

print(summary_outcomes)
summary_outcomes

# ============================================
# ================= NEW TRUCK STATISTICS ===================
# ============================================

moves <- merged %>%
        filter(treated == 1L) %>%
        distinct(movedate_id, .keep_all = TRUE)

truck_stats <- moves %>%
        summarise(
                pct_dep_truck_new  = mean(dep_truck_is_new == 1, na.rm = TRUE),
                pct_dep_truck_old  = mean(dep_truck_is_new == 0, na.rm = TRUE),
                pct_dest_truck_new = mean(dest_truck_is_new == 1, na.rm = TRUE),
                pct_dest_truck_old = mean(dest_truck_is_new == 0, na.rm = TRUE)
        )

truck_stats
# ============================================
# ================= LOW/HIGH FE MOVE STATISTICS ===================
# ============================================


summary(merged)
names(moves)
moves_unit_productivity_stats <- purrr::map_dfr(OUTCOMES_AKM, function(outcome) {
        dest_var <- paste0("moveTo_highFE_", outcome)
        dep_var  <- paste0("moveFrom_highFE_", outcome)
        moves %>%
                summarise(
                        outcome = outcome,
                        
                        pct_to_high = mean(.data[[dest_var]] == 1, na.rm = TRUE),
                        
                        pct_high_to_high =
                                mean(.data[[dep_var]] == 1 & .data[[dest_var]] == 1, na.rm = TRUE),
                        
                        pct_low_to_high =
                                mean(.data[[dep_var]] == 0 & .data[[dest_var]] == 1, na.rm = TRUE),
                        
                        pct_to_low = mean(.data[[dest_var]] == 0, na.rm = TRUE),
                        
                        pct_high_to_low =
                                mean(.data[[dep_var]] == 1 & .data[[dest_var]] == 0, na.rm = TRUE),
                        
                        pct_low_to_low =
                                mean(.data[[dep_var]] == 0 & .data[[dest_var]] == 0, na.rm = TRUE)
                )
})

# =========================================================================
# ================= DAILY WORKER AND UNIT FE'S COVARIANCE ===================
# =========================================================================

names(data_processed)
head(data_processed)
head(unit_fe)
head(worker_fe)

panel_covariance <- data_processed %>%
        select(all_of(c(i, j, t)))

panel_covariance

        

only_std_fe_unit <- unit_fe %>%
        select(all_of(c(j, STD_OUTCOMES_AKM))) %>%
        rename_with(
                ~ paste0(.x, "_unit"),
                all_of(STD_OUTCOMES_AKM)
        )

only_std_fe_worker <- worker_fe %>%
        select(all_of(c(i, STD_OUTCOMES_AKM))) %>%
        rename_with(
                ~ paste0(.x, "_worker"),
                all_of(STD_OUTCOMES_AKM)
        )


panel_covariance <- panel_covariance %>% right_join(only_std_fe_unit, by = j)
panel_covariance
dim(panel_covariance)
panel_covariance <- panel_covariance %>% inner_join(only_std_fe_worker, by= i)

cov_daily <- panel_covariance %>%
        group_by(.data[[t]]) %>%
        summarise(
                !!!setNames(
                        lapply(OUTCOMES_AKM, function(outcome) {
                                
                                worker_col <- paste0("std_", outcome, "_worker")
                                unit_col   <- paste0("std_", outcome, "_unit")
                                
                                rlang::expr(
                                        cov(
                                                !!rlang::sym(worker_col),
                                                !!rlang::sym(unit_col),
                                                use = "complete.obs"
                                        )
                                )
                                
                        }),
                        paste0("cov_", OUTCOMES_AKM)
                ),
                .groups = "drop"
        )

cov_daily
