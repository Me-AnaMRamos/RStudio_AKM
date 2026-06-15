library(tidyverse)

# ------------------------------------------------------------------
# 1. Read results
# ------------------------------------------------------------------
results <- read_csv(
        "Outputs/Regressions/results_week_akm-week_tenure_def1_feworker_period_clworker.csv",
        show_col_types = FALSE
)

# ------------------------------------------------------------------
# 2. Outcomes and specifications of interest
# ------------------------------------------------------------------
AKM_OUTCOMES <- c(
        "log_rev_per_hour",
        "log_wage"
)

OUTCOMES <- c(
        "log_rev_per_hour",
        "tenure",
        "separation"
)

SPECS <- c(
        "spec1_post_high_interaction",
        "spec2_post_only",
        "spec3_highhigh_vs_lowhigh"
)

res <- results %>%
        filter(
                akm_outcome %in% AKM_OUTCOMES,
                outcome %in% OUTCOMES,
                spec %in% SPECS
        )


summary_tbl <- res %>%
        select(
                akm_outcome,
                outcome,
                spec,
                term,
                estimate,
                std_error,
                p_value,
                n_obs
        ) %>%
        arrange(
                akm_outcome,
                outcome,
                spec,
                term
        )

print(summary_tbl)
