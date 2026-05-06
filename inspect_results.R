# ==============================================================================
# inspect_did.R
#
# Quick inspection of DiD_tables.dta before building the presentation.
# Run this, copy the full console output, and paste it back.
# ==============================================================================

library(haven)
library(dplyr)

# ---- path -------------------------------------------------------------------
# Adjust if the file lives elsewhere.
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
path <- file.path(script_dir, "DiD_tables.dta")

df <- read_dta(path)

# ==============================================================================
# 1. Structure
# ==============================================================================
cat("\n==============================\n")
cat("STRUCTURE\n")
cat("==============================\n")
cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
cat("Column names:\n")
print(colnames(df))
cat("\nColumn types:\n")
print(sapply(df, class))

# ==============================================================================
# 2. Head
# ==============================================================================
cat("\n==============================\n")
cat("FIRST 20 ROWS\n")
cat("==============================\n")
print(as.data.frame(head(df, 20)), digits = 4)

# ==============================================================================
# 3. Unique values of categorical / identifier columns
# ==============================================================================
cat("\n==============================\n")
cat("UNIQUE VALUES — KEY COLUMNS\n")
cat("==============================\n")

# Guess which columns are categorical (few unique values or character)
for (col in colnames(df)) {
        nu <- n_distinct(df[[col]], na.rm = TRUE)
        if (nu <= 30 || is.character(df[[col]]) || is.factor(df[[col]])) {
                cat("\n$", col, " (", nu, " unique):\n", sep = "")
                print(sort(unique(df[[col]])))
        }
}

# ==============================================================================
# 4. Numeric summaries for all columns
# ==============================================================================
cat("\n==============================\n")
cat("NUMERIC SUMMARIES\n")
cat("==============================\n")
print(summary(df))

# ==============================================================================
# 5. Missing values
# ==============================================================================
cat("\n==============================\n")
cat("MISSING VALUES PER COLUMN\n")
cat("==============================\n")
miss <- sapply(df, function(x) sum(is.na(x)))
print(miss[miss > 0])

# ==============================================================================
# 6. If there are estimate / std_error / p_value columns, show results by spec
# ==============================================================================
has_est <- all(c("estimate", "std_error") %in% colnames(df))

if (has_est) {
        cat("\n==============================\n")
        cat("RESULTS TABLE (estimate / se / p)\n")
        cat("==============================\n")
        
        # Detect grouping columns (anything that is not a numeric result column)
        result_cols <- intersect(colnames(df),
                                 c("estimate", "std_error", "t_stat", "p_value",
                                   "n_obs", "conf_low", "conf_high"))
        group_cols  <- setdiff(colnames(df), result_cols)
        
        # Print grouped
        for (gc in group_cols) {
                if (n_distinct(df[[gc]], na.rm = TRUE) <= 50) {
                        cat("\n--- by", gc, "---\n")
                        tbl <- df %>%
                                group_by(.data[[gc]]) %>%
                                summarise(
                                        n          = n(),
                                        mean_est   = mean(estimate,  na.rm = TRUE),
                                        sd_est     = sd(estimate,    na.rm = TRUE),
                                        mean_se    = mean(std_error, na.rm = TRUE),
                                        pct_sig05  = mean(p_value < 0.05, na.rm = TRUE),
                                        .groups = "drop"
                                )
                        print(as.data.frame(tbl), digits = 4)
                }
        }
        
        # Full table sorted by outcome then term
        sort_cols <- intersect(c("outcome", "term", "akm_outcome", "spec"), colnames(df))
        cat("\n--- Full results table ---\n")
        print(as.data.frame(df %>% arrange(across(all_of(sort_cols)))), digits = 4)
}

cat("\n[inspection complete]\n")



# صاف / clean environment
rm(list = ls())

library(haven)
library(dplyr)
library(stringr)
library(readr)

# ==============================
# 1. LOAD RESULTS TABLE
# ==============================

df <- read_dta("DiD_tables.dta")

# ==============================
# 2. KEEP ONLY TREATMENT EFFECTS
# ==============================

treat <- df %>%
        filter(
                spec == "post_x_highmfe",
                str_detect(term, "^postXhigh")
        )

# Extract FE definition cleanly
treat <- treat %>%
        mutate(
                fe_type = str_remove(term, "postXhigh_")
        )
# ==============================
# 2b. KEEP BASELINE POST EFFECT (β1)
# ==============================

post_eff <- df %>%
        filter(
                term == "post"
        )

# Keep FE label consistent (comes from feff_col already)
post_eff <- post_eff %>%
        mutate(
                fe_type = feff_col
        )

# ==============================
# 3. ADD SIGNIFICANCE + FORMAT
# ==============================

treat <- treat %>%
        mutate(
                signif = case_when(
                        p < 0.01 ~ "***",
                        p < 0.05 ~ "**",
                        p < 0.1  ~ "*",
                        TRUE     ~ ""
                ),
                coef_se = sprintf("%.4f (%.4f)%s", coef, se, signif)
        )

post_eff <- post_eff %>%
        mutate(
                signif = case_when(
                        p < 0.01 ~ "***",
                        p < 0.05 ~ "**",
                        p < 0.1  ~ "*",
                        TRUE     ~ ""
                ),
                coef_se = sprintf("%.4f (%.4f)%s", coef, se, signif)
        )
# ==============================
# 4. DEFINE OUTCOME GROUPS
# ==============================

productivity_outcomes <- c(
        "log_rev_per_hour",
        "log_rev",
        "log_quant",
        "log_value",
        "log_tottime",
        "log_avgtime"
)

behavior_outcomes <- c(
        "absent",
        "ndaysabsent",
        "on_time",
        "overtime",
        "novertime",
        "turnover",
        "changed_start_hour"
)

# ==============================
# 5. PRODUCTIVITY TABLE
# ==============================

prod_table <- treat %>%
        filter(outcome %in% productivity_outcomes) %>%
        select(outcome, fe_type, coef, se, p, coef_se) %>%
        arrange(outcome, desc(abs(coef)))

# ==============================
# 6. BEHAVIOR TABLE
# ==============================

behav_table <- treat %>%
        filter(outcome %in% behavior_outcomes) %>%
        select(outcome, fe_type, coef, se, p, coef_se) %>%
        arrange(outcome, desc(abs(coef)))

# ==============================
# 7. MAIN RESULT PER OUTCOME
# ==============================

main_effects <- treat %>%
        group_by(outcome) %>%
        slice_max(order_by = abs(coef), n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(outcome, fe_type, coef, se, p, coef_se)

# ==============================
# 7b. MAIN POST EFFECT (β1) PER OUTCOME
# ==============================

main_post <- post_eff %>%
        group_by(outcome) %>%
        slice_max(order_by = abs(coef), n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(outcome, fe_type, coef, se, p, coef_se)

# ==============================
# 8. DIRECTION + SIGNIFICANCE SUMMARY
# ==============================

summary_flags <- treat %>%
        mutate(
                positive = coef > 0,
                significant = p < 0.05
        ) %>%
        group_by(outcome) %>%
        summarise(
                mean_coef = mean(coef),
                any_significant = any(significant),
                share_positive = mean(positive),
                .groups = "drop"
        )

# ==============================
# 9. SAVE OUTPUTS
# ==============================

write_csv(prod_table, "Outputs/prod_table.csv")
write_csv(behav_table, "Outputs/behavior_table.csv")
write_csv(main_effects, "Outputs/main_effects.csv")
write_csv(summary_flags, "Outputs/summary_flags.csv")

write_csv(main_post, "Outputs/main_post_effects.csv")
# ==============================
# 10. PRINT FOR QUICK VIEW
# ==============================

cat("\n==============================\n")
cat("MAIN EFFECTS (SLIDE-READY)\n")
cat("==============================\n")
print(main_effects)

cat("\n==============================\n")
cat("PRODUCTIVITY RESULTS\n")
cat("==============================\n")
print(prod_table)

cat("\n==============================\n")
cat("BEHAVIOR RESULTS\n")
cat("==============================\n")
print(behav_table)

cat("\n==============================\n")
cat("POST EFFECTS (β1)\n")
cat("==============================\n")
print(main_post)

# Run this in R to get the combined table values
# Assumes your results dataframe is called `res` with same structure as the CSV

natural_specs <- data.frame(
        outcome  = c("log_rev","log_value","log_tottime","log_avgtime",
                     "log_quant","log_rev_per_hour"),
        feff_col = c("log_rev_feff","log_value_feff","log_tottime_feff",
                     "log_avgtime_feff","log_quant_feff","log_rev_per_hour_feff")
)

# beta2 (treatment effect)
b2 <- df |>
        filter(spec == "post_x_highmfe") |>
        inner_join(natural_specs, by = c("outcome","feff_col")) |>
        filter(grepl("^postXhigh", term)) |>
        select(outcome, feff_col, beta2=coef, se2=se, p2=p)

# beta1 (common post effect, from same spec)
b1 <- df |>
        filter(spec == "post_x_highmfe") |>
        inner_join(natural_specs, by = c("outcome","feff_col")) |>
        filter(term == "post") |>
        select(outcome, feff_col, beta1=coef, se1=se, p1=p)

combined <- left_join(b1, b2, by = c("outcome","feff_col"))
print(combined, n=Inf)
