# Install if needed: install.packages(c("haven", "dplyr"))
library(haven)
library(dplyr)


# ==============================================================================
# 1. PATHS AND SETTINGS
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

input_path  <- file.path(script_dir, "Data", "Analysis_data", "data.dta")


df <- read_dta(input_path1)


names(df)

set.seed(123)

df <- df %>%
        mutate(
                shift_start_time = sprintf(
                        "%02d:%02d",
                        sample(0:23, n(), replace = TRUE),
                        sample(c(0, 15, 30, 45), n(), replace = TRUE)
                )
        )

df <- df %>%
        mutate(
                start_hour = sample(0:23, n(), replace = TRUE),
                shift_start_time = sprintf("%02d:00", start_hour)
        )


df <- df %>%
        mutate(camion = as.character(plant))

df <- df %>%
        mutate(id_supervisor = as.character(plant))

head(df)
# Save back to same file
write_dta(df, input_path)