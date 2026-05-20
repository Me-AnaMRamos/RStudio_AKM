# Install if needed: install.packages(c("haven", "dplyr"))
library(haven)
library(dplyr)


# ==============================================================================
# 1. PATHS AND SETTINGS
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())

input_path  <- file.path(script_dir, "Data", "Analysis_data", "data.dta")

df <- read_dta(input_path)

df <- df %>%
        mutate(plant = camion)

head(df)
# Save back to same file
write_dta(df, input_path)