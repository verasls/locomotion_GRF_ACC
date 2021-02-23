# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "prediction_models.rda"))
load(here("output", "loocv_data.rda"))

# Build table -------------------------------------------------------------

res_GRF_tb <- build_formula_table(
  res_GRF_models, cv_res_GRF_models, "pGRF", "resultant"
)
ver_GRF_tb <- build_formula_table(
  ver_GRF_models, cv_ver_GRF_models, "pGRF", "vertical"
)

tab1 <- rbind(res_GRF_tb, ver_GRF_tb)
# Reorder table rows
tab1 <- tab1[c(1, 4, 2, 5, 3, 6), ]
# Remove placemente name duplicates
tab1[c(2, 4, 6), 1] <- ""

# Save the table tibble ---------------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(tab1, file = here("output", "tab1.rda"))
