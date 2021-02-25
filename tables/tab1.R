# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(broman)
library(kableExtra)
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

tab1 <- rbind(res_GRF_tb, ver_GRF_tb) %>%
  mutate(
    MAPE = paste0(myround(MAPE * 100, 2), "\\%")
  )
# Reorder table rows
tab1 <- tab1[c(1, 4, 2, 5, 3, 6), ]
# Remove placemente name duplicates
tab1[c(2, 4, 6), 1] <- ""


tab1 <- kbl(
  tab1, booktabs = TRUE, escape = FALSE, linesep = "",
  col.names = c(
    linebreak("Accelerometer\nplacement"),
    "Regression equations",
    "$R^2$",
    "MAE",
    "MAPE",
    "RMSE"
  ),
  align = c(rep("l", 2), rep("r", 4))
) %>%
  kable_styling(latex_options = "scale_down") %>%
  pack_rows(
    "Peak ground reaction force prediction", 1, 6,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  landscape()
