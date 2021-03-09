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

tab1 <- rbind(res_GRF_tb, ver_GRF_tb)
# Reorder table rows
tab1 <- tab1[c(1, 4, 2, 5, 3, 6), ]
# Remove placemente name duplicates
tab1[c(2, 4, 6), 1] <- ""

tab1 <- knitr::kable(
  tab1, booktabs = TRUE, escape = FALSE, linesep = "",
  label = "none",
  caption = "Regession equations, $R^2$ and accuracy indices",
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
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; pRACC, peak resultant acceleration; pRAR, peak resultant acceleration rate; pRGRF, peak resultant ground reaction force; pRLR, peak resultant loading rate; pVACC, peak vertical acceleration; pVAR, peak vertical acceleration rate; pVGRF, peak vertical ground reaction force; pVLR peak vertical loading rate; RMSE, root mean square error",
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = "scale_down") %>%
  pack_rows(
    "Peak ground reaction force prediction", 1, 6,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  landscape()
