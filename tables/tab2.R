# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(kableExtra)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "prediction_models.rda"))
load(here("output", "sub_analyses_accuracy.rda"))

# Build table -------------------------------------------------------------

tab2 <- data.frame(
  Prediction = c("Overall", "Walking", "Running"),
  MAE_our = c(
    accuracy_ver_GRF_models$hip$MAE,
    walking_ver_accuracy$hip$MAE,
    running_ver_accuracy$hip$MAE
  ),
  MAPE_our = c(
    accuracy_ver_GRF_models$hip$MAPE,
    walking_ver_accuracy$hip$MAPE,
    running_ver_accuracy$hip$MAPE
  ),
  RMSE_our = c(
    accuracy_ver_GRF_models$hip$RMSE,
    walking_ver_accuracy$hip$RMSE,
    running_ver_accuracy$hip$RMSE
  ),
  MAE_neug = c(
    neug_overall_accuracy$MAE,
    neug_walk_accuracy$MAE,
    neug_run_accuracy$MAE
  ),
  MAPE_neug = c(
    neug_overall_accuracy$MAPE,
    neug_walk_accuracy$MAPE,
    neug_run_accuracy$MAPE
  ),
  RMSE_neug = c(
    neug_overall_accuracy$RMSE,
    neug_walk_accuracy$RMSE,
    neug_run_accuracy$RMSE
  )
) %>%
  kbl(
    col.names = c(
      "Prediction",
      "MAE", "MAPE", "RMSE",
      "MAE", "MAPE", "RMSE"
    ),
    booktabs = TRUE
  ) %>%
  add_header_above(c("", "Our equation" = 3, "Neugebauer equation" = 3))
