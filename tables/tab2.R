# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(kableExtra)
library(broman)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "prediction_models.rda"))
load(here("output", "sub_analyses_accuracy.rda"))

# Build table -------------------------------------------------------------

tab2_df <- data.frame(
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
  mutate(
    across(starts_with("MAPE"), as.numeric),
    across(starts_with("MAPE"), ~ .x * 100),
    across(where(is.numeric), myround),
    across(starts_with("MAPE"), ~ paste0(.x, "\\%"))
  )


tab2 <- tab2_df %>%
  kbl(
    booktabs = TRUE, escape = FALSE,
    label = "none2",
    caption = "Accuracy indices of ours and Neugebauer’s equation to predict peak vertical ground reaction force with data from hip-worn accelerometers",
    col.names = c(
      "Prediction",
      "MAE", "MAPE", "RMSE",
      "MAE", "MAPE", "RMSE"
    )
  ) %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; RMSE, root mean square error",
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(position = "center") %>%
  add_header_above(c("", "Our equation" = 3, "Neugebauer equation" = 3))


tab2_html <- tab2_df %>%
  kbl(caption = "Accuracy indices of ours and Neugebauer’s equation to predict peak vertical ground reaction force with data from hip-worn accelerometers") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; RMSE, root mean square error",
    general_title = ""
  )
