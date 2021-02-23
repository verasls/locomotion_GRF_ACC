# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)

# Load and prepare data ---------------------------------------------------

load(here("output", "prediction_models.rda"))
load(here("output", "loocv_data.rda"))

# Define functions --------------------------------------------------------

build_table <- function(model, cv, outcome, vector) {
  model_accuracy <- purrr::map(cv_res_GRF_models, accuracy, na.rm = TRUE)
  R2 <- unname(purrr::map_dbl(model_accuracy, "R2_cond"))
  MAE <- unname(purrr::map_dbl(model_accuracy, "MAE"))
  MAPE <- unname(purrr::map_dbl(model_accuracy, "MAPE"))
  RMSE <- unname(purrr::map_dbl(model_accuracy, "RMSE"))

  tibble::tibble(
    "Accelerometer placement" = c("Ankle", "Lower back", "Hip"),
    "Regression equations" = unname(
      purrr::map_chr(model, get_equation, outcome, vector)
    ),
    "R2" = R2, "MAE" = MAE, "MAPE" = MAPE, "RMSE" = RMSE
  )
}

get_equation <- function(model, outcome, vector) {
  model_coefs <- as.character(round(coef(summary(model))[, 1], 3))
  model_coefs[1] <- ifelse(
    model_coefs[1] > 0,
    model_coefs[1],
    paste0("- ", abs(as.numeric(model_coefs[1])))
  )
  model_coefs[-1] <- ifelse(
    as.numeric(model_coefs[-1]) > 0,
    paste0(" + ", abs(as.numeric(model_coefs[-1]))),
    paste0(" - ", abs(as.numeric(model_coefs[-1])))
  )

  if (stringr::str_detect(outcome, "GRF") & vector == "resultant") {
    model_outcome <- "pRGRF (N)"
    model_acceleration <- "pRACC"
  } else if (stringr::str_detect(outcome, "GRF") & vector == "vertical") {
    model_outcome <- "pVGRF (N)"
    model_acceleration <- "pVACC"
  } else if (stringr::str_detect(outcome, "LR") & vector == "resultant") {
    model_outcome <- "pVLR (Ns)"
    model_acceleration <- "pRATR"
  } else if (stringr::str_detect(outcome, "LR") & vector == "resultant") {
  } else if (stringr::str_detect(outcome, "LR") & vector == "vertical") {
    model_outcome <- "pVLR (Ns)"
    model_acceleration <- "pVATR"
  }

  regression_equation <- glue::glue(
    "{model_outcome} = {model_coefs[1]}{model_coefs[2]}({model_acceleration})\\
    {model_coefs[3]}(body mass)\\
    {model_coefs[4]}({model_acceleration} x body mass)"
  )
  as.character(regression_equation)
}

# Build table -------------------------------------------------------------

res_GRF_tb <- build_table(
  res_GRF_models, cv_res_GRF_models, "pGRF", "resultant"
)
ver_GRF_tb <- build_table(
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
