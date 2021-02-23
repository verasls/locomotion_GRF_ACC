# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)

# Load and prepare data ---------------------------------------------------

load(here("output", "loocv_data.rda"))
load(here("data", "mechanical_load_data.rda"))

# Accuracy per activity ---------------------------------------------------

# Walking
cv_res_GRF_models %>%
  map(~ filter(.x, speed %in% 1:6)) %>%
  map(accuracy, na.rm = TRUE)
# Running
cv_res_GRF_models %>%
  map(~ filter(.x, speed %in% 7:14)) %>%
  map(accuracy, na.rm = TRUE)

# Prediction comparison ---------------------------------------------------

# Neugebauer 2014 coefficients
b0 <- 5.247
b1 <- 0.271
b2 <- 0.014
b3 <- 0.934
b4 <- -0.216
# Apply Neugebauer equation
walk_predictions <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "hip" & speed %in% 1:6) %>%
  transmute(
    subj, speed, pACC_g, body_mass, BMI_cat, pGRF_N,
    predicted_Neug = exp(
      b0 + b1 * pACC_g + b2 * body_mass + b3 * 0 + b4 * pACC_g * 0
    )
  )
run_predictions <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "hip" & speed %in% 1:6) %>%
  transmute(
    subj, speed, pACC_g, body_mass, BMI_cat, pGRF_N,
    predicted_Neug = exp(
      b0 + b1 * pACC_g + b2 * body_mass + b3 * 1 + b4 * pACC_g * 1
    )
  )
overall_predictions <- rbind(walk_predictions, run_predictions)
# Compute MAPE
neug_walk_accuracy <- mean_error_abs_pct(
  walk_predictions$pGRF_N, walk_predictions$predicted_Neug, na.rm = TRUE
)
neug_run_accuracy <- mean_error_abs_pct(
  run_predictions$pGRF_N, run_predictions$predicted_Neug, na.rm = TRUE
)
neug_overall_accuracy <- mean_error_abs_pct(
  overall_predictions$pGRF_N, overall_predictions$predicted_Neug, na.rm = TRUE
)
