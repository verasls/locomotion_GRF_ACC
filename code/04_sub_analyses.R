# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "loocv_data.rda"))
load(here("data", "mechanical_load_data.rda"))

# Bland-Altman plots tests ------------------------------------------------

# Linear regression diff ~ mean
ba_regression_res_GRF <- map(cv_res_GRF_models, bland_altman_regression)
ba_regression_ver_GRF <- map(cv_ver_GRF_models, bland_altman_regression)
ba_regression_res_LR <- map(cv_res_LR_models, bland_altman_regression)
ba_regression_ver_LR <- map(cv_ver_LR_models, bland_altman_regression)

# Check if bias differs from 0
ba_bias_test_res_GRF <- map(cv_res_GRF_models, bland_altman_t_test)
ba_bias_test_ver_GRF <- map(cv_ver_GRF_models, bland_altman_t_test)
ba_bias_test_res_LR <- map(cv_res_LR_models, bland_altman_t_test)
ba_bias_test_ver_LR <- map(cv_ver_LR_models, bland_altman_t_test)

# Accuracy per activity ---------------------------------------------------

# Walking
walking_accuracy <- cv_res_GRF_models %>%
  map(~ filter(.x, speed %in% 1:6)) %>%
  map(accuracy, na.rm = TRUE)
# Running
running_accuracy <- cv_res_GRF_models %>%
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
  filter(vector == "vertical" & acc_placement == "hip" & speed %in% 7:14) %>%
  transmute(
    subj, speed, pACC_g, body_mass, BMI_cat, pGRF_N,
    predicted_Neug = exp(
      b0 + b1 * pACC_g + b2 * body_mass + b3 * 1 + b4 * pACC_g * 1
    )
  )
overall_predictions <- rbind(walk_predictions, run_predictions)
# Compute accuracy
neug_walk_accuracy <- compute_accuracy(
  walk_predictions$pGRF_N, walk_predictions$predicted_Neug
)
neug_run_accuracy <- compute_accuracy(
  run_predictions$pGRF_N, run_predictions$predicted_Neug
)
neug_overall_accuracy <- compute_accuracy(
  overall_predictions$pGRF_N, overall_predictions$predicted_Neug
)

# Save Neugebauer 2014 prediction accuracy --------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  neug_overall_accuracy, neug_walk_accuracy, neug_run_accuracy,
  file = here("output", "neug_accuracy.rda")
)
