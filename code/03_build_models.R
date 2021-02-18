# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lvmisc)

# Load and prepare data ---------------------------------------------------

load(here("data", "running_df.rda"))

# Create a list of 3 data frames: one for each accelerometer placement
running_df_placements <- map(
  c("ankle", "lower_back", "hip"),
  ~ filter(running_df, acc_placement == .x)
) %>% set_names(c("ankle", "lower_back", "hip"))

# Filter the data frames in the list, separating the vertical and resultant
# vectors
res_running_df <- map(running_df_placements, filter, vector == "resultant")
ver_running_df <- map(running_df_placements, filter, vector == "vertical")

# Build GRF models --------------------------------------------------------

GRF_formula <- as.formula(
  "pGRF_N ~ pACC_g + body_mass + pACC_g:body_mass + (1 | subj)"
)

# Resultant vector
# Build models
res_GRF_models <- map(
  res_running_df,
  ~ lmer(GRF_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_res_GRF_models <- map2(
  res_GRF_models, res_running_df,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_res_GRF_models <- map(cv_res_GRF_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_res_GRF_models <- map(
  cv_res_GRF_models, plot_bland_altman, color = BMI_cat
)

# Vertical vector
# Build models
ver_GRF_models <- map(
  ver_running_df,
  ~ lmer(GRF_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_ver_GRF_models <- map2(
  ver_GRF_models, ver_running_df,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_ver_GRF_models <- map(cv_ver_GRF_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_ver_GRF_models <- map(
  cv_ver_GRF_models, plot_bland_altman, color = BMI_cat
)

# Build LR models ---------------------------------------------------------

LR_formula <- as.formula(
  "pLR_Ns ~ pATR_gs + body_mass + pATR_gs:body_mass + (1 | subj)"
)

# As LR for the ankle placement is missing at the moment, this placement
# needs to be removed from the data frame list
res_running_df_LR <- res_running_df[-1]
ver_running_df_LR <- ver_running_df[-1]

# Resultant vector
# Build models
res_LR_models <- map(
  res_running_df_LR,
  ~ lmer(LR_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_res_LR_models <- map2(
  res_LR_models, res_running_df_LR,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_res_LR_models <- map(cv_res_LR_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_res_LR_models <- map(
  cv_res_LR_models, plot_bland_altman, color = BMI_cat
)

# Vertical vector
# Build models
ver_LR_models <- map(
  ver_running_df_LR,
  ~ lmer(LR_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_ver_LR_models <- map2(
  ver_LR_models, ver_running_df_LR,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_ver_LR_models <- map(cv_ver_LR_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_ver_LR_models <- map(
  cv_ver_LR_models, plot_bland_altman, color = BMI_cat
)
