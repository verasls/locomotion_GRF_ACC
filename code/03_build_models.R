# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lvmisc)
library(emmeans)

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

# Save leave-one-out cross-validation data --------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  cv_res_GRF_models, cv_ver_GRF_models,
  cv_res_LR_models, cv_ver_LR_models,
  file = here("output", "loocv_data.rda")
)

# Test differences between actual and predicted values --------------------

# Prepare data
prepare_data <- function(dataframe_list) {
  map_dfr(dataframe_list, rbind) %>%
  select(subj, acc_placement, vector, speed, .actual, .predicted) %>%
  pivot_longer(
    cols = c(.actual, .predicted),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      ".actual" = paste0("actual_", acc_placement),
      ".predicted" = paste0("predicted_", acc_placement)
    )
  ) %>%
  filter(type %!in% c("actual_ankle", "actual_hip")) %>%
  mutate(
    type = recode(type, "actual_lower_back" = "actual"),
    across(- value, as.factor)
  )
}

test_res_GRF_df <- prepare_data(cv_res_GRF_models)
test_ver_GRF_df <- prepare_data(cv_ver_GRF_models)
test_res_LR_df <- prepare_data(cv_res_LR_models)
test_ver_LR_df <- prepare_data(cv_ver_LR_models)

# Resultant GRF
diff_model_res_GRF <- lmerTest::lmer(
  value ~ type + speed + type:speed + (1 | subj),
  test_res_GRF_df
)
# Fixed effects test
anova(diff_model_res_GRF)

# Vertical GRF
diff_model_ver_GRF <- lmerTest::lmer(
  value ~ type + speed + type:speed + (1 | subj),
  test_ver_GRF_df
)
# Fixed effects test
anova(diff_model_ver_GRF)

# Resultant LR
diff_model_res_LR <- lmerTest::lmer(
  value ~ type + speed + type:speed + (1 | subj),
  test_res_LR_df
)
# Fixed effects test
anova(diff_model_res_LR)

# Vertical LR
diff_model_ver_LR <- lmerTest::lmer(
  value ~ type + speed + type:speed + (1 | subj),
  test_ver_LR_df
)
# Fixed effects test
anova(diff_model_ver_LR)
