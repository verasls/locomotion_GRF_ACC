# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(corrr)
library(lvmisc)

# Load data ---------------------------------------------------------------

load(here("data", "mechanical_load_data.rda"))

# Sample size per activity ------------------------------------------------

sample_size <- mechanical_load_data %>%
  group_by(acc_placement, vector, speed) %>%
  select(acc_placement, vector, speed, pGRF_N, pLR_Ns) %>%
  summarise_all(~ sum(!is.na(.)))

# Sample descriptives -----------------------------------------------------

sample_descriptives <- mechanical_load_data %>%
  # Filter only one data point per participant
  filter(acc_placement == "hip" & vector == "resultant" & speed == 2) %>%
  select(age, height, body_mass, BMI) %>%
  summarise(
    across(where(is.double), list(mean = mean, sd = sd))
  ) %>%
  round(1)

sample_size_sex <- mechanical_load_data %>%
  # Filter only one data point per participant
  filter(acc_placement == "hip" & vector == "resultant" & speed == 2) %>%
  select(sex) %>%
  as_vector() %>%
  unname() %>%
  table()

# Number of peaks descriptives --------------------------------------------

n_peaks_desc <- mechanical_load_data %>%
  mutate(
    locomotion_type = as.factor(ifelse(speed %in% 1:6, "walking", "running"))
  ) %>%
  group_by(locomotion_type) %>%
  summarise(
    n_peaks_mean = mean(n_peaks), n_peaks_sd = sd(n_peaks)
  )

# GRF and ACC magnitude and rate per running speed -----------------------

# Ground reaction force
GRF_speed_plots <- mechanical_load_data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pGRF_BW, fill = vector)) +
  geom_boxplot()

# Loading rate
LR_speed_plots <- mechanical_load_data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pLR_BWs, fill = vector)) +
  geom_boxplot()

# Acceleration
ACC_speed_plots <- mechanical_load_data %>%
  ggplot(aes(x = speed, y = pACC_g, fill = vector)) +
  geom_boxplot() +
  facet_wrap(~ acc_placement)

# Acceleration rate
AR_speed_plots <- mechanical_load_data %>%
  ggplot(aes(x = speed, y = pAR_gs, fill = vector)) +
  geom_boxplot() +
  facet_wrap(~ acc_placement)

# Correlations ------------------------------------------------------------

info <- tibble(
  vectors = c(rep("resultant", 3), rep("vertical", 3)),
  placement = rep(c("ankle", "lower_back", "hip"), 2)
)
correlations <- map2(
  info$vectors, info$placement,
  ~ mechanical_load_data %>%
    filter(vector == .x & acc_placement == .y) %>%
    select(pGRF_N, pLR_Ns, pACC_g, pAR_gs, body_mass)
) %>%
  set_names(paste(info$vectors, info$placement, sep = "_")) %>%
  map(correlate)

# Scatterplots ------------------------------------------------------------

# GRF x ACC
GRF_ACC_scatterplot <- plot_scatter(
  mechanical_load_data, pACC_g, pGRF_N, color = BMI_cat
) +
  facet_wrap(~ acc_placement)

# LR x AR
LR_AR_scatterplot <- plot_scatter(
  mechanical_load_data, pAR_gs, pLR_Ns, color = BMI_cat
) +
  facet_wrap(~ acc_placement)
