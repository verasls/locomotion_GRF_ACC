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
knitr::kable(sample_size)

# Number of peaks median and IQR ------------------------------------------

n_peaks_desc <- mechanical_load_data %>%
  summarise(
    n_peaks_median = median(n_peaks), n_peaks_iqr = IQR(n_peaks)
  )

# GRF and ACC magnitude and rate per running speeed -----------------------

# Ground reaction force
mechanical_load_data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pGRF_BW, fill = vector)) +
  geom_boxplot()

# Loading rate
mechanical_load_data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pLR_BWs, fill = vector)) +
  geom_boxplot()

# Acceleration
mechanical_load_data %>%
  ggplot(aes(x = speed, y = pACC_g, fill = vector)) +
  geom_boxplot() +
  facet_wrap(~ acc_placement)

# Acceleration transient rate
mechanical_load_data %>%
  ggplot(aes(x = speed, y = pATR_gs, fill = vector)) +
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
    select(pGRF_N, pLR_Ns, pACC_g, pATR_gs, body_mass)
) %>%
  set_names(paste(info$vectors, info$placement, sep = "_")) %>%
  map(correlate)

# Scatterplots ------------------------------------------------------------

# GRF x ACC
plot_scatter(mechanical_load_data, pACC_g, pGRF_N, color = BMI_cat) +
  facet_wrap(~ acc_placement)

# LR x ATR
plot_scatter(mechanical_load_data, pATR_gs, pLR_Ns, color = BMI_cat) +
  facet_wrap(~ acc_placement)
