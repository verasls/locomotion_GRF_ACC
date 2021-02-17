# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)

# Load data ---------------------------------------------------------------

load(here("data", "running_df.rda"))

# Sample size per activity ------------------------------------------------

sample_size <- running_df %>%
  group_by(acc_placement, vector, speed) %>%
  select(acc_placement, vector, speed, pGRF_N, pLR_Ns) %>%
  summarise_all(~ sum(!is.na(.)))
knitr::kable(sample_size)

# Number of peaks median and IQR ------------------------------------------

n_peaks_desc <- running_df %>%
  summarise(
    n_peaks_median = median(n_peaks), n_peaks_iqr = IQR(n_peaks)
  )

# GRF and ACC magnitude and rate per running speeed -----------------------

# Ground reaction force
running_df %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pGRF_N, fill = vector)) +
  geom_boxplot()

# Loading rate
running_df %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = speed, y = pLR_Ns, fill = vector)) +
  geom_boxplot()

# Acceleration
running_df %>%
  ggplot(aes(x = speed, y = pACC_g, fill = vector)) +
  geom_boxplot() +
  facet_wrap(~ acc_placement)

# Acceleration transient rate
running_df %>%
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
  ~ running_df %>%
    filter(vector == .x & acc_placement == .y) %>%
    select(pGRF_N, pLR_Ns, pACC_g, pATR_gs, body_mass)
) %>%
  set_names(paste(info$vectors, info$placement, sep = "_")) %>%
  map(correlate)

# Scatterplots ------------------------------------------------------------

# GRF x ACC
plot_scatter(running_df, pACC_g, pGRF_N, color = BMI_cat) +
  facet_wrap(~ acc_placement)

# LR x ATR
plot_scatter(running_df, pATR_gs, pLR_Ns, color = BMI_cat) +
  facet_wrap(~ acc_placement)
