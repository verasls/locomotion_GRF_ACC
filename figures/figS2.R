# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("output", "loocv_data.rda"))
cv_res_GRF_models <- map(
  cv_res_GRF_models,
  ~ mutate(
    .x, activity = as.factor(ifelse(speed %in% 1:6, "walking", "running"))
  )
)
cv_ver_GRF_models <- map(
  cv_ver_GRF_models,
  ~ mutate(
    .x, activity = as.factor(ifelse(speed %in% 1:6, "walking", "running"))
  )
)
cv_res_LR_models <- map(
  cv_res_LR_models,
  ~ mutate(
    .x, activity = as.factor(ifelse(speed %in% 1:6, "walking", "running"))
  )
)
cv_ver_LR_models <- map(
  cv_ver_LR_models,
  ~ mutate(
    .x, activity = as.factor(ifelse(speed %in% 1:6, "walking", "running"))
  )
)

# GRF plots ---------------------------------------------------------------

# Resultant: Ankle
BA_GRF_res_ankle <- cv_res_GRF_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1500, 600),
    expand = c(0, 0),
    breaks = seq(-1500, 600, 300)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Actual - Predicted pRGRF (N)"
  )

# Resultant: Lower back
BA_GRF_res_back <- cv_res_GRF_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Actual - Predicted pRGRF (N)"
  )

# Resultant: Hip
BA_GRF_res_hip <- cv_res_GRF_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Actual - Predicted pRGRF (N)"
  )

# Vertical: Ankle
BA_GRF_ver_ankle <- cv_ver_GRF_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-900, 600),
    expand = c(0, 0),
    breaks = seq(-900, 600, 300)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Actual - Predicted pVGRF (N)"
  )

# Vertical: Lower back
BA_GRF_ver_back <- cv_ver_GRF_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Actual - Predicted pVGRF (N)"
  )

# Vertical: Hip
BA_GRF_ver_hip <- cv_ver_GRF_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Actual - Predicted pVGRF (N)"
  )

# LR plots ----------------------------------------------------------------

# Resultant: Ankle
BA_LR_res_ankle <- ggplot()
# BA_LR_res_ankle <- cv_res_LR_models$ankle %>%
#   plot_bland_altman(color = BMI_cat, shape = activity) +
#   guides(shape = FALSE) +
#   scale_color_nejm() +
#   scale_y_continuous(
#     labels = scales::label_number(),
#     limits = c(-100000, 125000),
#     expand = c(0, 0),
#     breaks = seq(-100000, 125000, 50000)
#   ) +
#   theme_light() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.title = element_blank()
#   ) +
#   labs(
#     title = "Ankle",
#     x = quote("Mean of Actual and Predicted pRLR" ~ (N %.% s^-1)),
#     y = quote("Actual - Predicted pRLR" ~ (N %.% s^-1))
#   )

# Resultant: Lower back
BA_LR_res_back <- cv_res_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("Mean of Actual and Predicted pRLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pRLR" ~ (N %.% s^-1))
  )

# Resultant: Hip
BA_LR_res_hip <- cv_res_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("Mean of Actual and Predicted pRLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pRLR" ~ (N %.% s^-1))
  )

# Vertical: Ankle
BA_LR_ver_ankle <- ggplot()
# BA_LR_ver_ankle <- cv_ver_LR_models$ankle %>%
#   plot_bland_altman(color = BMI_cat, shape = activity) +
#   guides(shape = FALSE) +
#   scale_color_nejm() +
#   scale_y_continuous(
#     labels = scales::label_number(),
#     limits = c(-100000, 150000),
#     expand = c(0, 0),
#     breaks = seq(-100000, 150000, 50000)
#   ) +
#   theme_light() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.title = element_blank()
#   ) +
#   labs(
#     title = "Ankle",
#     x = quote("Mean of Actual and Predicted pVLR" ~ (N %.% s^-1)),
#     y = quote("Actual - Predicted pVLR" ~ (N %.% s^-1))
#   )

# Vertical: Lower back
BA_LR_ver_back <- cv_ver_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("Mean of Actual and Predicted pVLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pVLR" ~ (N %.% s^-1))
  )

# Vertical: Hip
BA_LR_ver_hip <- cv_ver_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  guides(shape = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("Mean of Actual and Predicted pVLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pVLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

# GFR
bland_altman_GRF <- BA_GRF_res_ankle +
  BA_GRF_res_back +
  BA_GRF_res_hip +
  BA_GRF_ver_ankle +
  BA_GRF_ver_back +
  BA_GRF_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figures", "bland-altman_GRF.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(bland_altman_GRF)
dev.off()

# LR
bland_altman_LR <- BA_LR_res_ankle +
  BA_LR_res_back +
  BA_LR_res_hip +
  BA_LR_ver_ankle +
  BA_LR_ver_back +
  BA_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figures", "bland-altman_LR.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(bland_altman_LR)
dev.off()
