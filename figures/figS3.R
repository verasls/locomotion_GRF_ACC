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
    .x,
    activity = fct_relevel(
      as.factor(ifelse(speed %in% 1:6, "Walking", "Running")),
      "Walking"
    )
  )
)
cv_ver_GRF_models <- map(
  cv_ver_GRF_models,
  ~ mutate(
    .x,
    activity = fct_relevel(
      as.factor(ifelse(speed %in% 1:6, "Walking", "Running")),
      "Walking"
    )
  )
)

# GRF plots ---------------------------------------------------------------

# Resultant: Ankle
BA_GRF_res_ankle <- cv_res_GRF_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 300)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Ankle placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Resultant: Lower back
BA_GRF_res_back <- cv_res_GRF_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Lower back placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Resultant: Hip
BA_GRF_res_hip <- cv_res_GRF_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Vertical: Ankle
BA_GRF_ver_ankle <- cv_ver_GRF_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 300)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Ankle placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Vertical: Lower back
BA_GRF_ver_back <- cv_ver_GRF_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Lower back placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Vertical: Hip
BA_GRF_ver_hip <- cv_ver_GRF_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  scale_x_continuous(
    limits = c(400, 3100),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Hip placement",
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Combine and save plots --------------------------------------------------

figS3 <- BA_GRF_res_ankle +
  BA_GRF_res_back +
  BA_GRF_res_hip +
  BA_GRF_ver_ankle +
  BA_GRF_ver_back +
  BA_GRF_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "figS3.png"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS3)
dev.off()

agg_tiff(
  here("figures", "figS3.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS3)
dev.off()
