# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("output", "loocv_data.rda"))
cv_res_LR_models <- map(
  cv_res_LR_models,
  ~ mutate(
    .x,
    activity = fct_relevel(
      as.factor(ifelse(speed %in% 1:6, "Walking", "Running")),
      "Walking"
    )
  )
)
cv_ver_LR_models <- map(
  cv_ver_LR_models,
  ~ mutate(
    .x,
    activity = fct_relevel(
      as.factor(ifelse(speed %in% 1:6, "Walking", "Running")),
      "Walking"
    )
  )
)

# LR plots ----------------------------------------------------------------

# Resultant: Ankle
BA_LR_res_ankle <- cv_res_LR_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 50000),
    expand = c(0, 0),
    breaks = seq(0, 50000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Resultant vector - Ankle placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Resultant: Lower back
BA_LR_res_back <- cv_res_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 50000),
    expand = c(0, 0),
    breaks = seq(0, 50000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Resultant vector - Lower back placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Resultant: Hip
BA_LR_res_hip <- cv_res_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 50000),
    expand = c(0, 0),
    breaks = seq(0, 50000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Vertical: Ankle
BA_LR_ver_ankle <- cv_ver_LR_models$ankle %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 50000),
    expand = c(0, 0),
    breaks = seq(0, 50000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Vertical vector - Ankle placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Vertical: Lower back
BA_LR_ver_back <- cv_ver_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Vertical vector - Lower back placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Vertical: Hip
BA_LR_ver_hip <- cv_ver_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-30000, 30000),
    expand = c(0, 0),
    breaks = seq(-30000, 30000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 50000),
    expand = c(0, 0),
    breaks = seq(0, 50000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:"),
    alpha = FALSE
  ) +
  labs(
    title = "Vertical vector - Hip placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

figS4 <- BA_LR_res_ankle +
  BA_LR_res_back +
  BA_LR_res_hip +
  BA_LR_ver_ankle +
  BA_LR_ver_back +
  BA_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_tiff(
  here("figures", "figS4.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS4)
dev.off()

agg_png(
  here("figures", "figS4.png"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS4)
dev.off()
