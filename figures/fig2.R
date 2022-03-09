# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("data", "mechanical_load_data.rda"))
mechanical_load_data <- mechanical_load_data %>%
  mutate(
    activity = fct_relevel(
      as.factor(ifelse(speed %in% 1:6, "Walking", "Running")),
      "Walking"
    )
  )

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

# Hip pGRF x pACC plot --------------------------------------------------

scatterplot_GRF_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  scale_x_continuous(
    limits = c(1, 9),
    expand = c(0, 0),
    breaks = seq(1, 9, 1)
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Hip pLR x pAR plot ----------------------------------------------------

scatterplot_LR_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 250),
    expand = c(0, 0),
    breaks = seq(0, 250, 50)
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Hip pGRF Bland-Altman plot ---------------------------------------------

BA_GRF_res_hip <- cv_res_GRF_models$hip %>%
  plot_bland_altman(color = BMI_cat, shape = activity, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-600, 600),
    expand = c(0, 0),
    breaks = seq(-600, 600, 200)
  ) +
  scale_x_continuous(
    limits = c(500, 3000),
    expand = c(0, 0),
    breaks = seq(500, 3000, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Hip pLR Bland-Altman plot ----------------------------------------------

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
    labels = scales::label_number(),
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

# Combine and save plots --------------------------------------------------

scatterplots <- scatterplot_GRF_res_hip +
  scatterplot_LR_res_hip &
  theme(legend.position = "none")

BA_plots <- BA_GRF_res_hip +
  BA_LR_res_hip +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig2 <- scatterplots / BA_plots +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16))

agg_tiff(
  here("figures", "fig2.tiff"),
  width = 90,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig2)
dev.off()
