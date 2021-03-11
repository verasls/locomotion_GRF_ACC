# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
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

# GRF x ACC plots ---------------------------------------------------------

# Resultant: Ankle
scatterplot_GRF_res_ankle <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 14.5),
    expand = c(0, 0),
    breaks = seq(1, 14, 2)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Resultant: Lower back
scatterplot_GRF_res_back <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 5.5),
    expand = c(0, 0),
    breaks = seq(1, 6, 1)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Resultant: Hip
scatterplot_GRF_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 6.5),
    expand = c(0, 0),
    breaks = seq(1, 6, 1)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Vertical: Ankle
scatterplot_GRF_ver_ankle <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 10.5),
    expand = c(0, 0),
    breaks = seq(1, 10, 2)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Vertical: Lower back
scatterplot_GRF_ver_back <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 5.5),
    expand = c(0, 0),
    breaks = seq(0, 6, 1)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Vertical: Hip
scatterplot_GRF_ver_hip <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity)
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
    limits = c(0.5, 6.5),
    expand = c(0, 0),
    breaks = seq(1, 6, 1)
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
    x = quote("pACC" ~ (italic(g))),
    y = "pGRF (N)"
  )

# Combine and save plots --------------------------------------------------

figS1 <- scatterplot_GRF_res_ankle +
  scatterplot_GRF_res_back +
  scatterplot_GRF_res_hip +
  scatterplot_GRF_ver_ankle +
  scatterplot_GRF_ver_back +
  scatterplot_GRF_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "figS1.png"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS1)
dev.off()

agg_tiff(
  here("figures", "figS1.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS1)
dev.off()
