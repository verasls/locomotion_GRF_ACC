# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("data", "mechanical_load_data.rda"))
mechanical_load_data <- mechanical_load_data %>%
  mutate(activity = as.factor(ifelse(speed %in% 1:6, "walking", "running")))

# LR x ATR plots ----------------------------------------------------------

# Resultant: Ankle
scatterplot_LR_res_ankle <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pRATR" ~ (italic(g) %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

# Resultant: Lower back
scatterplot_LR_res_back <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pRATR" ~ (italic(g) %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

# Resultant: Hip
scatterplot_LR_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pRATR" ~ (italic(g) %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

# Vertical: Ankle
scatterplot_LR_ver_ankle <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pVATR" ~ (italic(g) %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

# Vertical: Lower back
scatterplot_LR_ver_back <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pVATR" ~ (italic(g) %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

# Vertical: Hip
scatterplot_LR_ver_hip <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat, shape = activity),
    show_guide = FALSE
  ) +
  geom_smooth(
    aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pVATR" ~ (italic(g) %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

figS2 <- scatterplot_LR_res_ankle +
  scatterplot_LR_res_back +
  scatterplot_LR_res_hip +
  scatterplot_LR_ver_ankle +
  scatterplot_LR_ver_back +
  scatterplot_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figures", "figS2.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS2)
dev.off()
