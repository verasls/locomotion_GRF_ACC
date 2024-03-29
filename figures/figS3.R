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

# LR x AR plots -----------------------------------------------------------

# Resultant: Ankle
scatterplot_LR_res_ankle <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 400),
    expand = c(0, 0),
    breaks = seq(0, 400, 50)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Ankle placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Resultant: Lower back
scatterplot_LR_res_back <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
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
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Resultant vector - Lower back placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Resultant: Hip
scatterplot_LR_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
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
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
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

# Vertical: Ankle
scatterplot_LR_ver_ankle <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "ankle") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Ankle placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Vertical: Lower back
scatterplot_LR_ver_back <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "lower_back") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Lower back placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Vertical: Hip
scatterplot_LR_ver_hip <- mechanical_load_data %>%
  filter(vector == "vertical" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity), alpha = 0.5
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE, show.legend = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 300),
    expand = c(0, 0),
    breaks = seq(0, 300, 50)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, vjust = 0.2),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  guides(
    color = guide_legend(title = "Body mass index category:"),
    shape = guide_legend(title = "Locomotion type:")
  ) +
  labs(
    title = "Vertical vector - Hip placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

figS3 <- scatterplot_LR_res_ankle +
  scatterplot_LR_res_back +
  scatterplot_LR_res_hip +
  scatterplot_LR_ver_ankle +
  scatterplot_LR_ver_back +
  scatterplot_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

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
