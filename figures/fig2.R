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

# Hip pRGRF x pRACC plot --------------------------------------------------

scatterplot_GRF_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = BMI_cat, shape = activity),
    show.legend = FALSE
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
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  labs(title = "Hip", x = quote("pRACC" ~ (italic(g))), y = "pRGRF (N)")

# Back pRLR x pRAR plot ---------------------------------------------------

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
    limits = c(0, 30000),
    expand = c(0, 0),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    expand = c(0, 0),
    breaks = seq(5, 45, 5)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  labs(
    title = "Lower Back",
    x = quote("pRATR" ~ (italic(g) %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

fig2 <- scatterplot_GRF_res_hip +
  scatterplot_LR_res_back +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )
agg_tiff(
  here("figures", "fig2.tiff"),
  width = 80,
  height = 25,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig2)
dev.off()
