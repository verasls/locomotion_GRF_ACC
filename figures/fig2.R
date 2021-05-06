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

# Hip pGRF x pACC plot --------------------------------------------------

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
    limits = c(0, 7),
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

# Hip pLR x pAR plot ----------------------------------------------------

scatterplot_LR_res_hip <- mechanical_load_data %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot() +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat, shape = activity)
  ) +
  geom_smooth(
    aes(x = pAR_gs, y = pLR_Ns, color = BMI_cat),
    method = "lm", se = FALSE
  ) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  scale_x_continuous(
    limits = c(0, 250),
    expand = c(0, 0),
    breaks = seq(50, 200, 50)
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
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("pLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

fig2 <- scatterplot_GRF_res_hip +
  scatterplot_LR_res_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "fig2.png"),
  width = 80,
  height = 25,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig2)
dev.off()

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
