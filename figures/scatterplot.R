# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("data", "running_df.rda"))

# GRF x ACC plots ---------------------------------------------------------

# Resultant: Ankle
scatterplot_GRF_res_ankle <- running_df %>%
  filter(vector == "resultant" & acc_placement == "ankle") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = quote("pRACC" ~ (italic(g))), y = "pRGRF (N)")

# Resultant: Lower back
scatterplot_GRF_res_back <- running_df %>%
  filter(vector == "resultant" & acc_placement == "lower_back") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower Back", x = quote("pRACC" ~ (italic(g))), y = "pRGRF (N)")

# Resultant: Hip
scatterplot_GRF_res_hip <- running_df %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = quote("pRACC" ~ (italic(g))), y = "pRGRF (N)")

# Vertical: Ankle
scatterplot_GRF_ver_ankle <- running_df %>%
  filter(vector == "vertical" & acc_placement == "ankle") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = quote("pVACC" ~ (italic(g))), y = "pVGRF (N)")

# Vertical: Lower back
scatterplot_GRF_ver_back <- running_df %>%
  filter(vector == "vertical" & acc_placement == "lower_back") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower Back", x = quote("pVACC" ~ (italic(g))), y = "pVGRF (N)")

# Vertical: Hip
scatterplot_GRF_ver_hip <- running_df %>%
  filter(vector == "vertical" & acc_placement == "hip") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = quote("pVACC" ~ (italic(g))), y = "pVGRF (N)")

# LR x ATR plots ----------------------------------------------------------

# Resultant: Ankle
scatterplot_LR_res_ankle <- running_df %>%
  filter(vector == "resultant" & acc_placement == "ankle") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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
scatterplot_LR_res_back <- running_df %>%
  filter(vector == "resultant" & acc_placement == "lower_back") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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
scatterplot_LR_res_hip <- running_df %>%
  filter(vector == "resultant" & acc_placement == "hip") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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
scatterplot_LR_ver_ankle <- running_df %>%
  filter(vector == "vertical" & acc_placement == "ankle") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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
scatterplot_LR_ver_back <- running_df %>%
  filter(vector == "vertical" & acc_placement == "lower_back") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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
scatterplot_LR_ver_hip <- running_df %>%
  filter(vector == "vertical" & acc_placement == "hip") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
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

# GRF x ACC
scatterplot_GRF <- scatterplot_GRF_res_ankle +
  scatterplot_GRF_res_back +
  scatterplot_GRF_res_hip +
  scatterplot_GRF_ver_ankle +
  scatterplot_GRF_ver_back +
  scatterplot_GRF_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figures", "scatterplot_GRF.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatterplot_GRF)
dev.off()

# LR x ATR
scatterplot_LR <- scatterplot_LR_res_ankle +
  scatterplot_LR_res_back +
  scatterplot_LR_res_hip +
  scatterplot_LR_ver_ankle +
  scatterplot_LR_ver_back +
  scatterplot_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figures", "scatterplot_LR.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatterplot_LR)
dev.off()
