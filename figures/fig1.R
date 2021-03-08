# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("data", "mechanical_load_data.rda"))
sample_size <- mechanical_load_data %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  group_by(speed) %>%
  select(pGRF_N) %>%
  summarise_all(~ sum(!is.na(.)))

# Plot BMI distribution per speed -----------------------------------------

fig1 <- mechanical_load_data %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = speed, y = BMI)) +
  geom_boxplot(outlier.shape = NA, fill = "gray", alpha = 0.6, width = 0.5) +
  geom_jitter(alpha = 0.5) +
  scale_y_continuous(
    limits = c(10, 60),
    expand = c(0, 0),
    breaks = seq(10, 60, 10)
  ) +
  scale_x_discrete(
    labels = c(
      "2" = quote(atop(2 ~ km %.% h^-1, paste("n = 64"))),
      "3" = quote(atop(3 ~ km %.% h^-1, paste("n = 64"))),
      "4" = quote(atop(4 ~ km %.% h^-1, paste("n = 64"))),
      "5" = quote(atop(5 ~ km %.% h^-1, paste("n = 61"))),
      "6" = quote(atop(6 ~ km %.% h^-1, paste("n = 46"))),
      "7" = quote(atop(7 ~ km %.% h^-1, paste("n = 20"))),
      "8" = quote(atop(8 ~ km %.% h^-1, paste("n = 20"))),
      "9" = quote(atop(9 ~ km %.% h^-1, paste("n = 20"))),
      "10" = quote(atop(10 ~ km %.% h^-1, paste("n = 19"))),
      "11" = quote(atop(11 ~ km %.% h^-1, paste("n = 19"))),
      "12" = quote(atop(12 ~ km %.% h^-1, paste("n = 19"))),
      "13" = quote(atop(13 ~ km %.% h^-1, paste("n = 19"))),
      "14" = quote(atop(14 ~ km %.% h^-1, paste("n = 17")))
    )
  ) +
  theme_light() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16)
  ) +
  labs(
    x = "",
    y = quote("Body mass index" ~ (kg %.% m^-2))
  )

# Save plot ---------------------------------------------------------------

agg_png(
  here("figures", "fig1.png"),
  width = 80,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig1)
dev.off()

agg_tiff(
  here("figures", "fig1.tiff"),
  width = 80,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig1)
dev.off()
