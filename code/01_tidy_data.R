# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lvmisc)

# Read anthropometric data ------------------------------------------------

anthropometric <- read_csv(here("data", "anthropometric_data.csv")) %>%
  select(id = ID, everything()) %>%
  mutate(
    BMI = bmi(body_mass, height / 100),
    BMI_cat = bmi_cat(BMI)
  )

# Read running data -------------------------------------------------------

# Data with force and acceleration
mechanical_load_data_wide <- read_xlsx(
  here("data", "GRF_ACC_data_all.xlsx"), sheet = "all"
) %>%
  clean_names() %>%
  select(
    raw_imu, id, trial = visita, filename = nome,
    acc_placement = local_acelerometro,
    speed = velocidade_km_h,
    res_n_peaks = ftot_numero_picos_ftot,
    ver_n_peaks = fvt_numero_picos_fvt,
    pRACC_g = atot_med_picos_atot_g,
    pRGRF_N = ftot_med_picos_ftot_n,
    pRGRF_BW = ftot_med_picos_ftot_bw,
    pVACC_g = avt_med_picos_avt_g,
    pVGRF_N = fvt_med_picos_fvt_n,
    pVGRF_BW = fvt_med_picos_fvt_bw
  ) %>%
  filter(
    raw_imu == "imu" &
    acc_placement %in% c("ankle", "back", "waist")
  ) %>%
  select(id, everything())

mechanical_load_data_res <- mechanical_load_data_wide %>%
  select(
    id, trial, filename, acc_placement, speed,
    n_peaks = res_n_peaks, pACC_g = pRACC_g,
    pGRF_N = pRGRF_N, pGRF_BW = pRGRF_BW
  ) %>%
  mutate(
    vector = "resultant",
    .after = acc_placement
  )

mechanical_load_data_ver <- mechanical_load_data_wide %>%
  select(
    id, trial, filename, acc_placement, speed,
    n_peaks = ver_n_peaks, pACC_g = pVACC_g,
    pGRF_N = pVGRF_N, pGRF_BW = pVGRF_BW
  ) %>%
  mutate(
    vector = "vertical",
    .after = acc_placement
  )

mechanical_load_data <- rbind(
  mechanical_load_data_res, mechanical_load_data_ver
) %>%
  mutate(
    # Create another subj identifier
    subj = paste0(id, "m", trial),
    subj = ifelse(str_length(subj) == 3, paste0("00", subj), subj),
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj)
  ) %>%
  left_join(
    select(
      anthropometric, id, trial, sex, age, height, body_mass, BMI, BMI_cat
    ),
    by = c("id", "trial")
  ) %>%
  select(
    id, trial, filename, subj, body_mass, BMI, BMI_cat, everything()
  )

# Data with force and acceleration rates
mechanical_load_rates_dt_wide <- read_csv(
  here("data", "max_rates_IMU.csv")
) %>%
  clean_names() %>%
  select(
    id, trial = visita, filename = nome, acc_placement = local_acelerometro,
    speed = velocidade_km_h,
    pRAR_gs = max_rates_atot_med_atot_g_s,
    pRLR_Ns = max_rates_ftot_med_ftot_n_s,
    pRLR_BWs = max_rates_ftot_med_ftot_bw,
    pVAR_gs = max_rates_avt_med_avt_g_s,
    pVLR_Ns = max_rates_fvt_med_fvt_n_s,
    pVLR_BWs = max_rates_fvt_med_fvt_bw
  ) %>%
  filter(acc_placement %in% c("ankle", "back", "waist"))

mechanical_load_rates_res <- mechanical_load_rates_dt_wide %>%
  select(
    id, trial, filename, acc_placement, speed,
    pAR_gs = pRAR_gs,
    pLR_Ns = pRLR_Ns, pLR_BWs = pRLR_BWs
  ) %>%
  mutate(
    vector = "resultant",
    .after = acc_placement
  )

mechanical_load_rates_ver <- mechanical_load_rates_dt_wide %>%
  select(
    id, trial, filename, acc_placement, speed,
    pAR_gs = pVAR_gs,
    pLR_Ns = pVLR_Ns, pLR_BWs = pVLR_BWs
  ) %>%
  mutate(
    vector = "vertical",
    .after = acc_placement
  )

mechanical_load_rates_df <- rbind(
  mechanical_load_rates_res, mechanical_load_rates_ver
) %>%
  mutate(
    # Create another subj identifier
    subj = paste0(id, "m", trial),
    subj = ifelse(str_length(subj) == 3, paste0("00", subj), subj),
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj)
  ) %>%
  left_join(
    select(
      anthropometric, id, trial, sex, age, height, body_mass, BMI, BMI_cat
    ),
    by = c("id", "trial")
  ) %>%
  select(
    id, trial, filename, subj, body_mass, BMI, BMI_cat, everything()
  )

# Join both data frames
mechanical_load_data <- mechanical_load_data %>%
  left_join(mechanical_load_rates_df) %>%
  mutate(
    acc_placement = recode(
      as.factor(acc_placement),
      "ankle" = "ankle",
      "back" = "lower_back",
      "waist" = "hip"
    ),
    vector = fct_relevel(as.factor(vector), "vertical"),
    speed = as.factor(speed)
  )

save(mechanical_load_data, file = here("data", "mechanical_load_data.rda"))
