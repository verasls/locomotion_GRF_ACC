# Build regression formula table
#
# Params:
#    model: A list of length 3 with the lmerMod model objects.
#    cv: A list of length 3 with the lvmisc_cv objects.
#    outcome: A character vector with the model outcome. Either "pGRF"
# or "pLR".
#    vector: A character vector with the vector used in the model. Either
# "resultant" or "vertical".
#
# Returns:
#    A tibble with the regression formula table.
build_formula_table <- function(model, cv, outcome, vector) {
  model_accuracy <- purrr::map(cv_res_GRF_models, accuracy, na.rm = TRUE)
  R2 <- unname(purrr::map_dbl(model_accuracy, "R2_cond"))
  MAE <- unname(purrr::map_dbl(model_accuracy, "MAE"))
  MAPE <- unname(purrr::map_dbl(model_accuracy, "MAPE"))
  RMSE <- unname(purrr::map_dbl(model_accuracy, "RMSE"))

  tibble::tibble(
    "Accelerometer placement" = c("Ankle", "Lower back", "Hip"),
    "Regression equations" = unname(
      purrr::map_chr(model, get_equation, outcome, vector)
    ),
    "R2" = R2, "MAE" = MAE, "MAPE" = MAPE, "RMSE" = RMSE
  )
}

# Get regression equation formula with coefficients and variables
#
# Params:
#    model: The lmerMod model object.
#    outcome: A character vector with the model outcome. Either "pGRF"
# or "pLR".
#    vector: A character vector with the vector used in the model. Either
# "resultant" or "vertical".
#
# Returns:
#    A character vector with the model equation.
get_equation <- function(model, outcome, vector) {
  model_coefs <- as.character(round(coef(summary(model))[, 1], 3))
  model_coefs[1] <- ifelse(
    model_coefs[1] > 0,
    model_coefs[1],
    paste0("- ", abs(as.numeric(model_coefs[1])))
  )
  model_coefs[-1] <- ifelse(
    as.numeric(model_coefs[-1]) > 0,
    paste0(" + ", abs(as.numeric(model_coefs[-1]))),
    paste0(" - ", abs(as.numeric(model_coefs[-1])))
  )

  if (stringr::str_detect(outcome, "GRF") & vector == "resultant") {
    model_outcome <- "pRGRF (N)"
    model_acceleration <- "pRACC"
  } else if (stringr::str_detect(outcome, "GRF") & vector == "vertical") {
    model_outcome <- "pVGRF (N)"
    model_acceleration <- "pVACC"
  } else if (stringr::str_detect(outcome, "LR") & vector == "resultant") {
    model_outcome <- "pVLR (Ns)"
    model_acceleration <- "pRATR"
  } else if (stringr::str_detect(outcome, "LR") & vector == "resultant") {
  } else if (stringr::str_detect(outcome, "LR") & vector == "vertical") {
    model_outcome <- "pVLR (Ns)"
    model_acceleration <- "pVATR"
  }

  regression_equation <- glue::glue(
    "{model_outcome} = {model_coefs[1]}{model_coefs[2]}({model_acceleration})\\
    {model_coefs[3]}(body mass)\\
    {model_coefs[4]}({model_acceleration} x body mass)"
  )
  as.character(regression_equation)
}

# Bland-Altman plot regression
#
# Runs a linear regression to test whether the difference of the actual and
# predicted values are explained by their mean.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#    The lm object summary.
bland_altman_regression <- function(cv) {
  data <- get_bland_altman_data(cv)
  summary(stats::lm(diff ~ mean, data))
}

# Bland-Altman plot t-test
#
# Runs a one-sample t-test on the bias value, to test whether it differs
# from 0.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#   The htest object summary.
bland_altman_t_test <- function(cv) {
  data <- get_bland_altman_data(cv)
  t.test(data$diff, mu = 0)
}

# Get Bland-Altman plot data.
#
# Computes the element-wise difference between the  actual and predicted values
# and their mean.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#  A tibble with two columns: diff and mean.
get_bland_altman_data <- function(cv) {
  dplyr::transmute(
    cv,
    diff = .actual - .predicted,
    mean = (.actual + .predicted) / 2
  )
}
