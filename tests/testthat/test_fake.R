context("attrib")

test_that("Model fit", {

  # generate data
  data <- gen_fake_attrib_data()
  formula <- "deaths ~ (1|location_code) + temperature_high + pr100_ili_lag_1 + (pr100_ili_lag_1|season) + is_winter + pr100_covid19_lag_1 + offset(log(pop))"

  # fit initial model
  fit <- fit_attrib(
    data = data,
    formula = formula)

  #   outcome = "deaths",
  #   exposures = list(
  #     "temperature_high" = "factor", #is it corect to call it this?
  #     "temperature_low" = "factor",
  #     #"pr100_ili" = "linear",
  #     "pr100_ili_lag_1" = "linear_season",
  #     "is_winter" = "binary",
  #     "pr100_covid19_lag_1" = "linear",
  #     "pop" = "offset"
  #   )
  # )
 #compleatly wrong for now
  testthat::expect_equal(
    round(as.numeric(coef(fit)), 0),
    c(-9, 0, 0, 0, 0, 8)                        #OBSOBS COEFICIANT DEPENDENT!!!!!
  )
})

test_that("Attributable numbers", {

  # generate data
  data <- gen_fake_attrib_data()

  # fit initial model
  fit <- fit_attrib(
    data = data,
    outcome = "deaths",
    exposures = list(
      "location_code" = "location",
      "temperature_high" = "linear",
      "temperature_low" = "linear",
      "pr100_ili" = "linear",
      "is_winter" = "linear",
      "pr100_covid19" = "linear",
      "pop" = "offset"
    )
  )

  # create reference datasets
  data_observed <- copy(data)

  data_reference_pr100_ili <- copy(data)
  data_reference_pr100_ili[, pr100_ili := 0]

  data_reference_temperature_high <- copy(data)
  data_reference_temperature_high[, temperature_high := 0]

  data_reference_temperature_low <- copy(data)
  data_reference_temperature_high[, temperature_low := 0]

  data_reference_is_winter <- copy(data)
  data_reference_is_winter[, is_winter := 0]

  data_reference_pr100_covid19 <- copy(data)
  data_reference_pr100_covid19[, pr100_covid19 := 0]

  # estimate attrib
  est_pr100_ili <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_pr100_ili
  )

  est_temperature_high <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_temperature_high
  )

  est_temperature_low <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_temperature_low
  )

  est_is_winter <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_is_winter
  )

  est_pr100_covid19 <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_pr100_covid19
  )

  data[, attrib_pr100_ili := est_pr100_ili]
  data[, attrib_temperature_high := est_temperature_high]
  data[, attrib_temperature_low := est_temperature_low]
  data[, attrib_is_winter := est_is_winter]
  data[, attrib_pr100_covid19 := est_pr100_covid19]

  # verify that your model is giving you results like you expect
  #influenza
  testthat::expect_equal(sum(est_pr100_ili < 0), 0)

  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$attrib_pr100_ili),
    sum(data[week >= 40 | week <= 20]$attrib_pr100_ili)
  )

  #heat_wave
  testthat::expect_equal(sum(est_temperature_high < 0), 0)

  #cold wave
  testthat::expect_equal(sum(est_temperature_low < 0), 0)

  # is winter
  testthat::expect_equal(sum(est_is_winter < 0), 0)

  # covid19
  testthat::expect_equal(sum(est_pr100_covid19 < 0), 0)

  #general expect more deaths during wintern no mather the cause
  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$deaths),
    sum(data[week >= 40 | week <= 20]$deaths)
  )

})
