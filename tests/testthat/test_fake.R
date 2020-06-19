context("attrib")

test_that("Model fit", {

  # generate data
  data <- gen_fake_attrib_data()

  # fit initial model
  fit <- fit_attrib(
    data = data,
    outcome = "deaths",
    exposures = list(
      "temperature_high" = "factor", #is it corect to call it this?
      "temperature_low" = "factor",
      "pr100_ili" = "linear",
      "is_winter" = "binary",
      "pop" = "offset"

    )
  )

  testthat::expect_equal(
    round(as.numeric(coef(fit)), 0),
    c(2.0, 0.0, 2.0)                        #OBSOBS COEFICIANT DEPENDENT!!!!!
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
      "influenza" = "linear",
      "temperature" = "linear"
    )
  )

  # create reference datasets
  data_observed <- copy(data)

  data_reference_influenza <- copy(data)
  data_reference_influenza[, influenza := 0]

  data_reference_temperature_high <- copy(data)
  data_reference_temperature_high[, temperature_high := 0]

  data_reference_temperature_low <- copy(data)
  data_reference_temperature_high[, temperature_low := 0]

  data_reference_is_winter <- copy(data)
  data_reference_is_winter[, is_winter := 0]

  # estimate attrib
  est_influenza <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_influenza
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

  data[, attrib_influenza := est_influenza]
  data[, attrib_temperature_high := est_temperature_high]

  # verify that your model is giving you results like you expect
  #influenza
  testthat::expect_equal(sum(est_influenza < 0), 0)
  testthat::expect_gt(sum(exp((data[year == 2010]$influenza)*0.000004)),600)       #OBSOBS COEFFICIANT DEPENDENT
  testthat::expect_lt(sum(exp((data[year == 2010]$influenza)*0.000004)),1200)

  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$attrib_influenza),
    sum(data[week >= 40 | week <= 20]$attrib_influenza)
  )

  #heat_wave
  testthat::expect_equal(sum(est_temperature_high < 0), 0)

  #cold wave
  testthat::expect_equal(sum(est_temperature_low < 0), 0)

  # is winter
  testthat::expect_equal(sum(est_is_winter < 0), 0)

})
