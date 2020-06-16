context("attrib")

test_that("Model fit", {

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

  testthat::expect_equal(
    round(as.numeric(coef(fit)), 0),
    c(3.0, 0.0, 0.0)
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

  data_reference_temperature <- copy(data)
  data_reference_temperature[, temperature := 0]

  # estimate attrib
  est_influenza <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_influenza
  )

  est_temperature <- est_attrib(
    fit = fit,
    data_observed = data_observed,
    data_reference = data_reference_temperature
  )

  data[, attrib_influenza := est_influenza]
  data[, attrib_temperature := est_temperature]

  # verify that your model is giving you results like you expect

  testthat::expect_equal(sum(est_influenza < 0), 0)

  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$attrib_influenza),
    sum(data[week >= 40 | week <= 20]$attrib_influenza)
  )
})
