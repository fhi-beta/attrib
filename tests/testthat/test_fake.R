context("attrib")

test_that("Model fit", {

  n_features <- 6 # incl intercept
  rep = 2

  pb <- txtProgressBar(min = 0, max = rep, style = 3)
  retval <- vector("list", length= rep)
  formula <- "deaths ~
  (1|location_code) +
  temperature_high +
  pr100_ili_lag_1 +
  (pr100_ili_lag_1|season) +
  pr100_covid19_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52) +
  offset(log(pop))"

  for(i in 1:rep){
    setTxtProgressBar(pb, i)
    data <- gen_fake_attrib_data()
    fit <- fit_attrib(
      data = data,
      formula = formula)
    temp <- colMeans(x=coef(fit)$season, na.rm = TRUE)
    temp = as.data.frame(temp)
    temp$var = row.names(temp)
    retval[[i]] <- temp
  }
  retval <- rbindlist(retval)

  results <- retval[, .(
    temp = mean(temp)
    ), keyby = .(
      var
  )]

###########################
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
#################################

  testthat::expect_equal(
    round(as.numeric(results$temp, 0)),
    c(-9, 0, 10, 0, 0, 0)                        #OBSOBS COEFICIANT DEPENDENT!!!!! #obsobs not sure if the oreder will always be the same when using the keyby..
  )
})

test_that("Attributable numbers", {

  # generate data
  data <- gen_fake_attrib_data()
  formula <- "deaths ~
  (1|location_code) +
  splines::ns(temperature, knots = 3) +
  pr100_ili_lag_1 +
  (pr100_ili_lag_1|season) +
  pr100_covid19_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52) +
  offset(log(pop))"

  # fit initial model
  fit <- fit_attrib(
    data = data,
    formula = formula)

  exposures = list("pr100_ili_lag_1" =  0  ,"temperature" = 12, "pr100_covid19_lag_1" = 0)
  data <- est_attrib(fit, data, exposures = exposures )

  data[,.(attrib_pr100_ili_lag_1 = sum(attr_pr100_ili_lag_1),
               attrib_pr100_covid19_lag_1 = sum(attr_pr100_covid19_lag_1),
               attrib_heatwave = sum(temperature)), keyby=.(season, location_code)]

  # verify that your model is giving you results like you expect
  #influenza
  testthat::expect_equal(sum(data$attr_pr100_ili_lag_1 < 0), 0)

  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$attr_pr100_ili_lag_1),
    sum(data[week >= 40 | week <= 20]$attr_pr100_ili_lag_1)
  )

  #heat_wave
  testthat::expect_equal(sum(data$attr_temperature_high < 0), 0)

  # is winter #må endres
  #testthat::expect_equal(sum(est_is_winter < 0), 0)

  # covid19
  testthat::expect_equal(sum(data$attr_pr100_covid19 < 0), 0)

  #general expect more deaths during wintern no mather the cause
  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$deaths),
    sum(data[week >= 40 | week <= 20]$deaths)
  )

})
