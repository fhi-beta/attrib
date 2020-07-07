context("attrib")

test_that("Model fit", {

  n_features <- 6 # incl intercept
  rep = 10

  pb <- txtProgressBar(min = 0, max = rep, style = 3)
  retval <- vector("list", length= rep)
  # formula <- "deaths ~
  # (1|location_code) +
  # temperature_high +
  # pr100_ili_lag_1 +
  # (pr100_ili_lag_1|season) +
  # pr100_covid19_lag_1 +
  # sin(2 * pi * (week - 1) / 52) +
  # cos(2 * pi * (week - 1) / 52) +
  # offset(log(pop))"
  
  fixef <- "deaths ~
  splines::ns(temperature, df=3) +
  pr100_ili_lag_1 +
  pr100_covid19_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52) +
  offset(log(pop))"
  
  # take in the random effects
  ranef <- "(1|location_code) +
  (pr100_ili_lag_1|season)"

  for(i in 1:rep){
    setTxtProgressBar(pb, i)
    
    data <- gen_fake_attrib_data(4)
    suppressWarnings(
      fit <- fit_attrib(
        data = data,
        fixef = fixef,
        ranef = ranef)
    )
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
    c(-9, 0, 10, 0, 0, 0, 0, 0)                        #OBSOBS COEFICIANT DEPENDENT!!!!! #obsobs not sure if the oreder will always be the same when using the keyby..
  )
})

test_that("Attributable numbers", {

  # generate data
  data <- gen_fake_attrib_data(2)
  
  fixef <- "deaths ~
  splines::ns(temperature, df=3) +
  pr100_ili_lag_1 +
  pr100_covid19_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52) +
  offset(log(pop))"
  
  # take in the random effects
  ranef <- "(1|location_code) +
  (pr100_ili_lag_1|season)"
  
  # formula <- "deaths ~
  # (1|location_code) +
  # splines::ns(temperature, knots = 3) +
  # pr100_ili_lag_1 +
  # (pr100_ili_lag_1|season) +
  # pr100_covid19_lag_1 +
  # sin(2 * pi * (week - 1) / 52) +
  # cos(2 * pi * (week - 1) / 52) +
  # offset(log(pop))"

  # fit initial model
  suppressWarnings(
    fit <- fit_attrib(
      data = data,
      fixef = fixef,
      ranef = ranef)
  )
  exposures = list("pr100_ili_lag_1" =  0  ,"temperature" = 7, "pr100_covid19_lag_1" = 0)
  #data_one <- data[1]
  data <- est_attrib(fit, data, exposures = exposures )

  data_copy <- copy(data)
  data_copy <-data_copy[,.(attr_pr100_ili_lag_1 = median(exp_mort_observed - `exp_mort_pr100_ili_lag_1=0`),
               attr_pr100_covid19_lag_1 = median(exp_mort_observed - `exp_mort_pr100_covid19_lag_1=0`),
               attr_heatwave = median(exp_mort_observed - `exp_mort_temperature=7`)), 
          keyby=.(season, location_code, id, week)]

  # verify that your model is giving you results like you expect
  #influenza
  testthat::expect_gt(mean(data_copy$attr_pr100_ili_lag_1), 0) # denne ufngerer ikke lenger men det er sikkert greit i snitt

  testthat::expect_lt(
    sum(data_copy[week >= 21 & week <= 39]$attr_pr100_ili_lag_1),
    sum(data_copy[week >= 40 | week <= 20]$attr_pr100_ili_lag_1)
  )

  #heat_wave
  #testthat::expect_gt(mean(data$attr_temperature), 0) # thisi is now for temperature not heatwaves!! 

  # is winter #må endres
  #testthat::expect_equal(sum(est_is_winter < 0), 0)

  # covid19
  testthat::expect_gt(mean(data_copy$attr_pr100_covid19), 0)

  #general expect more deaths during wintern no mather the cause
  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$deaths),
    sum(data[week >= 40 | week <= 20]$deaths)
  )

})


test_that("simmulations", {

  set.seed(40)
  data <- gen_fake_attrib_data(2)
  
  # take in the fixed effects
  fixef <- "deaths ~
  splines::ns(temperature, df=3) +
  pr100_ili_lag_1 +
  pr100_covid19_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52) +
  offset(log(pop))"
  
  # take in the random effects
  # ranef <- "(1|location_code) +
  # (pr100_ili_lag_1|season)"
  # 
  ranef <- "(pr100_ili_lag_1|season)"
  
  
  suppressWarnings(
    fit <- fit_attrib(data, fixef = fixef, ranef = ranef)
  )
  
  exposures <- list("pr100_ili_lag_1" = 0, "pr100_covid19_lag_1" = 0, "temperature" = 7)
  est_mort <- est_attrib(fit, data, exposures, response = "deaths")

  # predict mean
  pred <- exp(lme4:::predict.merMod(fit, data))
  est_mean <- est_mort[,.(exp_mort_median = median(exp_mort_observed)), keyby = .(id, location_code, week, season, yrwk, pop, deaths)]

  dif_mean <- pred - est_mean$exp_mort_median  #apply(expected,2,median)
  mean(dif_mean)
  median(dif_mean)
  
  testthat::expect_equal(
    round(as.numeric(median(dif_mean), 0)),
    c(0)
  )
  
  dif_obs <- est_mean$deaths - est_mean$exp_mort_median #apply(expected,2,median)
  mean(dif_obs)
  median(dif_obs)
  
  testthat::expect_equal(
    round(as.numeric(median(dif_obs), 0)),
    c(0)
  )
  
  dif <- est_mean$deaths - pred
  median(dif)
  
  testthat::expect_equal(
    round(as.numeric(median(dif), 0)),
    c(0)
  )

})
