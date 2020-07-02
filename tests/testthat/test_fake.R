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
  data_one <- data[1]
  data <- est_attrib(fit, data, exposures = exposures )

  data[,.(attrib_pr100_ili_lag_1 = mean(attr_pr100_ili_lag_1),
               attrib_pr100_covid19_lag_1 = mean(attr_pr100_covid19_lag_1),
               attrib_heatwave = mean(temperature)), keyby=.(season, location_code, id)]

  # verify that your model is giving you results like you expect
  #influenza
  testthat::expect_gt(mean(data$attr_pr100_ili_lag_1), 0) # denne ufngerer ikke lenger men det er sikkert greit i snitt

  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$attr_pr100_ili_lag_1),
    sum(data[week >= 40 | week <= 20]$attr_pr100_ili_lag_1)
  )

  #heat_wave
  #testthat::expect_gt(mean(data$attr_temperature), 0) # thisi is now for temperature not heatwaves!! 

  # is winter #må endres
  #testthat::expect_equal(sum(est_is_winter < 0), 0)

  # covid19
  testthat::expect_gt(mean(data$attr_pr100_covid19), 0)

  #general expect more deaths during wintern no mather the cause
  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$deaths),
    sum(data[week >= 40 | week <= 20]$deaths)
  )

})


test_that("irr", {
  
  #### THIS IS THE GOOD ONE
  # generate data
  set.seed(40)
  data <- gen_fake_attrib_data()
  
  # take in the fixed effects
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
  
  fit <- fit_attrib(data, fixef = fixef, ranef = ranef)
  
  est_mean_data <- est_mean(fit, data)
  

  #newdata <- data[10:1000,]

  # predict mean
  pred <- exp(lme4:::predict.merMod(fit, data))
  est_mean <- unique(est_mean_data$sim_mean)

  dif_mean <- pred - est_mean  #apply(expected,2,median)
  mean(dif_mean)
  median(dif_mean)
  
  dif_obs <- est_mean_data$deaths - est_mean #apply(expected,2,median)
  mean(dif_obs)
  median(dif_obs)
  
  dif <- newdata$deaths - pred
  median(dif)
  
  # median(apply(expected,2,mean))
  # 
  # # look at what we get directly from the model
  # summary(fit)
  # exp(0.0289)
  # exp(0.0289 - 1.96*0.004067)
  # exp(0.0289 + 1.96*0.004067)
  # 
  # # IRR
  # quantile(expected[,2]/expected[,1], probs=c(0.025, 0.5, 0.975))
  # 
  # # absolute numbers
  # quantile(expected[,2]-expected[,1], probs=c(0.025, 0.5, 0.975))
  # 
  # # confirm that "simulating random effects" isn't doing anything weird
  # # i.e. we see that the means of the simulations are correct
  # lme4::ranef(fit)
  # apply(x@ranef$season[,,"pr100_ili_lag_1"],2,mean)
  # lme4::ranef(fit)$season[,2] - apply(x@ranef$season[,,"pr100_ili_lag_1"],2,mean)
})