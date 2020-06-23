#' Generates fake attributable data
#'
#' This function generates one dataset
#' @export
gen_fake_attrib_data <- function() {
  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2020-12-31")

  location_code <- unique(fhidata::norway_locations_b2020$county_code)

  skeleton <- expand.grid(
    location_code = location_code,
    date = seq.Date(
      from = start_date,
      to = end_date,
      by = 7                                    #to get a weakly base.
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  skeleton[, year := fhi::isoyear_n(date)]
  skeleton[, week := fhi::isoweek_n(date)]
  skeleton[, yrwk := fhi::isoyearweek(date)]
  skeleton[, x := fhi::x(week)]
  skeleton[, season := fhi::season(yrwk)]

  x_pop <- fhidata::norway_population_b2020[level == "county", .(
    pop = sum(pop)
  ), keyby = .(
    year,
    location_code
  )]
  skeleton[
    x_pop,
    on = c("year", "location_code"),
    pop := pop
  ]

  ######## seasonbased influenza
  #### still without any lag.
  skeleton_season <- unique(skeleton[,c("location_code", "season")])
  skeleton_season[, peak_center_influenza := round(rnorm(.N, mean = 28, sd = 3 ))  ]
  skeleton_season[, hight_peak := rnorm(.N, mean = 2, sd = 0.02) ]
  skeleton_season[, influenza_coef := rnorm(.N, mean = 0.3, sd = 0.02) ]

  skeleton <- merge(
    skeleton,
    skeleton_season,
    by=c("location_code", "season")
  )

  skeleton[, normal_base := dnorm(x, peak_center_influenza, 5)]
  skeleton[, pr100_ili := 1.2*hight_peak * normal_base]
  skeleton[pr100_ili< 0, pr100_ili := 0]  #should there be some more randomness here??

  skeleton[, pr100_ili_lag_1 := shift(pr100_ili, fill = 0), by = c("location_code")]
  skeleton[, pr100_ili_lag_2 := shift(pr100_ili, n= 2L, fill = 0), by = c("location_code")]

  # temperature high

  skeleton_weeks_temp <- unique(skeleton[,c("location_code", "week")])
  skeleton_weeks_temp[, mean_temperature := (26 - abs((week- 26)))]
  skeleton_weeks_temp[, mean_temperature := c(skeleton_weeks_temp[(.N-4):.N]$mean_temperature, skeleton_weeks_temp[1:(.N-5)]$mean_temperature) - 5]

  skeleton <- merge(
    skeleton,
    skeleton_weeks_temp,
    by = c("location_code","week")
  )
  skeleton[, temperature := rnorm(
    n = .N,
    mean = mean_temperature,              #temperature span between -5,20 on average
    sd = 5
  )]

  skeleton[, temperature_high := 0]
  skeleton[temperature > 20 , temperature_high := rbinom(.N, 7, 0.2)]

  # temperature low
  # skeleton[, temperature_low:= 0]
  # skeleton[temperature < -10 , temperature_low := rbinom(.N, 7, 0.2)]        # is -10 an okay threshhold?


  # is winter
  #skeleton[, is_winter:= 0]
  #skeleton[, is_winter:= sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)]                          #same weeks as the flue?

  # covid-19
  skeleton[, pr100_covid19:= 0]
  skeleton[date>="2020-03-01", pr100_covid19:= rnorm(.N, mean = 0.0042, sd = 0.001)]
  skeleton[, pr100_covid19_lag_1 := shift(pr100_covid19, fill = 0), by = c("location_code")]
  skeleton[, pr100_covid19_lag_2 := shift(pr100_covid19, n= 2L, fill = 0), by = c("location_code")]


  # generate deaths
  #set.seed(1234)
  skeleton[, mu := exp(-8.8 +
                         0.05*temperature_high +
                         #0.05 * temperature_low +
                         #0.25*influenza_coef * pr100_ili +
                         influenza_coef * pr100_ili_lag_1 +
                         #0.25*influenza_coef * pr100_ili_lag_2 +
                         #0.1 * is_winter +
                         #1*pr100_covid19 +
                         10*pr100_covid19_lag_1 +
                         3*sin(2 * pi * (week - 1) / 52) + 7*cos(2 * pi * (week - 1) / 52)+ #finn a og b
                         #1*pr100_ili_lag_2 +
                         log(pop))]


  #location dep intercept.
  #skeleton[location_code == "county03", mu := exp(-8.4 + 0.05*temperature_high + 0.05 * temperature_low + influenza_coef * pr100_ili + 0.1 * is_winter + 0.25*pr100_covid19 + log(pop))] # is the intercept location depentent?


  skeleton[, deaths := rpois(n = .N, lambda = mu)]

  # test with poisson regression,
  ##### what should I change here?
  #zero info shaing
  # fit <- glm(deaths ~ location_code + temperature_high + pr100_ili + is_winter + pr100_covid19 + offset(log(pop)), data = skeleton, family = "poisson")           # is it okay to change away from the glm?
  # summary(fit)

  #partial infosharing for intercept
  # fit <- lme4::glmer(deaths ~ (1|location_code) +temperature_high + pr100_ili + is_winter + pr100_covid19 + offset(log(pop)), data = skeleton, family = "poisson")           # is it okay to change away from the glm?
  # summary(fit)

  fit <- lme4::glmer(deaths ~ (1|location_code) +
                       temperature_high +
                       #pr100_ili +
                       pr100_ili_lag_1 +
                       #pr100_ili_lag_2 +
                       #(pr100_ili + pr100_ili_lag_1 + pr100_ili_lag_2|season) +
                       (pr100_ili_lag_1|season) +
                       #is_winter +
                       #pr100_covid19 +
                       pr100_covid19_lag_1 +
                       sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+
                       #pr100_covid19_lag_2 +
                       offset(log(pop)),
                     data = skeleton,
                     family = "poisson")
  summary(fit)


  death_tot <- skeleton[, .(
    death = sum(deaths),
    year
  ), keyby = .(
    date
  )]

  return(skeleton)
}
