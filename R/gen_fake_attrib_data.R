#' Fake data for mortality in Norway
#'
#' libraries. This data is licensed under specific conditions noted in the details section.
#'
#' The permission to use the data is granted on condition that:
#' \itemize{
#' \item The data will not be used for commercial purposes
#' \item The source will be acknowledged. A copyright notice will have to be visible on any printed or electronic publication using this dataset
#' }
#'
#' @format
#' \describe{
#' \item{location_code}{Location code of the norwegian municipalities}
#' \item{week}{Week}
#' \item{season}{Season used for influenza like ilnesses}
#' \item{date}{Date}
#' \item{yrwk}{Year and week}
#' \item{x}{Number of weeks from the start of the season}
#' \item{pop}{Population size}
#' \item{pr100_ili}{Pr hundered ILI, precentage og consultations diagnised as influenza like illnesses}
#' \item{pr100_ili_lag_1}{pr100_ili_lag_1}
#' \item{temperature}{ temperature}
#' \item{temperature_high}{temperature_high}
#' \item{deaths}{deaths}
#' }

"data_fake"



# Generates fake attributable data
#
# This function generates one dataset
# @param n_locations Telling how many locations one watns in the output data

gen_fake_attrib_data <- function(n_locations = 11 ) {
  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2020-12-31")

  yrwk <- NULL
  x <- NULL
  season <- NULL
  level <- NULL
  . <- NULL
  pop <- NULL
  peak_center_influenza <- NULL
  hight_peak <- NULL
  influenza_coef <- NULL
  normal_base <- NULL
  pr100_ili <- NULL
  pr100_ili_lag_1 <- NULL
  pr100_ili_lag_2 <- NULL
  mean_temperature <- NULL
  temperature <- NULL
  temperature_high<- NULL
  temperature_spline_1 <- NULL
  temperature_spline_2 <- NULL
  temperature_spline_3 <- NULL
  mu <- NULL
  deaths <- NULL




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
  skeleton_season[, peak_center_influenza := round(stats::rnorm(.N, mean = 28, sd = 3 ))  ]
  skeleton_season[, hight_peak := stats::rnorm(.N, mean = 2, sd = 0.02) ]
  skeleton_season[, influenza_coef := stats::rnorm(.N, mean = 0.03, sd = 0.02) ]

  skeleton <- merge(
    skeleton,
    skeleton_season,
    by=c("location_code", "season")
  )

  skeleton[, normal_base := stats::dnorm(x, peak_center_influenza, 5)]
  skeleton[, pr100_ili := 10*1.2*hight_peak * normal_base]            #something strange doing on her but this gives pr100ili around 2
  skeleton[pr100_ili< 0, pr100_ili := 0]  #should there be some more randomness here??

  skeleton[, pr100_ili_lag_1 := shift(pr100_ili, fill = 0), by = c("location_code")]
  skeleton[, pr100_ili_lag_2 := shift(pr100_ili, n= 2L, fill = 0), by = c("location_code")]

  # temperature

  skeleton_weeks_temp <- unique(skeleton[,c("location_code", "week")])
  skeleton_weeks_temp[, mean_temperature := (26 - abs((week- 26)))]
  skeleton_weeks_temp[, mean_temperature := c(skeleton_weeks_temp[(.N-4):.N]$mean_temperature, skeleton_weeks_temp[1:(.N-5)]$mean_temperature) - 5]

  skeleton <- merge(
    skeleton,
    skeleton_weeks_temp,
    by = c("location_code","week")
  )
  skeleton[, temperature := stats::rnorm(
    n = .N,
    mean = mean_temperature,              #temperature span between -5,20 on average
    sd = 5
  )]

  skeleton[, temperature_high := 0]
  skeleton[temperature > 20 , temperature_high := stats::rbinom(.N, 7, 0.2)]

  # skeleton[, temperature_spline_1 := splines::ns(skeleton$temperature, df=3)[,1]]
  # skeleton[, temperature_spline_2 := splines::ns(skeleton$temperature, df=3)[,2]]
  # skeleton[, temperature_spline_3 := splines::ns(skeleton$temperature, df=3)[,3]]

  #remove covid to pas the tests
  # # covid-19
  # skeleton[, pr100_covid19:= 0]
  # skeleton[date>="2020-03-01", pr100_covid19:= rnorm(.N, mean = 0.0042, sd = 0.001)]
  # skeleton[, pr100_covid19_lag_1 := shift(pr100_covid19, fill = 0), by = c("location_code")]
  # skeleton[, pr100_covid19_lag_2 := shift(pr100_covid19, n= 2L, fill = 0), by = c("location_code")]


  # generate deaths

  skeleton[, mu := exp(-8.8 +
                         0.08 * temperature_high +
                         #0.25*influenza_coef * pr100_ili +
                         influenza_coef * pr100_ili_lag_1 +
                         #10*pr100_covid19_lag_1 +
                         0.02*sin(2 * pi * (week - 1) / 52) + 0.07*cos(2 * pi * (week - 1) / 52)+ #finn a og b
                         #1*pr100_ili_lag_2 +
                         log(pop))]


  skeleton[, deaths := stats::rpois(n = .N, lambda = mu)]


  # fit <- lme4::glmer(deaths ~ (1|location_code) +
  #                      #splines::ns(skeleton$temperature, df=3) +
  #                      temperature_high +
  #                      pr100_ili_lag_1 +
  #                      (pr100_ili_lag_1|season) +
  #                      sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+
  #                      offset(log(pop)),
  #                    data = skeleton,
  #                    family = "poisson")
  # summary(fit)
  #
  #
  # death_tot <- skeleton[, .(
  #   death = sum(deaths),
  #   year
  # ), keyby = .(
  #   date
  # )]
  # min(death_tot$death)
  # max(death_tot$death)
  #get unique loctation codes, return n first.

  # fake_data_colums <- c("location_code", "week", "season", "date", "year", "yrwk", "x", "pop", "pr100_ili", "pr100_ili_lag_1", "temperature", "temperature_high", "deaths")
  # data_fake <- skeleton[, ..fake_data_colums]
  # save(data_fake, file = "data/data_fake.rda", compress = "bzip2")

  locations <- unique(skeleton$location_code)
  locations_current <- locations[1:n_locations]
  return(skeleton[location_code %in% locations_current])
}
