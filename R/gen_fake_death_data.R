#' Fake data for mortality in Norway
#'
#' @format
#' \describe{
#' \item{location_code}{Location code of the Norwegian municipalities}
#' \item{week}{Week}
#' \item{season}{Season used for influenza like illnesses}
#' \item{yrwk}{Year and week}
#' \item{x}{Number of weeks from the start of the season}
#' \item{pop}{Population size}
#' \item{pr100_ili}{Per hundred ILI, percentage of consultations diagnosed as influenza like illnesses}
#' \item{pr100_ili_lag_1}{pr100_ili_lag_1}
#' \item{temperature}{ temperature}
#' \item{temperature_high}{temperature_high}
#' \item{deaths}{deaths}
#' }
"data_fake_county"


# Generates fake data
#
# This function generates a fake dataset with parameters
# @param n_locations Telling how many locations one wants in the output data, default = 11 the number of municipalities in Norway.

gen_fake_death_data <- function() {
  DoE <- NULL
  DoR <- NULL
 
  
  
  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2020-01-01")
  
  
  skeleton <- expand.grid(
    count = seq(1, 150, by = 1),
    date = seq.Date(
      from = start_date,
      to = end_date,
      by = 1 # to get a weakly base.
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)
  
  skeleton[, DoE := date]
  skeleton[, reg_lag := stats::rpois(.N, 28)]
  skeleton[, DoR := DoE + reg_lag]
  
  
 
  return(skeleton[,.(DoE, DoR)])
}
