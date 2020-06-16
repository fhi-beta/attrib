#' Generates fake attributable data
#'
#' This function generates one dataset
#' @export
gen_fake_attrib_data <- function(){
  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2020-12-31")

  location_code <- "norge"

  skeleton <- expand.grid(
    location_code = location_code,
    date = seq.Date(
      from = start_date,
      to = end_date,
      by=1
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  skeleton[, year := fhi::isoyear_n(date)]
  skeleton[, week := fhi::isoweek_n(date)]
  skeleton[, x := fhi::x(week)]

  x_pop <- fhidata::norway_population_b2020[,.(
    pop = sum(pop)
  ), keyby=.(
    year,
    location_code
  )]
  skeleton[
    x_pop,
    on=c("year","location_code"),
    pop := pop
  ]


  # influenza only happens during weeks 40 -> 20
  skeleton[, influenza := rpois(.N, lambda = 50)]
  skeleton[week >= 40 | week <= 20, influenza := rpois(.N, lambda = 500)]

  # temperature
  skeleton[, temperature := rnorm(
    n=.N,
    mean = 1*(6-abs(lubridate::month(date)-6)),
    sd = 5
  )]

  # generate deaths
  skeleton[, mu := exp(3 + 0.1 * temperature + 0.002 * influenza)]
  skeleton[, deaths := rpois(n=.N, lambda = mu)]

  # test with poisson regression
  fit <- glm(deaths ~ temperature + influenza, data=skeleton, family = "poisson")
  summary(fit)

  return(skeleton)
}
