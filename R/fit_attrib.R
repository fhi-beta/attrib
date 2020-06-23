#' fit_attrib
#' @param data x
#' @param formula x
#' @export
fit_attrib <- function(
                       data,
                       formula){
  #                      outcome = "deaths",
  #                      exposures =
  #                        list(
  #                          "temperature_high" = "linear", #is it corect to call it this?
  #                          "temperature_low" = "linear",
  #                          "pr100_ili_lag" = "linear",
  #                          "pr100_ili_lag" = "linear_season",
  #                          "pr100_ili_lag_1" = "linear",
  #                          "pr100_ili_lag_1" = "linear_season",
  #                          "pr100_ili_lag_2" = "linear",
  #                          "pr100_ili_lag_2" = "linear_season",
  #                          "is_winter" = "linear",
  #                          "pr100_covid19" = "linear",
  #                          "pop" = "offset",
  #                          "location_code" = "location", #what is the correct name?
  #                        ),
  #                      random_effects = list(
  #                        "pr100_ili_lag" = "season"
  #                      )) {
  # stopifnot(is.list(exposures))
  # for (i in seq_along(exposures)) {
  #   stopifnot(exposures[[i]] %in% c("dummy_location","linear", "linear_season", "factor", "binary", "offset"))
  # }
  # expo <- c()
  # for (i in seq_along(exposures)) {
  #   if (exposures[[i]] == "linear") {
  #     expo <- c(expo, names(exposures)[i])
  #   } else if (exposures[[i]] == "linear_season"){
  #     expo <- c(expo, names(exposures)[i])
  #
  #     name <- names(exposures)[i]
  #     location_intercept <- glue::glue("({name}|season)")
  #     expo <- c(expo, location_intercept)
  #   }else if (exposures[[i]] == "spline") {
  #     temp <- glue::glue("splines::ns({names(exposures)[i]}, df=3)")   #not used for anyting
  #     expo <- c(expo, temp)
  #   } else if  (exposures[[i]] == "offset"){
  #     name <- names(exposures)[i]
  #     offset <- glue::glue("offset(log({name}))")
  #     expo <- c(expo, offset)
  #   } else if (exposures[[i]] == "dummy_location"){
  #     name <- names(exposures)[i]
  #     location_intercept <- glue::glue("(1|{name})")
  #     expo <- c(expo, location_intercept)
  #   }
  # }
  # expo <- glue::glue_collapse(expo, sep = "+")
  # formula <- glue::glue("{outcome} ~ {expo}")

  fit <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)

  return(fit)
}
