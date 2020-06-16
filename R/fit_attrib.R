#' fit_attrib
#' @param data x
#' @param outcome x
#' @param exposures a
#' @export
fit_attrib <- function(
                       data,
                       outcome = "deaths",
                       exposures =
                         list(
                           "influenza" = "linear",
                           "temperature" = "linear"
                         )) {
  stopifnot(is.list(exposures))
  for (i in seq_along(exposures)) {
    stopifnot(exposures[[i]] == "linear")
  }

  expo <- c()
  for (i in seq_along(exposures)) {
    if (exposures[[i]] == "linear") {
      expo <- c(expo, names(exposures)[i])
    }
  }
  expo <- glue::glue_collapse(expo, sep = "+")
  formula <- glue::glue("{outcome} ~ {expo}")

  fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data)

  return(fit)
}
