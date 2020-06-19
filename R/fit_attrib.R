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
                           "temperature_high" = "factor", #is it corect to call it this?
                           "temperature_low" = "factor",
                           "pr100_ili" = "linear",
                           "is_winter" = "binary",
                           "pop" = "offset"
                         )) {
  stopifnot(is.list(exposures))
  for (i in seq_along(exposures)) {
    stopifnot(exposures[[i]] %in% c("linear", "factor", "binary", "offset"))
  }

  expo <- c()
  for (i in seq_along(exposures)) {
    if (exposures[[i]] == "linear") {
      expo <- c(expo, names(exposures)[i])
    } else if (exposures[[i]] == "spline") {
      temp <- glue::glue("splines::ns({names(exposures)[i]}, df=3)")
      expo <- c(expo, temp)
    } else if  (exposures[[i]] == "binary"){                         #is this correct?
      expo <- c(expo, names(exposures)[i])
    } else if  (exposures[[i]] == "factor"){                         #is this correct?
      expo <- c(expo, names(exposures)[i])
    } else if  (exposures[[i]] == "offset"){
      offset <- glue::glue("offset({names(exposures)[i]})")
      expo <- c(expo, offset)
    }
  }
  expo <- glue::glue_collapse(expo, sep = "+")
  formula <- glue::glue("{outcome} ~ {expo}")

  fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data)

  return(fit)
}
