#' Estimates attributable mortality
#'
#' @param fit A model fit
#' @param data_observed The observed data
#' @param data_reference The data with reference values
#' @export
est_attrib <- function(
                       fit,
                       data_observed,
                       data_reference) {
  pred_observed <- predict(fit, data_observed, type = "response")
  pred_reference <- predict(fit, data_reference, type = "response")

  pred_attrib <- pred_observed - pred_reference

  return(pred_attrib)
}
