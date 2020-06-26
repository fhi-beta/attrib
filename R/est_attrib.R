#' Estimates attributable mortality
#'
#' @param fit A model fit
#' @param data The observed data
#' @param exposures The exposures that will be given attributable deaths
#' @export
# est_attrib <- function(
#                        fit,
#                        data_observed,
#                        data_reference) {
#   pred_observed <- predict(fit, data_observed, type = "response")
#   pred_reference <- predict(fit, data_reference, type = "response")
# 
#   pred_attrib <- pred_observed - pred_reference
# 
#   return(pred_attrib)
# }

est_attrib <- function(
  fit,
  data,
  exposures) {
  
  for (exp in exposures){
    data_reference <- copy(data)
    data_reference <- data_reference[, glue::glue({exp}) := 0] 
    
    pred_observed <- predict(fit, data, type = "response")
    pred_reference<- predict(fit, data_reference, type = "response")
    pred_attrib <- pred_observed - pred_reference
    data[, glue::glue("attr_{exp}"):= pred_attrib]
  }
  
  return(data)
}
