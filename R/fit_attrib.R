#' fit_attrib
#' @param data x
#' @param fixef x
#' @param ranef x
#' @param offset x
#' @export
fit_attrib <- function(
  data,
  fixef,
  ranef, 
  offset = NULL){
  is_data_table(data)
  
  #fix this with offset
  if(is.null(offset)){
    formula <- paste0(fixef,"+",ranef)
    fit_fix <- lm(stats::as.formula(fixef), data=data)
  } else {
    formula <- paste0(fixef,"+ offset(",offset, ")+", ranef)
    fit_fix <- lm(stats::as.formula(paste0(fixef,"+ offset(",offset, ")")), data=data)
  }

  
  fit <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)

  attr(fit, "fit_fix") <- fit_fix
  attr(fit, "offset") <- offset
  return(fit)
}

