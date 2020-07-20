#' fit_attrib
#' @param data x
#' @param response x
#' @param fixef x
#' @param ranef x
#' @param offset x
#' @export
fit_attrib <- function(
  data,
  response,
  fixef,
  ranef,
  offset = NULL){
  is_data_table(data)

  #fix this with offset
  if(is.null(offset)){
    formula <- paste0(response, "~",fixef,"+",ranef)
    fit_fix <- stats::lm(stats::as.formula(paste0(response, "~" ,fixef)), data=data)
  } else {
    formula <- paste0(response, "~", fixef,"+ offset(",offset, ")+", ranef)
    fit_fix <- stats::lm(stats::as.formula(paste0(response, "~" , fixef,"+ offset(",offset, ")")), data=data)
  }


  fit <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)

  attr(fit, "fit_fix") <- fit_fix
  attr(fit, "offset") <- offset
  return(fit)
}

