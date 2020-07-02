#' fit_attrib
#' @param data x
#' @param fixef x
#' @param ranef x
#' @export
fit_attrib <- function(
  data,
  fixef,
  ranef){
  is_data_table(data)
  
  formula <- paste0(fixef,"+",ranef)
  fit_fix <- lm(stats::as.formula(fixef), data=data)
  
  fit <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)

  attr(fit, "fit_fix") <- fit_fix
  return(fit)
}

