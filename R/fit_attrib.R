#' Data fit
#'
#' Data fit using glmer from lme4 with family poisson to fit the dataset with the given formula.
#'
#'
#' @param data The observed data to be fitted.
#' @param response The response
#' @param fixef The fixed effects
#' @param ranef The random effects
#' @param offset The offsets.
#'
#' @return The model fit of the data with additional attributes offset, response and fit_fix.
#' Offset and response are the same as in the input and fit_fix is the linear model of the fix effects.
#'
#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @examples
#'
#' response <- "deaths"
#' fixef <- "pr100_ili + sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)"
#' ranef <- " (pr100_ili| season)"
#' offset <- "log(pop)"
#'
#' data = data.table::data.table(deaths = c(100, 110, 150, 160), week = c(10, 11, 10, 11),
#'                   season = c("2018/2019", "2018/2019", "2019/2020", "2019/2020"),
#'                   pr100_ili = c(0.2, 0.21, 0.35, 0.36),
#'                   pop = c(50000, 50000, 50000, 50000))
#'
#' fit_attrib(data = data, response = response, fixef = fixef, ranef = ranef, offset = offset)
#'
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

    if (tryCatch({stats::as.formula(paste0(response, "~" ,fixef))}, error = function(e){"error"}) == "error"){
      stop("response, fixef or ranef is not in the correct form")
    }

    formula <- paste0(response, "~",fixef,"+",ranef)
    fit_fix <- stats::lm(stats::as.formula(paste0(response, "~" ,fixef)), data=data)
  } else {

    formula <- paste0(response, "~", fixef,"+ offset(",offset, ")+", ranef)

    if (tryCatch({stats::as.formula(formula)}, error = function(e){"error"}) == "error"){
      stop("response, offset, fixef or ranef is not in the correct form")
    }

    fit_fix <- stats::lm(stats::as.formula(paste0(response, "~" , fixef,"+ offset(",offset, ")")), data=data)
  }


  fit <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)

  attr(fit, "fit_fix") <- fit_fix
  attr(fit, "offset") <- offset
  attr(fit, "response") <- response

  return(fit)
}

