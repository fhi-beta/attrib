create_basis <- function(x, type){

  lg <- 30  # 30 days maximum lag (fixed)

  if(type=="cubic"){
    retval <- dlnm::crossbasis(x, lag=lg,
                               argvar=list(fun="bs", degree=2, knots=stats::quantile(x, c(0.1,0.75,0.9))),
                               arglag=list(fun="ns", knots=dlnm::logknots(lg,3)))
  } else if(type=="linear"){
    retval <- dlnm::crossbasis(x, lag=lg,
                     argvar=list(fun="poly", degree=1),
                     arglag=list(fun="ns", knots=dlnm::logknots(lg,3)))
  }
  return(retval)
}

gen_basis_name <- function(tag){
  return(glue::glue("x_basis_{tag}"))
}

#' fit_attrib
#' @param outcome x
#' @param exposure_values x
#' @param exposure_types a
fit_attrib <- function(
  outcome = FluMoDL::greece$daily$deaths,
  exposure_values = list(
    "tg"=FluMoDL::greece$daily$temp
  ),
  exposure_types = list(
    "tg"="cubic"
  )
  ){

  basis_names <- c()
  basis <- list()
  for(i in seq_along(exposure_values)){
    name <- names(exposure_values)[[i]]
    new_name <- gen_basis_name(tag="name")
    basis[[name]] <- temp <- create_basis(x=exposure_values[[i]], type=exposure_types[[i]])
    txt <- glue::glue("{new_name} <- temp")
    eval(parse(text=txt))
    basis_names <- c(basis_names, new_name)
  }

  exposures <- glue::glue_collapse(basis_names, sep=" + ")
  formula <- glue::glue("outcome ~ {exposures}")

  fit <- stats::glm(stats::as.formula(formula), family="quasipoisson")

  MMPs <- pred <- list()
  for(i in seq_along(exposure_values)){
    name <- names(exposure_values)[[i]]
    vals <- exposure_values[[name]]
    txt <- glue::glue(
      "dlnm::crosspred({basis_names[i]}, fit,",
      "at = seq(ceiling(min(vals)), floor(max(vals)), 1),",
      "bylag=0.2, cen=round(median(vals)), cumul=TRUE)"
    )
    pred[[name]] <- eval(parse(text=txt))

    MMP <- as.integer(names(which(pred[[name]]$allfit==min(pred[[name]]$allfit))))
    MMPs[[name]] <- MMP
    # Refit prediction for temperature, centered at the MMP
    txt <- glue::glue(
      "dlnm::crosspred({basis_names[i]}, fit,",
      "at = seq(ceiling(min(vals)), floor(max(vals)), 1),",
      "bylag=0.2, cen={MMP}, cumul=TRUE)"
    )
    pred[[name]] <- eval(parse(text=txt))
  }

  attrib_x <- list(
    "outcome"=outcome,
    "exposure_values"=exposure_values,
    "fit"=fit,
    "basis"=basis,
    "pred"=pred,
    "mmps"=MMPs
  )

  return(attrib_x)
}

#' get_attrib
#' @param attrib_x x
#' @param tag x
#' @param range a
#' @param sub a
get_attrib <- function(attrib_x, tag="tg", range=c(15,100), sub=NULL){

  if(is.null(sub)){
    sub <- 1:length(attrib_x$outcome)
  }

  retval <- FluMoDL::attrdl(
    x=attrib_x$exposure_values[[tag]],
    basis=attrib_x$basis[[tag]],
    cen=attrib_x$mmp[[tag]],
    range=c(10,20),
    cases=attrib_x$outcome,
    coef=attrib_x$pred[[tag]]$coefficients,
    vcov=attrib_x$pred[[tag]]$vcov,
    sim=T,
    sub=sub
  )

  return(retval)
}
