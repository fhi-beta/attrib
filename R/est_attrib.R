#' Estimates simulations of expected responses
#'
#' For each exposure the dataset is copied and the original value replaced by the referance value.
#' Then the sim function is used to generate 500 simulations of expected responses for each row.
#' Finaly the dataset is transformed to obtain expected response for original and referance values
#'  of the given exposures for each original row of the dataset.
#'
#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param fit A model fit constructed by fit_attrib
#' @param data The observed data
#' @param exposures The exposures that will get reference expected mortalities
#'
#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @examples
#'
#' response <- "deaths"
#' fixef <- "pr100_ili_lag_1 + sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)"
#' ranef <- " (pr100_ili_lag_1| season)"
#' offset <- "log(pop)"
#'
#' data = attrib::data_fake_nation
#'
#' fit <- fit_attrib(data = data, response = response, fixef = fixef, ranef = ranef, offset = offset)
#' exposures <- c(pr100_ili_lag_1 = 0)
#' new_data <- est_attrib(fit, data, exposures)
#' new_data[]
#' @return dataset with expected responses for all exposures given there re
#'
#' @export
est_attrib <- function(
  fit,
  data,
  exposures) {
  if (length(which(is.na(data))) != 0){
    stop("The dataset has NA values")
  }

  if (is.null(attr(fit, "fit_fix"))){
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  if (is.null(attr(fit, "response"))){
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  if( length(exposures)==0){
    stop("Exposures is empthy")
  }
  for ( i in seq_along(exposures)){
    if (!names(exposures)[i] %in% colnames(data)){
      stop(glue::glue("Exposure {names(exposures)[i]} is not in the dataset"))
    }
  }


  id = NULL
  tag = NULL
  id_row = NULL

  data_ret_val = copy(data)
  data_ret_val[, id := 1:.N]

  col_names_orig<- colnames(data)

  data_observed <- copy(data)
  data_observed[, id:= 1:.N]
  data_observed$tag <- "observed"
  #data_ret_val_2 = copy(data)
  data_tot <- vector("list", length = length(exposures)+1 )
  data_tot[[1]] <- data_observed
  for (i in seq_along(exposures)){
    data_reference <- copy(data)
    data_reference[, id:= 1:.N]
    data_reference <- data_reference[, glue::glue({names(exposures)[i]}) := exposures[[i]]]
    data_reference$tag <- as.character(glue::glue("ref_{names(exposures)[i]}"))
    data_tot [[i+1]]<- data_reference
  }
  data_tot <- rbindlist(data_tot)

  data_tot_ret <- sim(fit, data_tot)

  data_ret_val <- data_tot_ret[tag == "observed"]
  setnames(data_ret_val, "expected_mort", "exp_mort_observed")
  #this works but is a bit sslow
  for (i in seq_along(exposures)){
    data_ret_temp <- data_tot_ret[tag == glue::glue("ref_{names(exposures)[i]}")]
    data_ret_val[data_ret_temp, on= c("sim_id", "id"),
                glue::glue("exp_mort_{names(exposures)[i]}={(exposures)[i]}") := data_ret_temp$expected_mort]
  }

  data_ret_val[, tag := NULL]
  data_ret_val[, id_row := NULL]
  return(data_ret_val)
}
