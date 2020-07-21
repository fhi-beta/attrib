#' Estimates expected mortality
#'
#' For each row in the dataset the expected number of mortalities
#' will be calculated for the original data as well as for the
#' data with refereance values for the exposures.
#'
#'
#' @param fit A model fit constructed by fit_attrib
#' @param data The observed data
#' @param exposures The exposures that will get reference expected mortalities
#' @param response The response
#'
#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @export
est_mort <- function(
  fit,
  data,
  exposures,
  response) {
  if (length(which(is.na(data))) != 0){
    stop("The dataset has NA values")
  }

  id = NULL
  tag = NULL
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

  data_tot_ret <- est_mean(fit, data_tot, response = response)

  data_ret_val <- data_tot_ret[tag == "observed"]
  setnames(data_ret_val, "expected_mort", "exp_mort_observed")
  #this works but is a bit sslow
  for (i in seq_along(exposures)){
    data_ret_temp <- data_tot_ret[tag == glue::glue("ref_{names(exposures)[i]}")]
    data_ret_val[data_ret_temp, on= c("sim_id", "id"),
                glue::glue("exp_mort_{names(exposures)[i]}={(exposures)[i]}") := data_ret_temp$expected_mort]
  }

  # cur_col_names <- c(col_names_orig[1:12], "sim_id", "id") #need to remove the exposures
  # formula_cast <- paste(c(paste(cur_col_names, collapse = " + "),  "~ tag"), collapse = " ")
  # data_ret_val <- data.table::dcast.data.table(data_tot_ret,
  #                                              as.formula(formula_cast), value.var = "expected_mort")
  #

  return(data_ret_val)
}
