#' Estimates attributable mortality and irr
#'
#' @param data The output data from est_mort on long format with all attr in one colum with there values in clumn "value"
#' @export
est_attrib <- function(
  data) {

  exp_attr <- NULL
  exp_mort_observed <- NULL
  value <- NULL
  exp_irr <- NULL
  col_names <- NULL
  . <- NULL


  data_copy <- copy(data)
  data_copy[, exp_attr:= (exp_mort_observed - value)]
  data_copy[, exp_irr:= (exp_mort_observed/value)]

  q05 <- function(x){
    return(stats::quantile(x, 0.05))
  }
  q95 <- function(x){
    return(stats::quantile(x, 0.95))
  }

  setkeyv(data_copy, col_names[!col_names %in% c("exp_attr", "exp_irr","sim_id", "exposures", "exp_mort_observed", "value")])
  data_copy_part <- data_copy[1:1000]


  aggregated_sim_weekly <- data_copy[,
                                     unlist(recursive = FALSE, lapply(.(median = stats::median, q05 = q05, q95 = q95),
                                                                      function(f) lapply(.SD, f)
                                     )),
                                     by = key(data_copy),
                                     .SDcols = c("exp_attr", "exp_irr")]

  return (aggregated_sim_weekly)
}


