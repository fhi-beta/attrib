#' Estimates attributable mortality and irr
#'
#' @param data The output data from est_mort on long format with all attr in one colum with there values in clumn "value" 
#' @export
est_attrib <- function(
  data) {
  
  data_copy[, exp_attr:= (exp_mort_observed - value)]
  data_copy[, exp_irr:= (exp_mort_observed/value)]
  
  q05 <- function(x){
    return(quantile(x, 0.05))
  }
  q95 <- function(x){
    return(quantile(x, 0.95))
  }
  
  setkeyv(data_copy, col_names[!col_names %in% c("exp_attr", "exp_irr","sim_id", "exposures", "exp_mort_observed", "value")])
  data_copy_part <- data_copy[1:1000]
  
  
  aggregated_sim_weekly <- data_copy[,
                                     unlist(recursive = FALSE, lapply(.(median = median, q05 = q05, q95 = q95),
                                                                      function(f) lapply(.SD, f)
                                     )), 
                                     by = key(data_copy),
                                     .SDcols = c("exp_attr", "exp_irr")]
  
  
  # 
  # 
  # 
  # data_copy <- copy(data) 
  # 
  # try(data_copy[, c("id_row", "tag") := NULL], silent = TRUE)
  # col_names <- colnames(data_copy)
  # 
  # 
  # exp_col <- rep(0, length(exposures)*3 +1)
  # for ( i in seq_along(exposures)){
  #   exp_col[i] <- glue::glue("exp_mort_{names(exposures)[i]}={exposures[i]}")
  #   exp_col[i+length(exposures)] <- glue::glue("attr_{names(exposures)[i]}")
  #   exp_col[i + 2*length(exposures)] <- glue::glue("irr_{names(exposures)[i]}")
  # }
  # exp_col[length(exposures)*3 +1]<- "exp_mort_observed"
  # 
  # 
  # setkeyv(data_copy, col_names[!col_names %in% c(exp_col, "sim_id")])
  # 
  # mort_obs <- "exp_mort_observed"
  # 
  # for ( i in seq_along(exposures)){
  #   mort_ref <- glue::glue("exp_mort_{names(exposures)[i]}={exposures[i]}")
  #   data_copy[, {glue::glue("attr_{names(exposures)[i]}"):= (data_copy[[mort_obs]]- data_copy[[as.character(mort_ref)]])}]
  #   data_copy[, {glue::glue("irr_{names(exposures)[i]}"):= (data_copy[[mort_obs]]/ data_copy[[as.character(mort_ref)]])}]
  # }
  # 
  # 
  # q05 <- function(x){
  #   return(quantile(x, 0.05))
  # }
  # q95 <- function(x){
  #   return(quantile(x, 0.95))
  # }
  # 
  # aggregated_sim_weekly <- data_copy[,
  #                             unlist(recursive = FALSE, lapply(.(median = median, q05 = q05, q95 = q95),
  #                                                              function(f) lapply(.SD, f)
  #                                                              )), 
  #                             by = key(data_copy),
  #                             .SDcols = exp_col]
  return (aggregated_sim_weekly)
}


