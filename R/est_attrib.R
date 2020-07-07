#' Estimates attributable mortality
#'
#' @param fit A model fit
#' @param data The observed data
#' @param exposures The exposures that will be given attributable deaths
#' @param response The name of the response column
#' @export
est_attrib <- function(
  fit,
  data,
  exposures, 
  response) {

  
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

# est_attrib <- function(
#   fit,
#   data,
#   exposures) {
#   
#   data_ret_val = copy(data)
#   data_ret_val[, id := 1:.N]
#   
#   data_ret_val_2 = copy(data)
#   for (i in seq_along(exposures)){
#     data_reference <- copy(data)
#     data_reference <- data_reference[, glue::glue({names(exposures)[i]}) := exposures[[i]]]
#     
#     
#     sim_observed <- simulate(fit, newdata=data, nsim=100, family="poisson", re.form=NULL)
#     setDT(sim_observed)
#     
#     sim_reference <- simulate(fit, newdata=data_reference, nsim=100, family="poisson", re.form=NULL)
#     setDT(sim_reference)
#     
#     sim_attr <- sim_observed- sim_reference
#     sim_irr <- sim_observed/sim_reference
#     sim_attr[, id := 1:.N]
#     sim_irr[, id := 1:.N]
#     sim_attr <- melt.data.table(sim_attr, id.vars = "id")
#     sim_irr<- melt.data.table(sim_irr, id.vars = "id")
#     setnames(sim_attr, "value", glue::glue("attr_{names(exposures)[i]}"))
#     setnames(sim_irr, "value", glue::glue("irr_{names(exposures)[i]}"))
#     
#     if ( i == 1){
#       sim_attr_list <- sim_attr
#     } else{
#       sim_attr_list<-merge(sim_attr_list, sim_attr, by = c("id", "variable"))
#     }
#     sim_attr_list<-merge(sim_attr_list, sim_irr, by = c("id", "variable"))
#     
#     # pred_observed <- predict(fit, data, type = "response")
#     # pred_reference<- predict(fit, data_reference, type = "response")
#     # pred_attrib <- pred_observed - pred_reference
#     # data_ret_val_2[, glue::glue("attr_{names(exposures)[i]}"):= pred_attrib]
#   }
#   
#   data_ret_val <- merge(
#     data_ret_val,
#     sim_attr_list,
#     by="id"
#   )
#   setnames(data_ret_val, "variable", "sim")
#   data_ret_val[, sim:= as.numeric(as.factor(sim))]
#   return(data_ret_val)
# }

# test <- data_ret_val[,.(attrib_pr100_ili_lag_1 = mean(attr_pr100_ili_lag_1),
#                 attrib_pr100_covid19_lag_1 = mean(attr_pr100_covid19_lag_1),
#                 attrib_heatwave = mean(attr_temperature),
#                 attrib_pr100_ili_lag_1_05 = quantile(attr_pr100_ili_lag_1, 0.05),
#                 attrib_pr100_ili_lag_1_95 = quantile(attr_pr100_ili_lag_1, 0.95)), keyby=.(season, location_code, x, id)]
#
# library(ggplot2)
# ggplot(test[location_code == "county03" & season == "2016/2017"], aes(x = x, y = attrib_pr100_ili_lag_1)) +
#   geom_line() +
#   geom_line(aes(x = x, y = attrib_pr100_ili_lag_1_05), col = "red") +
#   geom_line(aes(x = x, y = attrib_pr100_ili_lag_1_95), col = "red")
#
# sum_sim <- test[, .(attrib_pr100_ili_lag_1 = sum( attrib_pr100_ili_lag_1)), keyby= .(season)]
# sum_pred <- data_ret_val_2[, .(attr_pr100_ili_lag_1 = sum( attr_pr100_ili_lag_1)), keyby= .(season)]
#
