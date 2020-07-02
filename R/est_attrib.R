#' Estimates attributable mortality
#'
#' @param fit A model fit
#' @param data The observed data
#' @param exposures The exposures that will be given attributable deaths
#' @export
est_attrib <- function(
  fit,
  data,
  exposures) {

  data_ret_val = copy(data)
  data_ret_val[, id := 1:.N]

  data_ret_val_2 = copy(data)
  for (i in seq_along(exposures)){
    data_reference <- copy(data)
    data_reference <- data_reference[, glue::glue({names(exposures)[i]}) := exposures[[i]]]

  }

  )
  setnames(data_ret_val, "variable", "sim")
  data_ret_val[, sim:= as.numeric(as.factor(sim))]
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
