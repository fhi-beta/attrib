




#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data Dataset containing DoE (Date of event) and DoR (Date of registation). The columns must have these exact names. 
#' @param aggregation_date Date of aggregation 
#' @param n_week Number of weeks to calculate the percentage of the total registraations. Must be larger og equal to 2 amd smaller than the total number of weeks in the dataset.
#' 
#' @examples
#' \dontrun{
#'
#' data <- attrib::data_fake_death
#' aggregation_date <- as.Date("2020-01-01")
#' 
#' clean_data <- nowcast_clean(data, aggregation_date)
#' }
#' @return Cleaned dataset with the percentiles of registered events within the last 52 weeks
#'
#' @export
nowcast_clean <- function(
  data_clean,
  n_week) {
  
  ##### for developing
  data <- as.data.table( data_fake_death_clean)
  n_week <- 8

  i = 2
  
  #### corrected n_deaths ----
  for ( i in 0:n_week){
    
    fit <- glm(stats::as.formula(paste0("n_death", "~",  glue::glue("n0_{i}"))), family = "poisson", data = data[1:(nrow(data)-n_week)])
    n_cor <- round(predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
    data[, glue::glue("ncor0_{i}"):= n_cor]

  }
  
  data[, ncor := n_death]
  for ( i in 0:n_week){
    temp <- paste0("ncor0_",i)
    data[, temp_variable := get(temp)]
    data[(nrow(data)-i), ncor:= temp_variable]
  }
  
  data[,temp_variable:=NULL]
  
  data[, yrwk:= fhi::isoyearweek(cut_DoE)]
  
  col_order <- c(c("yrwk", "n_death", "ncor"), colnames(data)[which(!colnames(data) %in% c("yrwk", "n_death", "ncor"))])
  setcolorder(data, col_order)
  ##### multiple weeks in formula ---- 
  
  # data <- as.data.table( data_fake_death_clean)
  # n_week <- 4
  
  # for ( i in 1:n_week){
  #   
  #   formula <- paste0("n_death", "~",  glue::glue("n0_{i}"))
  #   if(i>1){
  #     for (j in 1:(i-1)){
  #      formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
  #     }
  #   }
  #   
  #   fit <- glm(stats::as.formula(formula), family = "poisson", data = data[1:(nrow(data)-n_week)])
  #   n_cor <- round(predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
  #   data[, glue::glue("ncor0_{i}"):= n_cor]
  #   
  # }
  
  
  
  
  #### testing ----
  
  # data[, n_death_lag_2 := shift(n_death, 1L)]
  # fit <- glm(n_death ~ n0_2 + n0_3 + n0_4, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  # 
  # data[, n_death_lag_2 := shift(n_death, 1L)]
  # fit <- glm(n_death ~ n0_4 + n0_3, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  # 
  # fit <- glm(n_death ~  n0_4, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # 
  # 
  # 
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  
  retval <- 1
  
  return (retval)
}