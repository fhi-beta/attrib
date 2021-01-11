#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data cleaned data to perform correction formula on
#' @param n_week_adjusting Number of weeks to correct
#'


nowcast_correction_fn_default <- function(data, n_week_adjusting){
  for ( i in 0:n_week_adjusting){
    
    fit <- stats::glm(stats::as.formula(paste0("n_death", "~",  glue::glue("n0_{i}"))), family = "poisson", data = data[1:(nrow(data)-n_week_adjusting)])
    n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
    data[, glue::glue("ncor0_{i}"):= n_cor]
    
  }
  return(data)
}



#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data_aggregated Cleaned dataset from the function npowcast_clean
#' @param n_week_adjusting Number of weeks to correct
#' @param n_week_training Number of weeks to train on
#' @param nowcast_correction_fn Correction function. Must return a table with columnames ncor0_i for i in 0:n_week and cut_doe. The default uses "n_death ~ n0_i" for all i in 0:n_week. 
#' @examples
#' \dontrun{
#'
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_training <- 12
#' data_correct <- nowcast(data, n_week_adjusting,n_week_training )
#' }
#' @return Dataset including the corrected values for n_death
#'
#' @export
nowcast <- function(
  data_aggregated,
  n_week_adjusting,
  n_week_training,
  nowcast_correction_fn = nowcast_correction_fn_default) {

  data_fake_death_clean <- NULL
  ncor <- NULL
  n_death <- NULL
  temp_variable <- NULL
  yrwk <- NULL
  cut_doe <- NULL

  
  ##### for developing
  # data_aggregated <- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_training <- 50
  # n_week_adjusting <- 8
  # nowcast_correction_fn<- nowcast_correction_fn_default
  # i = 2
  
  data <- as.data.table(data_aggregated)
  n_week_start <- n_week_training + n_week_adjusting
  data <- data[(nrow(data)-n_week_start+1):nrow(data)]
  
  #### corrected n_deaths ----
  data <- nowcast_correction_fn(data, n_week_adjusting)
  
  #check that all the required variables are there
  # (i.e. that the correction function actually gives reasonable stuff back)
  
  for ( i in 0:n_week_adjusting){
    temp <- paste0("ncor0_",i)
    if(! temp %in% colnames(data)){
      stop(glue::glue("nowcast_correction_fn is not returning {temp}"))
    }
  }
  
  
  data[, ncor := n_death]
  for ( i in 0:n_week_adjusting){
    temp <- paste0("ncor0_",i)
    data[, temp_variable := get(temp)]
    data[(nrow(data)-i), ncor:= temp_variable]
  }
  
  data[,temp_variable:=NULL]

  data[, yrwk:= isoyearweek(cut_doe)]

  
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
  
  # data_aggregated
  # fit <- stats::glm(stats::as.formula(paste0("n_death", "~", "n0_2 + n0_3 + n0_4")), family = "poisson", data = data_aggregated[600:(nrow(data_aggregated)-n_week_adjusting),])
  # summary(fit)
  # n_cor <- round(stats::predict(fit, newdata = data_aggregated, type = "response")) ###SHOULD THIS BE ROUNDED?
  # n_cor
  # 
  # 
  # data_aggregated[, n_cor :=n_cor]
  # 
  # data_aggregated[, diff := n_death - n_cor]
  
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
  
  retval <- data
  
  return (retval)
}
