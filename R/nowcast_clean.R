
#' Cleaned fake data for mortalityregistration
#'
#' @format
#' \describe{
#' \item{doe}{Date og event}
#' \item{dor}{Date of registration}
#' }
"data_fake_nowcasting_aggregated"



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
#' n_week <- 52
#' 
#' clean_data <- nowcast_clean(data, aggregation_date, n_week)
#' }
#' @return Cleaned dataset with the percentiles of registered events within the last 52 weeks
#'
#' @export
nowcast_aggregate <- function(
  data,
  aggregation_date,
  n_week) {
  
  DoE <- NULL
  DoR <- NULL
  cut_DoE <- NULL
  n_death <- NULL
  temp_outcome <- NULL
  n0_0 <- NULL
  p0_0 <- NULL
  temp_variable_n <- NULL
  temp_variable_p <- NULL
  . <- NULL
  new_value <- NULL
  # retur only dataset or graphs as well? ## First only dataset! 
  
  
  
  ##### for developing
  
  # data <- gen_fake_death_data()
  # aggregation_date <- as.Date("2020-01-01")
  # n_week <- 52

  ### check og parameters ----
  
  if (! "DoE" %in% colnames(data)){
    stop("The dataset does not have the correct column names")
  }
  
  if (! "DoR" %in% colnames(data)){
    stop("The dataset does not have the correct column names")
  }
  
  if (! "n_week" > 1){
    stop("n_week is to small" )
  }
  
  #should perhaps have a check for max length as well. 
  
  ### cleaning ----
  d <- data.table::as.data.table(data)
  d <- d[, .(DoE, DoR)]
  d <- d[DoR < aggregation_date]
  d[, cut_DoE := as.Date(cut(DoE, "week"))]
  
  
  # count deaths
  
  d_death <- d[ , .(
    "n_death" = .N
  ), keyby = .(
    cut_DoE
  )]
  
  d[ d_death, 
     on = "cut_DoE",
     n_death := n_death]
  
  retval <- vector("list", length = n_week)
  d_within_week <- d[, .(cut_DoE)]
  
  for ( i in 1:n_week){
    
    temp <- d[, .(
      temp_outcome_n = sum(DoR < (as.Date(cut_DoE) + i*7)),
      temp_outcome_p = sum(DoR < (as.Date(cut_DoE) + i*7))/n_death), 
      keyby = .(cut_DoE)]
    
    setnames(temp, "temp_outcome_p", paste0("p0_", (i-1)))
    setnames(temp, "temp_outcome_n", paste0("n0_", (i-1)))
    
    retval[[i]] <- as.data.frame(subset(temp, select = -c(cut_DoE) ))
    
  }
  
  d_within_week <- cbind.data.frame(retval)
  d_within_week <- unique(as.data.table(d_within_week))
  d_within_week <- (cbind(d_within_week, unique(d[, .(cut_DoE, n_death)])))
  
  
  # insert NA where we do not have data
  
  d_corrected <- d_within_week[, .(cut_DoE, n_death, n0_0)]
  for ( i in 2:n_week){
    
    week_n <- paste0("n0_",(i-1))
    week_p <- paste0("p0_",(i-1))
    d_within_week[, new_value := NA]
    d_within_week[, temp_variable_n := get(week_n)]
    d_within_week[, temp_variable_p := get(week_p)]
    d_within_week[(nrow(d_within_week)-i+2):nrow(d_within_week), temp_variable_n := new_value]
    d_within_week[(nrow(d_within_week)-i+2):nrow(d_within_week), temp_variable_p := new_value]
    d_corrected[ d_within_week, 
                 on = "cut_DoE",
                 paste0("n0_",(i-1)) := temp_variable_n]
    d_corrected[ d_within_week, 
                 on = "cut_DoE",
                 paste0("p0_",(i-1)) := temp_variable_p]
  }
  
  
  d_corrected[99:103]
  
    # data_fake_death_clean <- d_corrected
    # save(data_fake_death_clean, file = "data/data_fake_death_clean.rda", compress = "bzip2")

  
  retval <- d_corrected
  
  return (retval)
}