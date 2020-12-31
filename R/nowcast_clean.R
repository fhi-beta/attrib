




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
  data,
  aggregation_date,
  n_week) {
  
  DoE <- NULL
  DoR <- NULL
  cut_DoE <- NULL
  n_death <- NULL
  temp_outcome <- NULL
  n0_0 <- NULL
  temp_variable <- NULL
  # retur only dataset or graphs as well? ## First only dataset! 
  
  
  
  # for developing
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
  col_names <- vector("double", length = n_week)
  
  for ( i in 1:n_week){
    new_outcome <- paste0("n0_", (i-1))
    col_names[i] <- new_outcome
    
    temp <- d[, .(temp_outcome = sum(DoR < (as.Date(cut_DoE) + i*7))/n_death), 
              keyby = .(cut_DoE)]
    retval[[i]] <- as.data.frame(temp[, temp_outcome])
    
  }
  
  
  d_within_week <- cbind.data.frame(retval)
  colnames(d_within_week) <- col_names
  d_within_week <- unique(as.data.table(d_within_week))
  d_within_week <- (cbind(d_within_week, unique(d[, .(cut_DoE)])))
  
  
  # insert NA where we do not have data
  
  d_corrected <- d_within_week[, .(cut_DoE, n0_0)]
  for ( i in 2:n_week){
    week <- paste0("n0_",(i-1))
    d_within_week[, new_value := NA]
    d_within_week[, temp_variable := get(week)]
    d_within_week[(nrow(d_within_week)-i+1):nrow(d_within_week), temp_variable := new_value]
    d_corrected[ d_within_week, 
                 on = "cut_DoE",
                 paste0("n0_",(i-1)) := temp_variable]
  }
  d
  

  
  
  
  
  
  retval <- d_corrected
  
  return (retval)
}