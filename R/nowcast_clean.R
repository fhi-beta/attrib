




#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data Dataset containing DoE (Date of event) and DoR (Date of registation). The columns must have these exact names. 
#' @param aggregation_date Date of aggregation 
#' 
nowcast_clean <- function(
  data,
  aggregation_date) {
  
  
  # for developing
  data <- gen_fake_death_data()
  aggregation_date <- as.Date("2020-01-01")
  
  
  ### cleaning ----
  d <- data.table::as.data.table(data)
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
  
  
  retval <- vector("list", length = 52)
  d_within_week <- d[, .(cut_DoE)]
  col_names <- vector("double", length = 52)
  
  for ( i in 1:52){
    new_outcome <- paste0("n0_", (i-1))
    col_names[i] <- new_outcome
    
    temp <- d[, .(temp_outcome = sum(DoR < (as.Date(cut_DoE) + i*7))/n_death), 
              keyby = .(cut_DoE)]
    retval[[i]] <- as.data.frame(temp[, temp_outcome])
    
  }
  
  
  d_within_week <- cbind.data.frame(retval)
  colnames(d_within_week) <- col_names
  
  d[, glue::glue("w{i}") := (as.Date(cut_DoD) + i*7)]
    # Do as in sykdomspulsen
  # return only dataset or graphs as well? ## First only dataset! 
  
  # Create a fake dataset for testing :) 
  
  # Figure out something smart with NA
  
  # 
  
  
  
  retval <- 1
  
  
  
  
  return (retval)
}