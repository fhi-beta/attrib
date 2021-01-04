#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data_clean Cleaned dataset from the function npowcast_clean
#' @param n_week Number of weeks to correct
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
#' 
nowcast <- function(data_aggregated, n_week_adjusting){
  
  # for developint
  data_aggregated <- data_fake_nowcasting_aggregated
  data <- nowcast(data_clean= data_aggregated, n_week_training = 50, n_week_adjusting = 8)
  n_week_adjusting <- 8
  
  retval <- vector("list" , length = (n_week_adjusting+1))
  
  
  for (i in 0:n_week_adjusting ){
    temp <- paste0("ncor0_", i)
    data[, temp_variable := get(temp)]
    data[, residual:= temp_variable -n_death]
    std <- (sum(data$residual[1:50]**2))**0.5
    data[, std_residual:= (temp_variable -n_death)/std]
    
    q <- ggplot2::ggplot(data, ggplot2::aes(x = temp_variable, y = std_residual))
    q <- q + ggplot2::geom_point()
    q <- q + ggplot2::geom_hline(yintercept = 0, colour = "red")
    
    temp_retval <- list()
    temp_retval$residualplot <- q
    retval[[i +1]] <- q
  }
   return (retval)
}

