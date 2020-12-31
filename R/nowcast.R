




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
  
  data <- data_fake_death_clean
  n_week <- 4

  i = 2
  
  
  fit <- glm(n_death ~ n0_4, family = "poisson", data = data[1:(nrow(data)-15)])
  summary(fit)
  predict(fit, newdata = data, type = "response")
  
  retval <- 1
  
  return (retval)
}