#' Fake data for mortalityregistration
#'
#' @format
#' \describe{
#' \item{DoE}{Date og event}
#' \item{DoR}{Date of registration}
#' }
"data_fake_death"


# Generates fake data
#
# This function generates a fake dataset with parameters
# @param n_locations Telling how many locations one wants in the output data, default = 11 the number of municipalities in Norway.

# gen_fake_death_data <- function() {
#   DoE <- NULL
#   DoR <- NULL
#   reg_lag <- NULL
#   . <- NULL
#   
#   start_date <- as.Date("2018-01-01")
#   end_date <- as.Date("2020-01-01")
#   
#   
#   skeleton <- expand.grid(
#     count = seq(1, 150, by = 1),
#     date = seq.Date(
#       from = start_date,
#       to = end_date,
#       by = 1 # to get a weakly base.
#     ),
#     stringsAsFactors = FALSE
#   )
#   setDT(skeleton)
#   
#   # skeleton <- expand.grid(
#   #   date = seq.Date(
#   #     from = start_date,
#   #     to = end_date,
#   #     by = 1 # to get a weakly base.
#   #   ),
#   #   stringsAsFactors = FALSE
#   # )
#   # setDT(skeleton)
#   # 
#   # skeleton[, n_death := rnorm(.N, mean = 150, sd = 20)]
#   
#   
#   skeleton[, DoE := date]
#   skeleton[, reg_lag := stats::rpois(.N, 28)]
#   skeleton[, DoR := DoE + reg_lag]
#   
#    # data_fake_death <- skeleton[,.(DoE, DoR)]
#    # save(data_fake_death, file = "data/data_fake_death.rda", compress = "bzip2")
# 
#   return(skeleton[,.(DoE, DoR)])
# }


gen_fake_death_data <- function() {
  DoE <- NULL
  DoR <- NULL
  reg_lag <- NULL
  . <- NULL
  
  start_date <- as.Date("2018-01-01")
  end_date <- as.Date("2020-01-01")
  
  date = seq.Date(
    from = start_date,
    to = end_date,
    by = 1 
  )
  
  temp_vec <- vector( "list", length = length(date))
  n_death <- round(rnorm(length(date), mean = 150, sd = 20))
  for (i in seq_along(date)){
    skeleton <- expand.grid(
      date = date[i],
      count = seq(1, n_death[i], by = 1),
      stringsAsFactors = FALSE
    )
    setDT(skeleton)
    temp_vec[[i]] <- as.data.table(skeleton)
  }
  
  skeleton <- rbindlist(temp_vec)
  
  skeleton[, DoE := date]
  skeleton[, reg_lag := stats::rpois(.N, 28)]
  skeleton[, DoR := DoE + reg_lag]
  
  # data_fake_death <- skeleton[,.(DoE, DoR)]
  # save(data_fake_death, file = "data/data_fake_death.rda", compress = "bzip2")
  
  return(skeleton[,.(DoE, DoR)])
}
