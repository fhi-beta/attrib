#'
#' @param fit A model fit, OBS: offset must be the last element
#' @param data The observed data. OBS: mortality must be the first column
#' @param data_reference The reference data
#' @export
est_mean <- function(
  fit,
  data) {

  #data_test <- data
  data <- data_test[1:1000,]
  #data_reference <- copy(data) #[location_code == "county03" & week== 8][1:5,]
  #data_reference$pr100_ili_lag_1 <- 0
  #data <- data[location_code == "county03" & week == 8][1:5,]

  col_names <- colnames(data)

  data$tag <- "obs"
  data_reference$tag <- "ref"


  #data_tot <- rbindlist(list(data, data_reference))
  data_tot <- data

  n_sim <- 500

  fix_eff <- attr(fit, "fit_fix")

  x <- arm::sim(fit, n.sims=n_sim)

  # get the design matrix for the fixed effects
  data_fix <- model.frame(fit_fix, data=data_tot)

  # multiply it out
  expected_fix <- cbind(as.matrix(x@fixef),1) %*% rbind(1,as.matrix(t(data_fix[,-1]))) # 1 HØRER TIL INTERCEPT
  #expected_fix

  # set up the results for random effects
  expected_ran <- matrix(0, ncol=ncol(expected_fix), nrow=nrow(expected_fix))

  # slowly add in each of the random effects
  i = j = k = 1
  for(i in 1:length(x@ranef)){
    grouping <- names(x@ranef)[i]
    for(j in 1:dim(x@ranef[[i]])[3]){
      print(j)
      variable <- dimnames(x@ranef[[i]])[[3]][j]
      coefficients <- x@ranef[[i]][,,j]
      if(variable=="(Intercept)"){
        expected_ran <- expected_ran + coefficients[,data_tot[[grouping]]]
      } else {
        expected_ran <- expected_ran + coefficients[,data_tot[[grouping]]] %*% diag(data_tot[[variable]])
      }
    }
  }

  # add together the coefficients for the fixed and random effects
  expected <- as.data.table(exp(expected_fix + expected_ran))

  expected_t <- data.table::transpose(expected)
  expected_t$id <- 1:nrow(data_tot)
  data_tot$id <- 1:nrow(data_tot)

  new_data<- merge(data_tot, expected_t, by = "id", all = TRUE)
  new_data<- data.table::melt(new_data, id.vars = c(col_names, "id", "tag"))

  setnames(new_data, "variable", "sim")
  new_data$sim <- as.numeric(as.factor(new_data$sim))

  setnames(new_data, "value", "expected_mort")
  setkeyv(new_data, c(col_names, "id", "tag"))
  new_data[,.(mort_mean = mean(expected_mort),
              mort_quantile_025 = quantile(expected_mort, 0.025),
              mort_quantile_975 = quantile(expected_mort, 0.9755)),
           keyby = key(new_data)]


  new_data

  data_obs <- new_data[tag == "obs"]
  data_ref <- new_data[tag == "ref"]

  diff <- data_obs$expected_mort - data_ref$expected_mort
  ratio <- data_obs$expected_mort/data_ref$expected_mort

  data_final <- new_data[tag == "obs"]
  data_final$diff <- diff
  data_final$ratio <- ratio

  setkeyv(data_final, c(col_names, "id", "tag"))
  data_final[,.(attrib_mean = mean(diff),
                attrib_mean_quantile_025 = quantile(diff, 0.025),
                attrib_mean_quantile_025 = quantile(diff, 0.9755),
                irr_mean = mean(ratio),
                irr_mean_quantile_025 = quantile(ratio, 0.025),
                irr_mean_quantile_025 = quantile(ratio, 0.9755)),
           keyby = .(id)]
di}
