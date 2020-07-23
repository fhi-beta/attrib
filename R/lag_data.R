# Lag data
#
# @param x
# @param lags
# @param ref
# @param by

lag_data <- function(x, lags, ref, by){
  #nx <- names(x)
  x <- as.vector(x)
  data<- data.table::data.table(lag_0 = x, by = by)

  for ( i in 1:lags){
    data[, glue::glue("lag_{i}") := shift(lag_0, n = i, fill = ref), by = by]
  }

  data[, by := NULL]
  data <- as.matrix(data)
  colnames(data) <- c(1,2,3)
  return(data)
}



# data <- gen_fake_attrib_data(4)
#
# response = "deaths"
# # take in the fixed effects
#
# formula <- "deaths ~ temperature_high +
# lag_data(x = pr100_ili, lags = 2, ref = 0, by = location_code) +
# sin(2 * pi * (week - 1) / 52) +
# cos(2 * pi * (week - 1) / 52)"
#
#
# data <- gen_fake_attrib_data(4)
#
# response = "deaths"
# # take in the fixed effects
# fixef <- "temperature_high +
# lag_data(x = pr100_ili, lags = 2, ref = 0, by = location_code) +
# sin(2 * pi * (week - 1) / 52) +
# cos(2 * pi * (week - 1) / 52)"
#
# offset <- "log(pop)"
# # take in the random effects
# ranef <- "(1|location_code) +(lag_data(x = pr100_ili, lags = 2, ref = 0, by = location_code)|season)"
#
#
#
# #ranef <- "(pr100_ili_lag_1|season)"
# formula <- paste0(response, "~", fixef,"+ offset(",offset, ")+", ranef)
# test <- lme4::glmer(stats::as.formula(formula), family = "poisson", data = data)
#
# lag_data(x, lags, ref, by)
#
#
#
#
#
#
# # function (x, df = NULL, knots = NULL, intercept = FALSE, Boundary.knots = range(x))
# # {
# #
# #   basis <- as.matrix((t(qr.qty(qr.const, t(basis))))[, -(1L:2L),
# #                                                      drop = FALSE])
# #   n.col <- ncol(basis)
# #   if (nas) {
# #     nmat <- matrix(NA, length(nax), n.col)
# #     nmat[!nax, ] <- basis
# #     basis <- nmat
# #   }
# #   dimnames(basis) <- list(nx, 1L:n.col)
# #   a <- list(degree = 3L, knots = if (is.null(knots)) numeric() else knots,
# #             Boundary.knots = Boundary.knots, intercept = intercept)
# #   attributes(basis) <- c(attributes(basis), a)
# #   class(basis) <- c("ns", "basis", "matrix")
# #   basis
# # }
