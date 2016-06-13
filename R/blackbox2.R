#' Time series data for taste test
#'
#' @param xs Time points, from 1 to 1000
#' @return Numeric vector
taste_test_ts_hidden <- function(xs) {
  set.seed(2)
  n <- max(xs)
  p_1 <- 0.8 * 1/(1 + exp(-((1:n) - 20)/80 - 0.3))
  p_2 <- 0.8 * 1/(1 + exp(-((1:n) - 20)/70 - 2))
  #plot(p_1, type = "l"); lines(p_2)
  assigs <- rbinom(n, 1, 0.5)
  p_comb <- ifelse(assigs == 0, p_1, p_2)
  y_n <- rbinom(n, 1, prob = p_comb)
  list(drink = c("P", "C")[assigs + 1][xs], y = y_n[xs])
}

#' Time series data for taste test
#'
#' @param xs Time points, from 1 to 200
#' @return Numeric vector
#' @export
taste_test_ts <- function(xs = 1:1000) {
  taste_test_ts_hidden(xs)
}



