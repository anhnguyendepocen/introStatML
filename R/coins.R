#' Simulates independent coin flips
#'
#' @param n Number of coin flips
#' @return a character vector of coin flips
#' @export
coin_flips <- function(n = 1) {
  sample(c("H", "T"), n, replace = TRUE)
}

#' Simulates independent dice rolls
#'
#' @param n Number of die rolls
#' @return a numeric vector of die rolls
#' @export
dice_rolls <- function(n = 1) {
  sample(1:6, n, replace = TRUE)
}


