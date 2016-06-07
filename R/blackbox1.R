#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
blackbox01_hidden <- function(n = 5) {
  x <- rpois(2 * n + 10, lambda = 1)
  x <- x[x <= 5]
  x[1:n]
}


#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
#' @export
blackbox01 <- function(n = 5) {
  blackbox01_hidden(n)
}
