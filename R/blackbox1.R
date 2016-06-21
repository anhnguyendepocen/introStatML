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
possibly_unfair_die1_hidden <- function(n = 5) {
  sample(1:6, size = n, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.1, 0.2, 0.3))
}

#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
possibly_unfair_die2_hidden <- function(n = 5) {
  sample(1:6, size = n, replace = TRUE)
}

#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
#' @export
blackbox01 <- function(n = 5) {
  blackbox01_hidden(n)
}




#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
#' @export
possibly_unfair_die1 <- function(n = 5) {
  possibly_unfair_die1_hidden(n)
}

#' An unknown discrete distribution
#'
#' @param n Number of observations
#' @return Numeric vector
#' @export
possibly_unfair_die2 <- function(n = 5) {
  possibly_unfair_die2_hidden(n)
}

