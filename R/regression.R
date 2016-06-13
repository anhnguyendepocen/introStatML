#' Moving average for uniformly spaced data
#'
#' @param y Y values
#' @param h Bandwidth = number of neighbors in either direction
#' @export
ma_unif <- function(y, h = 1) {
  n <- length(y)
  pad_y <- c(rep(0, h + 1), y, rep(0, h + 1))
  c_y <- cumsum(pad_y)
  wsize <- h * 2 + 1
  diff_y <- c_y[-(1:wsize)] - c_y[-((length(c_y)-wsize+1): length(c_y))]
  sum_nbr_y <- diff_y[1:n]
  n_nbr_y <- h + c(1:h, rep(h + 1, n - 2 * h), rev(1:h))
  sum_nbr_y/n_nbr_y
}

#' Moving average for uniformly spaced data but with filter
#'
#' @param y Y values
#' @param h Bandwidth = number of neighbors in either direction
#' @param filt Logical indicating which values to filter
#' @export
ma_unif_filt <- function(y, h = 1, filt) {
  n <- length(y)
  y[!filt] <- 0
  pad_y <- c(rep(0, h + 1), y, rep(0, h + 1))
  pad_f <- c(rep(0, h + 1), filt, rep(0, h + 1))
  c_y <- cumsum(pad_y)
  c_f <- cumsum(pad_f)
  wsize <- h * 2 + 1
  diff_y <- c_y[-(1:wsize)] - c_y[-((length(c_y)-wsize+1): length(c_y))]
  diff_f <- c_f[-(1:wsize)] - c_f[-((length(c_y)-wsize+1): length(c_y))]
  sum_nbr_f <- diff_f[1:n]
  sum_nbr_y <- diff_y[1:n]
  sum_nbr_y/sum_nbr_f
}
