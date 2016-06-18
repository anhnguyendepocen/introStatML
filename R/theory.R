#' Visualize a discrete distribution
#'
#' @param support The support of the pmf
#' @param pmf The probability mass function
#' @return A plot
#' @export
plot_discrete_distribution <- function(support, pmf) {
  plot(support, pmf, ylim = c(0, max(pmf) * 1.1))
  for (i in 1:length(support)) {
    s <- support[i]
    lines(c(s, s), c(0, pmf[i]))
  }
  abline(h = 0)
}
