#' Samples rows from a table
#'
#' @param tab A data fram
#' @param k The number of rows to sample
#' @return Random subset of rows from the data frame
#' @export
sample_rows <- function(tab, k = 5, replace = FALSE) {
  tab[sample(nrow(tab), k, replace), ]
}

