#' Samples rows from a table
#'
#' @param tab A data fram
#' @param k The number of rows to sample
#' @return Random subset of rows from the data frame
#' @export
sample_rows <- function(tab, k = 5, replace = FALSE) {
  tab[sample(nrow(tab), k, replace), ]
}

#' Get data from 2014 house of representatives and print metadata
#'
#' @param rolls Which rolls to obtain, 1-99
#' @param print Whether to print metadata
#' @return A dataframe
#' @export
roll_data <- function(rolls = 1:99, print = TRUE) {
  if (print) {
    print(roll2014meta[rolls, -5])
  }
  roll2014votes[, c(2:3, rolls + 3)]
}
