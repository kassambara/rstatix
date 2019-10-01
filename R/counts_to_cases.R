#' Convert a Table of Counts into a Data Frame of cases
#' @description converts a contingency table or a data frame of counts into a
#'   data frame of individual observations.
#' @param x a contingency table or a data frame
#' @param count.col the name of the column containing the counts. Default is "Freq".
#' @return a data frame of cases
#'
#' @examples
#' # Create a cross-tabulation demo data
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' xtab <- as.table(
#'   rbind(c(20, 5), c(16,9))
#' )
#' dimnames(xtab) <- list(
#'   before = c("non.smoker", "smoker"),
#'   after = c("non.smoker", "smoker")
#' )
#' xtab
#'
#' # Convert into a data frame of cases
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' df <- counts_to_cases(xtab)
#' head(df)
#'
#' @export
counts_to_cases <- function(x, count.col = "Freq") {
  if(!inherits(x, "table")) x <- as.table(as.matrix(x))
  x <- as.data.frame(x)
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[count.col]])
  # Drop count column
  x[[count.col]] <- NULL
  # Get the rows from x
  x <- x[idx, ]
  rownames(x) <- 1:nrow(x)
  x
}
