#' @include utilities.R
NULL
#' Pull Lower and Upper Triangular Part of a Matrix
#' @description Returns the lower or the upper triangular part of a
#'   (correlation) matrix.
#' @param x a (correlation) matrix
#' @param diagonal logical. Default is FALSE. If TRUE, the matrix diagonal is
#'   included.
#' @param triangle the type of triangle to pull. Allowed values are one of
#'   "upper" and "lower".
#' @param replacement Appropriate values are either "" or NA. Used to replace
#'   the upper, lower or the diagonal part of the matrix.
#' @return a data frame
#' @examples
#' # Compute correlation matrix
#' df <- mtcars[, c(1,3,4,5,6,7)]
#' cor.mat <- cor_mat(df)
#' cor.mat
#'
#' # Pull lower triangular part
#' cor.mat %>% pull_lower_triangle()
#'
#' # Pull upper triangular part
#' cor.mat %>% pull_upper_triangle()

#' @describeIn pull_triangle Returns either the lower or upper triangular part of a matrix.
#' @export
pull_triangle <- function(x, triangle = c("lower", "upper"),
                          diagonal = FALSE, replacement = ""){

  triangle <-match.arg(triangle)
  switch(
    triangle,
    lower = pull_lower_triangle(x, diagonal = diagonal, replacement = replacement),
    upper = pull_upper_triangle(x, diagonal = diagonal, replacement = replacement)
  )
}

#' @describeIn pull_triangle Returns the upper triangular part of a matrix.
#' @export
pull_upper_triangle <- function(x, diagonal = FALSE, replacement = ""){

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  x[lower.tri(x)] <- replacement
  if (!diagonal) diag(x) <- replacement

  .matrix_to_dataframe(x)
}

#' @describeIn pull_triangle Returns the lower triangular part of a matrix.
#' @export
pull_lower_triangle <- function(x, diagonal = FALSE, replacement = ""){

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  x[upper.tri(x)] <- replacement
  if (!diagonal) diag(x) <- replacement
  .matrix_to_dataframe(x)
}


