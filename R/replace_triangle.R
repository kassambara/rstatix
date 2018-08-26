#' @include utilities.R
NULL
#' Replace Lower and Upper Triangular Part of a Matrix
#' @description Replace the lower or the upper triangular part of a
#'   (correlation) matrix.
#' @param x a (correlation) matrix
#' @param diagonal logical. Default is FALSE. If TRUE, the matrix diagonal is
#'   included.
#' @param triangle the triangle to replace. Allowed values are one of
#'   "upper" and "lower".
#' @param by a replacement argument. Appropriate values are either "" or NA. Used to replace
#'   the upper, lower or the diagonal part of the matrix.
#' @return an object of class \code{cor_mat_tri}, which is a data frame
#' @seealso \code{\link{pull_triangle}()}
#' @examples

#' # Compute correlation matrix and pull triangles
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Correlation matrix
#'  cor.mat <- mtcars %>%
#'    select(mpg, disp, hp, drat, wt, qsec) %>%
#'    cor_mat()
#'  cor.mat
#'
#'  # Replace upper triangle by NA
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat %>% replace_upper_triangle(by = NA)
#'
#'
#' # Replace upper triangle by NA and reshape the
#' # correlation matrix to have unique combinations of variables
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat %>%
#'   replace_upper_triangle(by = NA) %>%
#'   cor_gather()

#' @describeIn replace_triangle replaces the specified triangle by empty or NA.
#' @export
replace_triangle <- function(x, triangle = c("lower", "upper"), by = "", diagonal = FALSE){

  triangle <- match.arg(triangle)
  remaining.triangle <- ifelse(
    triangle == "lower", "upper", "lower"
  )
  remaining.triangle.class <- paste0(remaining.triangle, "_tri")

  replacement <- by
  get_tri <- switch(
    triangle, upper = upper.tri, lower = lower.tri
  )

  res <- as_matrix(x)
  res[get_tri(res)] <- replacement
  if (!diagonal) diag(res) <- replacement
  res <- res %>% matrix_to_dataframe()

  if(.is_cor_mat(x)){
    pvalue <- x %>% attr("pvalue") %>% as_matrix()
    pvalue[get_tri(pvalue)] <- replacement
    if (!diagonal) diag(pvalue) <- replacement
    pvalue <- pvalue %>% matrix_to_dataframe()

    res <- res %>%
      set_attrs(pvalue = pvalue)  %>%
      add_class("cor_mat_tri")
  }

  res %>% add_class(remaining.triangle.class)
}

#' @describeIn replace_triangle replaces the upper triangular part of a matrix.
#'   Returns an object of class \code{lower_tri}.
#' @export
replace_upper_triangle <- function(x,  by = "", diagonal = FALSE){
  x %>% replace_triangle("upper", by = by, diagonal = diagonal)
}

#' @describeIn replace_triangle replaces the lower triangular part of a matrix.
#'   Returns an object of class \code{lower_tri}
#' @export
replace_lower_triangle <- function(x,  by = "", diagonal = FALSE){
  x %>% replace_triangle("lower", by = by, diagonal = diagonal)
}

