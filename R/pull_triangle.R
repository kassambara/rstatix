#' @include utilities.R replace_triangle.R
NULL
#' Pull Lower and Upper Triangular Part of a Matrix
#' @description Returns the lower or the upper triangular part of a
#'   (correlation) matrix.
#' @param x a (correlation) matrix
#' @param diagonal logical. Default is FALSE. If TRUE, the matrix diagonal is
#'   included.
#' @param triangle the triangle to pull. Allowed values are one of
#'   "upper" and "lower".
#' @return an object of class \code{cor_mat_tri}, which is a data frame
#' @seealso \code{\link{replace_triangle}()}
#' @examples
#'
#' # Data preparation
#' #::::::::::::::::::::::::::::::::::::::::::
#' mydata <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec)
#' head(mydata, 3)
#'
#' # Compute correlation matrix and pull triangles
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Correlation matrix
#' cor.mat <- cor_mat(mydata)
#' cor.mat
#'
#' # Pull lower triangular part
#' cor.mat %>% pull_lower_triangle()
#'
#' # Pull upper triangular part
#' cor.mat %>% pull_upper_triangle()
#'
#'
#' @describeIn pull_triangle returns either the lower or upper triangular part of a matrix.
#' @export
pull_triangle <- function(x, triangle = c("lower", "upper"),
                          diagonal = FALSE){

  triangle.to.pull <- match.arg(triangle)
  triangle.to.replace <- ifelse(
    triangle.to.pull == "lower", "upper", "lower"
  )
  triangle.class <- paste0(triangle.to.pull, "_tri")

  res <- x %>%
    replace_triangle(triangle.to.replace, by = "", diagonal = diagonal) %>%
    add_class(triangle.class)
  res
}

#' @describeIn pull_triangle returns an object of class \code{upper_tri}, which
#'   is a data frame containing the upper triangular part of a matrix.
#' @export
pull_upper_triangle <- function(x,  diagonal = FALSE){
  x %>% pull_triangle("upper", diagonal = diagonal)
}

#' @describeIn pull_triangle returns an object of class \code{lower_tri}, which
#'   is a data frame containing the lower triangular part of a matrix.
#' @export
pull_lower_triangle <- function(x, diagonal = FALSE){
  x %>% pull_triangle("lower", diagonal = diagonal)
}
