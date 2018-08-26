#' @include utilities.R
NULL
#' Replace Correlation Coefficients by Symbols
#'
#' @description Take a correlation matrix and replace the correlation coefficients by symbols according to the
#'   level of the correlation.
#' @param x a correlation matrix. Particularly, an object of class \code{cor_mat}.
#' @param cutpoints numeric vector used for intervals. Default values are
#'  \code{c(0, 0.25, 0.5, 0.75, 1)}.
#' @param symbols character vector, one shorter than cutpoints, used as
#'  correlation coefficient symbols. Default values are \code{c(" ", ".",  "+",
#'  "*")}.
#' @seealso \code{\link{cor_mat}()}
#' @examples
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_mat()
#'
#' # Replace correlation coefficient by symbols
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat %>%
#'   cor_as_symbols() %>%
#'   pull_lower_triangle()
#'
#' @name cor_as_symbols
#' @export
cor_as_symbols <- function( x, cutpoints = c(0, 0.25, 0.5, 0.75, 1),
                            symbols = c(" ", ".",  "+", "*"))
{


  if(inherits(x, "cor_mat_tri")){
    cor.mat <- x %>%
      replace_empty_by(0) %>%
      as_numeric_triangle() %>%
      as_matrix()
  }
  else{
    cor.mat <- as_matrix(x)
  }

  res <- stats::symnum(
    abs(cor.mat), cutpoints = cutpoints, symbols = symbols,
    abbr.colnames = FALSE
  ) %>%
    structure(class = "matrix") %>% # overwrite "noquote" class
    matrix_to_dataframe() %>%
    add_class(c("data.frame", "tbl_df"))

  pvalue <- attr(x, "pvalue")
  if(!is.null(pvalue)){
    res <- res %>%
      set_attrs(pvalue = pvalue) %>%
      add_class("cor_mat")
  }

  res
}

