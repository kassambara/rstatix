#' @include utilities.R
NULL
#' Subset Correlation Matrix
#' @name cor_select
#' @param x a correlation matrix. Particularly, an object of class \code{cor_mat}.
#' @param vars a character vector containing the variable names of interest.
#' @param ... One or more unquoted expressions (or variable names) separated by
#'  commas. Used to select variables of interest.
#'@return a data frame
#'@seealso \code{\link{cor_mat}()}, \code{\link{pull_triangle}()}, \code{\link{replace_triangle}()}
#' @examples
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_mat()
#'
#' # Subsetting correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#'
#' # Select some variables of interest
#' cor.mat %>%
#'   cor_select(mpg, drat, wt)
#'
#' # Remove variables
#' cor.mat %>%
#'   cor_select(-mpg, -wt)
#'
#' @export
cor_select <- function(x, ..., vars = NULL){

  vars <- x %>% get_selected_vars(..., vars = vars) %>%
    setdiff("rowname")

  if(!.is_empty(vars)){
    # Filter the correlation matrix
    vars <- unique(vars)
    x <- x %>% subset_matrix(vars)
    # Filter p-values
    pvalue <- x %>% attr("pvalue")
    if(!is.null(pvalue)){
      pvalue <- pvalue %>% subset_matrix(vars)
      x <- x %>% set_attrs(pvalue = pvalue) %>%
        add_class("cor_mat")
    }
  }

  x
}

