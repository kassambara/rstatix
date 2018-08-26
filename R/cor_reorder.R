#' @include utilities.R
NULL
#' Reorder Correlation Matrix
#' @description reorder correlation matrix, according to the coefficients,
#'   using the hierarchical clustering method.
#'@param x a correlation matrix. Particularly, an object of class \code{cor_mat}.
#'@return a data frame
#'@seealso \code{\link{cor_mat}()}, \code{\link{cor_gather}()}, \code{\link{cor_spread}()}
#' @examples
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_mat()
#'
#
#' # Reorder by correlation and get p-values
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Reorder
#' cor.mat %>%
#'   cor_reorder()
#' # Get p-values of the reordered cor_mat
#' cor.mat %>%
#'   cor_reorder() %>%
#'   cor_get_pval()
#'
#' @name cor_reorder
#' @export
cor_reorder <- function(x){

  pvalue <- attr(x, "pvalue")
  x <- as_matrix(x)
  hc <- stats::as.dist(1 - x) %>%
    stats::hclust(method = "complete")

  x <- x %>% subset_matrix(hc$order)

  if(!is.null(pvalue)){
    pvalue <- pvalue %>% subset_matrix(hc$order)
    x <- x %>% set_attrs(pvalue = pvalue) %>%
      add_class("cor_mat")
  }
  x
}
