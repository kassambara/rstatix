#' @include utilities.R
NULL
#' Convert a Correlation Test Data Frame into a Correlation Matrix
#'
#' @description Convert a correlation test data frame, returned by the
#'   \code{\link{cor_test}()}, into a correlation matrix format.
#'
#' @param x an object of class \code{cor_test}.
#' @return  Returns a data frame containing the matrix of the correlation
#'   coefficients. The output has an attribute named "pvalue", which contains
#'   the matrix of the correlation test p-values.
#' @seealso \code{\link{cor_mat}()}, \code{\link{cor_test}()}
#' @examples
#' # Pairwise correlation tests between variables
#' #:::::::::::::::::::::::::::::::::::::::::::::::
#' res.cor.test <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_test()
#' res.cor.test
#'
#' # Convert the correlation test into a correlation matrix
#' #:::::::::::::::::::::::::::::::::::::::::::::::
#' res.cor.test %>% as_cor_mat()
#'
#' @export
as_cor_mat <- function(x){

  if(!inherits(x, "cor_test")){
    stop("x should be an object of class cor_test")
  }

  p.mat <- x %>%
    cor_spread(value = "p") %>%
    add_class("pvalue")

  cor.mat <- x %>%
    cor_spread(value = "cor") %>%
    add_class("cor_mat") %>%
    set_attrs(pvalue = p.mat)

  cor.mat
}
