#' @include utilities.R as_cor_mat.R
NULL
#'Compute Correlation Matrix with P-values
#'@description Compute correlation matrix with p-values. Numeric columns in the
#'  data are detected and automatically selected for the analysis. You can also
#'  specify variables of interest to be used in the correlation analysis.
#'@inheritParams cor_test
#'@param x an object of class \code{cor_mat}
#'@param vars a character vector containing the variable names of interest.
#'@param ... One or more unquoted expressions (or variable names) separated by
#'  commas. Used to select a variable of interest.
#'@return a data frame
#'@seealso \code{\link{cor_test}()}, \code{\link{cor_reorder}()},
#'  \code{\link{cor_gather}()}, \code{\link{cor_select}()},
#'  \code{\link{cor_as_symbols}()}, \code{\link{pull_triangle}()},
#'  \code{\link{replace_triangle}()}
#' @examples
#' # Data preparation
#' #:::::::::::::::::::::::::::::::::::::::::::
#' mydata <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec)
#' head(mydata, 3)
#'
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Correlation matrix between all variables
#' cor.mat <- mydata %>% cor_mat()
#' cor.mat
#'
#' # Specify some variables of interest
#' mydata %>% cor_mat(mpg, hp, wt)
#'
#' # Or remove some variables in the data
#' # before the analysis
#' mydata %>% cor_mat(-mpg, -hp)
#'
#' # Significance levels
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat %>% cor_get_pval()
#'
#'
#' # Visualize
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Insignificant correlations are marked by crosses
#' cor.mat %>%
#'   cor_reorder() %>%
#'   pull_lower_triangle() %>%
#'   cor_plot(label = TRUE)
#'
#' # Gather/collapse correlation matrix into long format
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat %>% cor_gather()
#'
#'
#'@describeIn cor_mat compute correlation matrix with p-values. Returns a data
#'  frame containing the matrix of the correlation coefficients. The output has
#'  an attribute named "pvalue", which contains the matrix of the correlation
#'  test p-values.
#'@export
cor_mat <- function(data, ..., vars = NULL, method = "pearson",
                    alternative = "two.sided", conf.level = 0.95){


  vars <- data %>% get_selected_vars(..., vars = vars)
  n.vars <- length(vars)
  if(n.vars > 1 & n.vars <= 2){
      stop("At least, 3 variables are required for a correlation matrix. ",
           "Use the function cor_test() for 2 or less variables.", call. = FALSE)
  }

  cor_test(
    data, vars = vars, method = method,
    alternative = alternative, conf.level = conf.level
    ) %>%
    as_cor_mat()
}


#' @describeIn cor_mat compute the correlation matrix but returns only the p-values of the tests.
#' @export
cor_pmat <- function(data, ..., vars = NULL, method = "pearson",
                     alternative = "two.sided", conf.level = 0.95){

  cor_mat(
    data = data, ..., vars = vars, method = method,
    alternative = alternative, conf.level = conf.level
  ) %>%
    cor_get_pval()
}


#' @describeIn cor_mat extract a correlation matrix p-values from an object of
#'   class \code{cor_mat()}.
#' @export
cor_get_pval <- function(x){
  res <- x %>% attr("pvalue")
  if(is.null(res))
    warning("Can't find p-value attributes.", call.= FALSE)
  res
}







