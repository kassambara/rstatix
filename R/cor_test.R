#' @include utilities.R
#' @importFrom stats as.formula
#' @importFrom stats cor.test
#'
NULL
#'Correlation Test
#'
#'
#'@description Provides a pipe-friendly framework to perform correlation test
#'  between paired samples, using Pearson, Kendall or Spearman method. Wrapper
#'  around the function \code{\link[stats]{cor.test}()}.
#'
#'  Can also performs multiple pairwise correlation analyses between more than
#'  two variables or between two different vectors of variables. Using this
#'  function, you can also compute, for example, the correlation between one
#'  variable vs many.
#'
#'
#'@inheritParams stats::cor.test
#'@inheritParams stats::cor
#'@param data a data.frame containing the variables.
#'@param vars optional character vector containing variable names for
#'  correlation analysis. Ignored when dot vars are specified. \itemize{ \item
#'  If \code{vars} is NULL, multiple pairwise correlation tests is performed
#'  between all variables in the data. \item If \code{vars} contain only one
#'  variable, a pairwise correlation analysis is performed between the specified
#'  variable vs either all the remaining numeric variables in the data or
#'  variables in \code{vars2} (if specified). \item If \code{vars} contain two
#'  or more variables: i) if \code{vars2} is not specified, a pairwise
#'  correlation analysis is performed between all possible combinations of
#'  variables. ii) if \code{vars2} is specified, each element in \code{vars} is
#'  tested against all elements in \code{vars2}}. Accept unquoted
#'  variable names: \code{c(var1, var2)}.
#'@param vars2 optional character vector. If specified, each element in
#'  \code{vars} is tested against all elements in \code{vars2}. Accept unquoted
#'  variable names: \code{c(var1, var2)}.
#'@param ... One or more unquoted expressions (or variable names) separated by
#'  commas. Used to select a variable of interest. Alternative to the argument
#'  \code{vars}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{var1, var2}: the variables used in the correlation test. \item
#'  \code{cor}: the correlation coefficient. \item \code{statistic}: Test
#'  statistic used to compute the p-value. \item \code{p}: p-value. \item
#'  \code{conf.low,conf.high}: Lower and upper bounds on a confidence interval.
#'  \item \code{method}: the method used to compute the statistic.}
#'@seealso \code{\link{cor_mat}()}, \code{\link{as_cor_mat}()}
#' @examples
#'
#' # Correlation between the specified variable vs
#' # the remaining numeric variables in the data
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test(mpg)
#'
#' # Correlation test between two variables
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test(wt, mpg)
#'
#' # Pairwise correlation between multiple variables
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test(wt, mpg, disp)
#'
#' # Grouped data
#' #:::::::::::::::::::::::::::::::::::::::::
#' iris %>%
#'   group_by(Species) %>%
#'  cor_test(Sepal.Width, Sepal.Length)
#'
#' # Multiple correlation test
#' #:::::::::::::::::::::::::::::::::::::::::
#' # Correlation between one variable vs many
#' mtcars %>% cor_test(
#'   vars = "mpg",
#'   vars2 = c("disp", "hp", "drat")
#'  )
#'
#' # Correlation between two vectors of variables
#' # Each element in vars is tested against all elements in vars2
#' mtcars %>% cor_test(
#'   vars = c("mpg", "wt"),
#'   vars2 = c("disp", "hp", "drat")
#'  )
#'
#'
#'@describeIn cor_test correlation test between two or more variables.
#'@export

cor_test <- function(
  data, ..., vars = NULL, vars2 =  NULL, alternative = "two.sided",
  method = "pearson", conf.level = 0.95,
  use = "pairwise.complete.obs"
)
{
  . <- NULL
  # Accept unquoted variables
  .args <- rlang::enquos(vars = vars, vars2 = vars2) %>%
    get_quo_vars_list(data, .)
  vars <- .args$vars
  vars2 <- .args$vars2

  vars <- data %>% get_selected_vars(..., vars = vars)
  n.vars <- length(vars)

  # Select only numeric columns
  data.numeric <- data %>%
    select_numeric_columns()

  if(is.null(vars2)){

    if(is.null(vars)){
      # Pairwise correlation test between all vars in data
      vars <- vars2 <- colnames(data.numeric)
    }
    else if(n.vars == 1){
      # Correlation between the specified variable vs
      # all numeric vars in the data
      vars2 <- colnames( data.numeric) %>% setdiff(vars)
    }
    else if(n.vars == 2){
      # Correlation test between two variables
      vars2 <- vars[2]
      vars <- vars[1]
    }
    # Multiple pairwise correlation between multiple variables
    else if(n.vars >2){
      vars2 <- vars
    }
  }
  else if(is.null(vars)){
    stop("You should specify the argument vars in addition to vars2")
  }

  # Multiple correlation tests between two vectors of variables.
  expand.grid(y = vars2, x = vars,  stringsAsFactors = FALSE) %>%
    as.list() %>%
    purrr::pmap_dfr(
      cor_test_xy, data = data, alternative = alternative,
      method = method, conf.level = conf.level, use = use
      ) %>%
    add_class("cor_test")
}



#:::::::::::::::::::::::::::::::::::::::::::::::::::
# Helper functions
#:::::::::::::::::::::::::::::::::::::::::::::::::::

# Correlation test between two variables x and y
#++++++++++++++++++++++++++++++++++++++++++++++++++++
cor_test_xy <- function(
  data, x, y, method = "pearson",
  use = "pairwise.complete.obs", ...
)
{
  if(is_grouped_df(data)){
    results <- data %>%
      doo(cor_test_xy, x, y, method = method, use = use, ...)
    return(results)
  }
  # Correlation test, supress the warning when method = "spearman" or "kendall".
  suppressWarnings(cor.test(data[[x]], data[[y]], method = method, use = use, ...)) %>%
    as_tidy_cor() %>%
    add_column(var1 = x, var2 = y, .before = "cor")
}



# Multiple correlation tests between two vectors of variables.
#++++++++++++++++++++++++++++++++++++++++++++++++++++
# x,y character vectors containing variable names to be used in the
# correlation analysis.
mcor_test <- function(data, x, y, ...){
  expand.grid(y = y, x = x,  stringsAsFactors = FALSE) %>%
    as.list() %>%
    purrr::pmap_dfr(cor_test_xy, data = data, ...) %>%
    add_class("cor_test")
}


# Tidy output for correlation test
as_tidy_cor <- function(x){

  estimate <- cor <- statistic <- p <-
    conf.low <- conf.high <- method <- NULL
  res <- x %>%
    as_tidy_stat() %>%
    rename(cor = estimate) %>%
    mutate(cor = signif(cor, 2))
  if(res$method == "Pearson"){
    res %>% select(cor, statistic, p, conf.low, conf.high, method)
  }
  else {
    res %>% select(cor, statistic, p, method)
  }

}



