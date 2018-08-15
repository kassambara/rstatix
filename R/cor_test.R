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
#'  Available functions include: \itemize{ \item \code{cor_test()}: Correlation
#'  test between two or variables. \item \code{mcor_test()}: Multiple
#'  correlation tests between two vectors of variables. Using this function, you
#'  can compute, for example, the correlation between one variable vs many. }
#'
#'
#'@inheritParams stats::cor.test
#'@inheritParams stats::cor
#'@param data a data.frame containing the variables.
#'@param vars a character vector containing at least two variable names. If
#'  NULL, multiple pairwise correlation tests is performed between all variables
#'  in the data.
#'@param x,y character vectors containing variable names to be used in the
#'  correlation analysis.
#'@param ... other arguments to be passed to the function \code{cor_test()} or
#'  \code{\link[stats]{cor.test}()}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{var1, var2}: the variables used in the correlation test. \item
#'  \code{cor}: the correlation coefficient. \item \code{statistic}: Test
#'  statistic used to compute the p-value. \item \code{p}: p-value. \item
#'  \code{conf.low,conf.high}: Lower and upper bounds on a confidende interval.
#'  \item \code{method}: the method used to compute the statistic.}
#' @examples
#' # Correlation test between two variables
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test(c("wt", "mpg"))
#'
#' # Pairwise correlation between multiple variables
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test(c("wt", "mpg", "disp"))
#'
#' # Grouped data
#' #:::::::::::::::::::::::::::::::::::::::::
#' iris %>%
#'   group_by(Species) %>%
#'  cor_test(c("Sepal.Width", "Sepal.Length"))
#'
#' # Multiple correlation test
#' #:::::::::::::::::::::::::::::::::::::::::
#' # Correlation between one variable vs many
#' mtcars %>% mcor_test(x = "mpg", y = c("disp", "hp", "drat"))
#'
#' # Correlation between two vectors of variables
#' mtcars %>% mcor_test(x = c("mpg", "wt"), y = c("disp", "hp", "drat"))
#'
#'
#'@describeIn cor_test Correlation test between two variables.
#'@export

cor_test <- function(
  data, vars = NULL, alternative = "two.sided",
  method = "pearson", conf.level = 0.95,
  use = "pairwise.complete.obs", ...
)
{

  n.vars <- length(vars)

  # Pairwise correlation test between all vars in data
  if(is.null(vars)){
    # Select only numeric columns
    data <- data %>% dplyr::select_if(is.numeric)
    # Multiple correlation tests
    vars <- colnames(data)
    res <- mcor_test(
      data, x = vars, y = vars,
      alternative = alternative, method = method,
      conf.level = conf.level, use = use, ...
    )
  }

  # Correlation test between two variables
  else if(n.vars == 2){
    res <- cor_test_xy(
      data, x = vars[1], y = vars[2],
      alternative = alternative, method = method,
      conf.level = conf.level, use = use, ...
    )
  }

  # Multiple pairwise correlation between multiple variables
  else if(n.vars >2){
    res <- mcor_test(
      data, x = vars, y = vars,
      alternative = alternative, method = method,
      conf.level = conf.level, use = use, ...
    )
  }

  else{
    stop("At least, two varables needed")
  }

  structure(res, class = c(class(res), "cor_test") )
}


#' @describeIn cor_test Multiple correlation tests between two vectors of variables.
#' @export
mcor_test <- function(data, x, y, ...){

  variables.grid <- expand.grid(y = y, x = x,  stringsAsFactors = FALSE)
  variables.grid <- variables.grid %>% as.list()
  cor.test <- purrr::pmap_dfr(variables.grid, cor_test_xy, data = data, ...)
  structure(cor.test, class = c(class(cor.test), "cor_test") )
}




# Helper functions
#:::::::::::::::::::::::::::::::::::::::::::::::::::

cor_test_xy <- function(
  data, x, y, method = "pearson",
  use = "pairwise.complete.obs", exact = FALSE,  ...
)
{

  # Case of grouped data
  if(is_grouped_df(data)){
    . <- NULL
    results <- data %>%
      do(cor_test_xy(data =., x, y, method = method, use = use, exact = exact, ...)) %>%
      ungroup()
    return(results)
  }

  # Correlation test
  x.value <- data %>% pull(x)
  y.value <- data %>% pull(y)
  res <- cor.test(
    x.value, y.value, method = method,
    exact = exact, use = use, ...
    ) %>%
    .as_tidy_stat()

  # Format the result
  estimate <- conf.low <- conf.high <- statistic <- p <- cor <- NULL
  .method <- method

  if(method == "pearson"){
    res <- res %>%
      select(estimate, statistic, p, conf.low, conf.high,  method)
  }

  res <- res %>%
    rename(cor = estimate) %>%
    add_column(var1 = x, var2 = y, .before = "cor") %>%
    mutate(
      cor = signif(cor, 2),
      method = to_uppercase_first_letter(.method)
    )

  res
}


