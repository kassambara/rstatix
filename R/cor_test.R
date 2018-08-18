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
#'  two variables or between two different vectors of variables. Using this function, you
#'  can also compute, for example, the correlation between one variable vs many.
#'
#'
#'@inheritParams stats::cor.test
#'@inheritParams stats::cor
#'@param data a data.frame containing the variables.
#'@param vars a character vector containing variable names for correlation
#'  analysis. \itemize{ \item If \code{vars} is NULL, multiple pairwise
#'  correlation tests is performed between all variables in the data. \item If
#'  \code{vars} contain only one variable, a pairwise correlation analysis is
#'  performed between the specified variable vs either all the remaining numeric
#'  variables in the data or variables in \code{vars2} (if specified). \item If
#'  \code{vars} contain two or more variables: i) if \code{vars2} is not
#'  specified, a pairwise correlation analysis is performed between all possible
#'  combinations of variables. ii) if \code{vars2} is specified, each element in
#'  \code{vars} is tested against all elements in \code{vars2}}
#'@param vars2 optional character vector. If specified, each element in
#'  \code{vars} is tested against all elements in \code{vars2}.
#'
#'@param ... other arguments to be passed to the function \code{cor_test()} or
#'  \code{\link[stats]{cor.test}()}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{var1, var2}: the variables used in the correlation test. \item
#'  \code{cor}: the correlation coefficient. \item \code{statistic}: Test
#'  statistic used to compute the p-value. \item \code{p}: p-value. \item
#'  \code{conf.low,conf.high}: Lower and upper bounds on a confidende interval.
#'  \item \code{method}: the method used to compute the statistic.}
#' @seealso cor_mat
#' @examples
#'
#' # Correlation between the specified variable vs
#' # the remaining numeric variables in the data
#' #:::::::::::::::::::::::::::::::::::::::::
#' mtcars %>% cor_test("mpg")
#'
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
  data, vars = NULL, vars2 =  NULL, alternative = "two.sided",
  method = "pearson", conf.level = 0.95,
  use = "pairwise.complete.obs", ...
)
{

  n.vars <- length(vars)
  # Select only numeric columns
  data <- data %>%
    dplyr::select_if(is.numeric)

  if(is.null(vars2)){

    if(is.null(vars)){
      # Pairwise correlation test between all vars in data
      vars <- vars2 <- colnames(data)
    }
    else if(n.vars == 1){
      # Correlation between the specified variable vs
      # all numeric vars in the data
      vars2 <- colnames(data) %>% setdiff(vars)
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

  res <- mcor_test(
    data, x = vars, y = vars2,
    alternative = alternative, method = method,
    conf.level = conf.level, use = use, ...
  )
  structure(res, class = c(class(res), "cor_test") )
}


#' @describeIn cor_test transform a cor_test object into a correlation matrix
#'  format as provided by \code{\link{cor_mat}()}
#'  @param x an object of class \code{cor_test}.
#' @export
as_cor_mat <- function(x){
  cor.mat <- x %>% spread_cor_test(value = "cor")
  attr(cor.mat, "cor_test") <- x
  structure(cor.mat, class = c(class(cor.mat), "cor_mat") )
}



# Helper functions
#:::::::::::::::::::::::::::::::::::::::::::::::::::


# Multiple correlation tests between two vectors of variables.
#++++++++++++++++++++++++++++++++++++++++++++++++++++
# x,y character vectors containing variable names to be used in the
# correlation analysis.
mcor_test <- function(data, x, y, ...){

  variables.grid <- expand.grid(y = y, x = x,  stringsAsFactors = FALSE)
  variables.grid <- variables.grid %>% as.list()
  cor.test <- purrr::pmap_dfr(variables.grid, cor_test_xy, data = data, ...)
  structure(cor.test, class = c(class(cor.test), "cor_test") )
}


# Correlation test between two variables x and y
#++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      p = signif(p, 2),
      method = to_uppercase_first_letter(.method)
    )

  res
}


