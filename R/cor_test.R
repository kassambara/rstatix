#' @include utilities.R
#' @importFrom stats as.formula
#' @importFrom stats cor.test
NULL
#'Correlation Test
#'
#'
#'@description Provides a pipe-friendly framework to perform correlation test
#'  between paired samples, using Pearson, Kendall or Spearman method. Wrapper
#'  around the function \code{\link[stats]{cor.test}()}.
#'
#'@inheritParams stats::cor.test
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{var1 ~ var2} where \code{var1} and
#'  \code{var2} are two numeric variables in the data.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{cor.test}}.
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
#' mtcars %>% cor_test(wt ~ mpg)
#'
#' # Grouped data
#' #:::::::::::::::::::::::::::::::::::::::::
#' iris %>%
#'   group_by(Species) %>%
#'  cor_test(Sepal.Width ~ Sepal.Length)
#'
#'@name cor_test
#'@export
cor_test <- function(
  data, formula, alternative = "two.sided", method = "pearson", conf.level = 0.95, ...
)
{

  # Formula variables
  formula.variables <- .extract_formula_variables(formula)
  var1 <- formula.variables[[1]]
  var2 <- formula.variables[[2]]

  if(is_grouped_df(data)){
    . <- NULL
    results <- data %>%
      do(cor_test(data =., formula, ...)) %>%
      ungroup()
    return(results)
  }


  res <- paste0("~ ", var1, " + ", var2) %>%
    as.formula() %>%
    cor.test(
      data = data, alternative = alternative,
      method = method, conf.level = conf.level, ...
      ) %>%
    .as_tidy_stat()

  estimate <- conf.low <- conf.high <- statistic <- p <- cor <- NULL
  .method <- method

  if(method == "pearson"){
    res <- res %>%
      select(estimate, statistic, p, conf.low, conf.high,  method)
  }

  res <- res %>%
    rename(cor = estimate) %>%
    add_column(var1 = var1, var2 = var2, .before = "cor") %>%
    mutate(
      cor = signif(cor, 2),
      method = to_uppercase_first_letter(.method)
      )

  res
}
