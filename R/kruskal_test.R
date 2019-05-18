#' @include utilities.R
NULL
#'Kruskal-Wallis Test
#'
#'
#'@description Provides a pipe-friendly framework to performs Kruskal-Wallis rank sum test.
#'  Wrapper around the function \code{\link[stats]{kruskal.test}()}.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{kruskal.test}}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{statistic}: the kruskal-wallis rank sum statistic used to compute the
#'  p-value. \item \code{p}: p-value.
#'  \item \code{method}: the statistical test used to compare groups.}
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Kruskal-wallis rank sum test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% kruskal_test(len ~ dose)

#'
#'@name kruskal_test
#'@export
kruskal_test <- function(
  data, formula, ...
)
{

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)

  if(is_grouped_df(data)){
    results <- data %>%
      doo(kruskal_test, formula, ...)
    return(results)
  }

  term <- statistic <- p <- df <- method <- NULL
  stats::kruskal.test(formula, data = data, ...) %>%
    as_tidy_stat() %>%
    select(statistic, df, p, method) %>%
    add_column(.y. = outcome, .before = "statistic")
}
