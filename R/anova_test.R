#' @include utilities.R
NULL
#'Anova Test
#'
#'
#'@description Provides a pipe-friendly framework to perform ANOVA tests.
#'  Wrapper around the function \code{\link[stats]{aov}()}.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{aov}}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{term}: the grouping
#'  variable. \item \code{statistic}: Test statistic (F-statistic) used to compute the
#'  p-value. \item \code{p}: p-value.
#'  \item \code{method}: the statistical test used to compare groups.}
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-way Anova test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% anova_test(len ~ dose)
#'
#' # Two-way Anova test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% anova_test(len ~ supp*dose)
#'
#'@name anova_test
#'@export
anova_test <- function(
  data, formula, ...
)
{

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)

  if(is_grouped_df(data)){
    results <- data %>%
      doo(anova_test, formula, ...)
    return(results)
  }

  term <- statistic <- p <- method <- NULL
  stats::aov(formula, data = data, ...) %>%
    as_tidy_stat() %>%
    filter(term != "Residuals") %>%
    select(term, statistic, p, method) %>%
    add_column(.y. = outcome, .before = "term")
}
