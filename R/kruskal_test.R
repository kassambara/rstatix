#' @include utilities.R
NULL
#'Kruskal-Wallis Test
#'
#'
#'@description Provides a pipe-friendly framework to perform Kruskal-Wallis
#'  rank sum test. Wrapper around the function
#'  \code{\link[stats]{kruskal.test}()}.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{kruskal.test}}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{n}: sample count.
#'  \item \code{statistic}: the kruskal-wallis rank sum statistic used to
#'  compute the p-value. \item \code{p}: p-value. \item \code{method}: the
#'  statistical test used to compare groups.}
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
#' # Grouped data
#' df %>%
#'   group_by(supp) %>%
#'   kruskal_test(len ~ dose)

#'@name kruskal_test
#'@export
kruskal_test <- function(data, formula, ...){
  args <- c(as.list(environment()), list(...)) %>%
    .add_item(method = "kruskal_test")
  if(is_grouped_df(data)){
    results <- data %>% doo(.kruskal_test, formula, ...)
  }
  else{
    results <- .kruskal_test(data, formula, ...)
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "kruskal_test"))
}

.kruskal_test <- function(data, formula, ...)
{
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  term <- statistic <- p <- df <- method <- NULL
  # Report the number of observations actually used by the test. kruskal.test()
  # always drops rows with missing outcome/group values via complete.cases()
  # (regardless of any na.action), so n must be the complete-case count, not
  # nrow(data) which would be inflated when the data contain NAs (#224). Forcing
  # na.action = na.omit here makes n match the test's effective sample size
  # irrespective of a global na.action option or one passed via ...; for data
  # without NAs it equals nrow(data), leaving the reported n unchanged.
  n <- nrow(stats::model.frame(formula, data = data, na.action = stats::na.omit))
  stats::kruskal.test(formula, data = data, ...) %>%
    as_tidy_stat() %>%
    select(statistic, df, p, method) %>%
    add_column(.y. = outcome, n = n, .before = "statistic")
}
