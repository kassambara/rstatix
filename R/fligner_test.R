#' @include utilities.R
NULL
#'Fligner-Killeen Test
#'
#'
#'@description Provides a pipe-friendly framework to perform Fligner-Killeen
#'  rank sum test. Wrapper around the function
#'  \code{\link[stats]{fligner.test}()}.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{fligner.test}}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{n}: sample count.
#'  \item \code{statistic}: the Fligner-Killeen rank sum statistic used to
#'  compute the p-value. \item \code{p}: p-value. \item \code{method}: the
#'  statistical test used to compare groups.}
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Fligner-Killeen rank sum test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% fligner_test(len ~ dose)
#'
#' # Grouped data
#' df %>%
#'   group_by(supp) %>%
#'   fligner_test(len ~ dose)

#'@name fligner_test
#'@export
fligner_test <- function(data, formula, ...){
  args <- c(as.list(environment()), list(...)) %>%
    .add_item(method = "fligner_test")
  if(is_grouped_df(data)){
    results <- data %>% doo(.fligner_test, formula, ...)
  }
  else{
    results <- .fligner_test(data, formula, ...)
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "fligner_test"))
}

.fligner_test <- function(data, formula, ...)
{
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  term <- statistic <- p <- df <- method <- NULL
  stats::fligner.test(formula, data = data, ...) %>%
    as_tidy_stat() %>%
    select(statistic, df, p, method) %>%
    add_column(.y. = outcome, n = nrow(data), .before = "statistic")
}
