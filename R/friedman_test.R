#' @include utilities.R
NULL
#'Friedman Rank Sum Test
#'
#'
#'@description Provides a pipe-friendly framework to perform a Friedman rank sum
#'  test, which is the non-parametric alternative to the one-way repeated
#'  measures ANOVA test. Wrapper around the function
#'  \code{\link[stats]{friedman.test}()}. Read more:
#'  \href{https://www.datanovia.com/en/lessons/friedman-test-in-r/}{Friedman
#'  test in R}.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{a ~ b | c}, where \code{a}
#'  (numeric) is the dependent variable name; \code{b} is the within-subjects
#'  factor variables; and \code{c} (factor) is the column name containing
#'  individuals/subjects identifier. Should be unique per individual.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{friedman.test}}.
#'
#'@return return a data frame with the following columns: \itemize{ \item
#'  \code{.y.}: the y (dependent) variable used in the test. \item \code{n}:
#'  sample count. \item \code{statistic}: the value of Friedman's chi-squared
#'  statistic, used to compute the p-value. \item \code{p}: p-value. \item
#'  \code{method}: the statistical test used to compare groups.}
#'
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth %>%
#'     filter(supp == "VC") %>%
#'     mutate(id = rep(1:10, 3))
#' head(df)
#'
#' # Friedman rank sum test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% friedman_test(len ~ dose | id)
#'
#'@name friedman_test
#'@export
friedman_test <- function(data, formula, ...){
  args <- c(as.list(environment()), list(...)) %>%
    add_item(method = "friedman_test")
  vars <- get_friedman_vars(formula)
  args <- args %>% add_item(dv = vars$dv, wid = vars$wid, within = args$within)
  if(is_grouped_df(data)){
    results <- data %>% doo(.friedman_test, formula, ...)
  }
  else{
    results <- .friedman_test(data, formula, ...)
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "friedman_test"))
}


.friedman_test <- function(data, formula, ...){
  vars <- get_friedman_vars(formula)
  term <- statistic <- p <- df <- method <- NULL
  sample.size <- data %>% pull(vars$wid) %>% unique() %>% length()
  res <- stats::friedman.test(formula, data = data, ...) %>%
    tidy() %>%
    rename(p = .data$p.value, df = .data$parameter) %>%
    mutate(method = "Friedman test") %>%
    select(.data$statistic, .data$df, .data$p, .data$method)  %>%
    add_columns(.y. = vars$dv, n = sample.size, .before = "statistic")
  res
}


get_friedman_vars <- function(formula){
  outcome <- get_formula_left_hand_side(formula)
  rhs <- get_formula_right_hand_side(formula)
  rhs <- gsub(pattern = " ", replacement = "", rhs)
  rhs <- strsplit(rhs, "|", fixed = TRUE) %>% unlist()
  list(dv = outcome, within = rhs[1],  wid = rhs[2])
}
