#' @include utilities.R utilities_two_sample_test.R
#' @importFrom stats ks.test
NULL
#'Two-Sample Kolmogorov-Smirnov Test
#'
#'
#'@description Provides a pipe-friendly framework to perform the two-sample
#'  Kolmogorov-Smirnov test, comparing the (empirical) distributions of a numeric
#'  variable between two groups. Wrapper around the R base function
#'  \code{\link[stats]{ks.test}()}.
#'
#'  When the grouping factor contains more than two levels, pairwise
#'  Kolmogorov-Smirnov tests are automatically performed, with p-value
#'  adjustment.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/assumptions/normality-test-in-r}{Normality Test in R}
#'  for a worked walkthrough.
#'@inheritParams stats::ks.test
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with two
#'  or more levels giving the corresponding groups.
#'@param comparisons A list of length-2 vectors specifying the groups of interest
#'  to be compared. For example to compare groups "A" vs "B" and "B" vs "C", the
#'  argument is as follow: \code{comparisons = list(c("A", "B"), c("B", "C"))}.
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group). If \code{ref.group =
#'  "all"}, pairwise two sample tests are performed for comparing each grouping
#'  variable level against all (i.e. basemean).
#'@param p.adjust.method method to adjust p values for multiple comparisons. Used
#'  when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@return return a data frame with some of the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{group1,group2}: the
#'  compared groups in the pairwise tests. \item \code{n1,n2}: sample counts.
#'  \item \code{statistic}: the value of the test statistic \code{D} (the maximum
#'  difference between the two empirical cumulative distribution functions). \item
#'  \code{p}: p-value. \item \code{p.adj}: the adjusted p-value. \item
#'  \code{method}: the statistical test used to compare groups. \item
#'  \code{p.signif, p.adj.signif}: the significance level of p-values and adjusted
#'  p-values, respectively. \item \code{alternative}: the alternative hypothesis.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@seealso \code{\link{wilcox_test}()}, \code{\link{t_test}()}
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/assumptions/normality-test-in-r}{Normality Test in R}.
#' @examples
#' # Two-samples test
#' #:::::::::::::::::::::::::::::::::::::::::
#' ToothGrowth %>% ks_test(len ~ supp)
#'
#' # Pairwise comparisons (more than two groups)
#' #:::::::::::::::::::::::::::::::::::::::::
#' ToothGrowth %>% ks_test(len ~ dose)
#'
#' # Comparison against a reference group
#' #:::::::::::::::::::::::::::::::::::::::::
#' ToothGrowth %>% ks_test(len ~ dose, ref.group = "0.5")
#'@export
ks_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", alternative = "two.sided",
  exact = NULL, detailed = FALSE
)
{
  env <- as.list(environment())
  args <- env %>%
    add_item(method = "ks_test")
  params <- env %>%
    remove_null_items() %>%
    add_item(method = "ks.test")

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group)){
    stop("The Kolmogorov-Smirnov test requires a grouping variable with at ",
         "least two levels; use a formula of the form 'outcome ~ group'.",
         call. = FALSE)
  }
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups == 1){
    stop("The grouping variable must have at least two levels.", call. = FALSE)
  }
  if(number.of.groups > 2 & !is.null(ref.group)){
    if(ref.group %in% c("all", ".all.")){
      params$data <- create_data_with_all_ref_group(data, outcome, group)
      params$ref.group <- "all"
    }
  }
  test.func <- two_sample_test
  if(number.of.groups > 2) test.func <- pairwise_two_sample_test
  do.call(test.func, params) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "ks_test"))
}
