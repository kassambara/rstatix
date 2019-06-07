#' @include utilities.R utilities_mean_test.R
#' @importFrom stats wilcox.test
NULL
#'Wilcoxon Tests
#'
#'
#'@description Provides a pipe-friendly framework to performs one and two sample
#'  Wilcoxon tests.
#'@inheritParams stats::wilcox.test
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param paired a logical indicating whether you want a paired test.
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'
#'  If \code{ref.group = "all"}, pairwise two sample Wilcoxon tests are performed for comparing each grouping
#'  variable levels against all (i.e. basemean).
#'@param comparisons A list of length-2 vectors specifying the groups of
#'  interest to be compared. For example to compare groups "A" vs "B" and "B" vs
#'  "C", the argument is as follow: \code{comparisons = list(c("A", "B"), c("B",
#'  "C"))}
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'
#' @param detailed logical value. Default is FALSE. If TRUE, a detailed result is shown.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{wilcox.test}}.
#'
#'@details - \code{pairwise_wilcox_test()} applies the standard two sample Wilcoxon test to
#'  all possible pairs of groups. This method calls the
#'  \code{\link[stats]{wilcox.test}()}, so extra arguments are accepted.
#'
#'
#'  - If a list of comparisons is specified, the result of the pairwise tests is
#'  filtered to keep only the comparisons of interest.The p-value is adjusted
#'  after filtering.
#'
#'  - For a grouped data, if pairwise test is performed, then the p-values are
#'  adjusted for each group level independently.
#'
#'@return return a data frame with some of the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{group1,group2}: the
#'  compared groups in the pairwise tests. \item \code{statistic}: Test
#'  statistic used to compute the p-value. \item \code{p}: p-value. \item
#'  \code{p.adj}: the adjusted p-value. \item \code{method}: the statistical
#'  test used to compare groups. \item \code{p.signif, p.adj.signif}: the
#'  significance level of p-values and adjusted p-values, respectively. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list holding
#'  the test arguments.
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-sample test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% wilcox_test(len ~ 1, mu = 0)
#'
#'
#' # Two-samples unpaired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% wilcox_test(len ~ supp)
#'
#' # Two-samples paired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% wilcox_test (len ~ supp, paired = TRUE)
#'
#' # Compare supp levels after grouping the data by "dose"
#' #::::::::::::::::::::::::::::::::::::::::
#' df %>%
#'   group_by(dose) %>%
#'   wilcox_test(data =., len ~ supp) %>%
#'   adjust_pvalue(method = "bonferroni") %>%
#'   add_significance("p.adj")
#'
#' # pairwise comparisons
#' #::::::::::::::::::::::::::::::::::::::::
#' # As dose contains more than two levels ==>
#' # pairwise test is automatically performed.
#' df %>% wilcox_test(len ~ dose)
#'
#' # Comparison against reference group
#' #::::::::::::::::::::::::::::::::::::::::
#' # each level is compared to the ref group
#' df %>% wilcox_test(len ~ dose, ref.group = "0.5")
#'
#' # Comparison against all
#' #::::::::::::::::::::::::::::::::::::::::
#' df %>% wilcox_test(len ~ dose, ref.group = "all")
#'
#' @describeIn wilcox_test Wilcoxon test
#'@export
wilcox_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm",
  paired = FALSE, exact = NULL, alternative = "two.sided",
  mu = 0, conf.level = 0.95, detailed = FALSE
)
{
  args <- as.list(environment()) %>%
    .add_item(method = "wilcox_test")

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)

  # Case of one sample test
  if(number.of.groups == 1){
    res <- one_sample_wilcox_test(
      data = data, formula = formula, exact = exact,
      alternative = alternative, mu = mu,
      conf.level = conf.level, detailed = detailed
    )
  }
  # Case of two independents or paired groups
  else if (number.of.groups == 2) {
    res <- two_sample_wilcox_test(
      data = data, formula = formula,
      paired = paired, exact = exact,
      alternative = alternative,
      conf.level = conf.level, ref.group = ref.group,
      detailed = detailed
    )
  }
  # Pairwise comparisons
  else if(number.of.groups > 2){

    if(is.null(ref.group))
      res <- pairwise_wilcox_test(
        data = data, formula = formula,
        comparisons = comparisons,
        p.adjust.method = p.adjust.method,
        paired = paired, exact = exact,
        alternative = alternative, conf.level = conf.level,
        detailed = detailed
      )
    else if(ref.group %in% c("all", ".all."))
      res <- one_vs_all_wilcox_test(
        data = data, formula = formula,
        p.adjust.method = p.adjust.method,
        exact = exact, alternative = alternative,
        conf.level = conf.level, detailed = detailed
      )
    else
      res <- pairwise_wilcox_test(
        data = data, formula = formula,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method,
        paired = paired, exact = exact,
        alternative = alternative, conf.level = conf.level,
        detailed = detailed
      )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "wilcox_test"))
}



one_sample_wilcox_test <- function(data, formula, mu = 0, exact = FALSE, ...){
  mean_test(data, formula, method = "wilcox.test", mu = mu, ...)
}

two_sample_wilcox_test <- function(data, formula, paired = FALSE,  ...)
{
  mean_test(data, formula, method = "wilcox.test", paired = paired, ...)
}


#'@describeIn wilcox_test performs pairwise two sample Wilcoxon test.
#'@export
pairwise_wilcox_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", detailed = FALSE, ...)
  {
  args <- as.list(environment()) %>%
    .add_item(method = "wilcox_test")
  res <- mean_test_pairwise(
    data, formula, method = "wilcox.test",
    comparisons = comparisons, ref.group = ref.group,
    p.adjust.method = p.adjust.method, detailed = detailed, ...
  )
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "wilcox_test"))
}


# wilcox_test performs pairwise two sample Wilcoxon test comparing each grouping
#  variable levels against all (i.e. basemean)
one_vs_all_wilcox_test <- function(data, formula, p.adjust.method = "holm", ...)
{

  mean_test_one_vs_all (
    data, formula, method = "wilcox.test",
    p.adjust.method = p.adjust.method, ...
  )
}
