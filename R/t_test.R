#' @include utilities.R utilities_mean_test.R
NULL
#'T-test
#'
#'
#'@description Provides a pipe-friendly framework to performs one and two sample
#'  t-tests.
#'@inheritParams stats::t.test
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
#'  If \code{ref.group = "all"}, pairwise two sample t-tests are performed for
#'  comparing each grouping variable levels against all (i.e. basemean).
#'
#'@param comparisons A list of length-2 vectors specifying the groups of
#'  interest to be compared. For example to compare groups "A" vs "B" and "B" vs
#'  "C", the argument is as follow: \code{comparisons = list(c("A", "B"), c("B",
#'  "C"))}
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param pool.sd logical value used in the function \code{pairwise_t_test()}.
#'  Switch to allow/disallow the use of a pooled SD.
#'
#'  The \code{pool.sd = TRUE} (default) calculates a common SD for all groups
#'  and uses that for all comparisons (this can be useful if some groups are
#'  small). This method does not actually call t.test, so extra arguments are
#'  ignored. Pooling does not generalize to paired tests so pool.sd and paired
#'  cannot both be TRUE.
#'
#'  If \code{pool.sd = FALSE} the standard two sample t-test is applied to all
#'  possible pairs of groups. This method calls the \code{t.test()}, so extra
#'  arguments, such as \code{var.equal} are accepted.
#'
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{t.test}}.
#'
#'@details
#'
#'- If a list of comparisons is specified, the result of the pairwise tests is
#'filtered to keep only the comparisons of interest. The p-value is adjusted
#'after filtering.
#'
#'- For a grouped data, if pairwise test is performed, then the p-values are
#'adjusted for each group level independently.
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{group1,group2}: the
#'  compared groups in the pairwise tests. \item \code{statistic}: Test
#'  statistic used to compute the p-value. \item \code{df}: degrees of freedom.
#'  \item \code{p}: p-value. \item \code{p.adj}: the adjusted p-value. \item
#'  \code{method}: the statistical test used to compare groups. \item
#'  \code{p.signif, p.adj.signif}: the significance level of p-values and
#'  adjusted p-values, respectively. \item \code{estimate}: estimate of the
#'  effect size. It corresponds to the estimated mean or difference in means
#'  depending on whether it was a one-sample test or a two-sample test. \item
#'  \code{estimate1, estimate2}: show the mean values of the two groups,
#'  respectively, for independent samples t-tests. \item \code{alternative}: a
#'  character string describing the alternative hypothesis. \item
#'  \code{conf.low,conf.high}: Lower and upper bound on a confidence interval. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list holding
#'  the test arguments.
#'
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-sample test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% t_test(len ~ 1, mu = 0)
#'
#'
#' # Two-samples unpaired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% t_test(len ~ supp)
#'
#' # Two-samples paired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% t_test (len ~ supp, paired = TRUE)
#'
#' # Compare supp levels after grouping the data by "dose"
#' #::::::::::::::::::::::::::::::::::::::::
#' df %>%
#'   group_by(dose) %>%
#'   t_test(data =., len ~ supp) %>%
#'   adjust_pvalue(method = "bonferroni") %>%
#'   add_significance("p.adj")
#'
#' # pairwise comparisons
#' #::::::::::::::::::::::::::::::::::::::::
#' # As dose contains more than two levels ==>
#' # pairwise test is automatically performed.
#' df %>% t_test(len ~ dose)
#'
#' # Comparison against reference group
#' #::::::::::::::::::::::::::::::::::::::::
#' # each level is compared to the ref group
#' df %>% t_test(len ~ dose, ref.group = "0.5")
#'
#' # Comparison against all
#' #::::::::::::::::::::::::::::::::::::::::
#' df %>% t_test(len ~ dose, ref.group = "all")
#'
#'@describeIn t_test t test
#'@export
t_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm",
  paired = FALSE, var.equal = FALSE, alternative = "two.sided",
  mu = 0, conf.level = 0.95, detailed = FALSE
)
{
  args <- as.list(environment()) %>%
    .add_item(method = "t_test")
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)

  # Case of one sample test
  if(number.of.groups == 1){
    res <- one_sample_t_test(
      data = data, formula = formula,
      alternative = alternative, mu = mu,
      conf.level = conf.level, detailed = detailed
    )
  }
  # Case of two independents or paired groups
  else if (number.of.groups == 2) {
    res <- two_sample_t_test(
      data = data, formula = formula, paired = paired,
      var.equal = var.equal, alternative = alternative,
      conf.level = conf.level, ref.group = ref.group,
      detailed = detailed
    )
  }
  # Pairwise comparisons
  else if(number.of.groups > 2){

    if(is.null(ref.group))
      res <- pairwise_t_test(
        data = data, formula = formula,
        comparisons = comparisons,
        p.adjust.method = p.adjust.method,
        paired = paired, var.equal = var.equal,
        alternative = alternative, conf.level = conf.level,
        pool.sd = FALSE, detailed = detailed
      )
    else if(ref.group %in% c("all", ".all."))
      res <- one_vs_all_t_test(
        data = data, formula = formula,
        p.adjust.method = p.adjust.method,
        var.equal = var.equal,
        alternative = alternative, conf.level = conf.level,
        detailed = detailed
      )
    else
      res <- pairwise_t_test(
        data = data, formula = formula,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method,
        paired = paired, var.equal = var.equal,
        alternative = alternative, conf.level = conf.level,
        pool.sd = FALSE, detailed = detailed
      )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "t_test"))
}



one_sample_t_test <- function(data, formula, mu = 0, ...){
  mean_test(data, formula, method = "t.test", mu = mu, ...)
}


two_sample_t_test <- function(data, formula, paired = FALSE, ...)
{
  mean_test(data, formula, method = "t.test", paired = paired, ...)
}


#'@describeIn t_test performs pairwise two sample t-test. Wrapper around the R
#'  base function \code{\link[stats]{pairwise.t.test}}.
#'@export
pairwise_t_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", paired = FALSE, pool.sd = !paired,
  detailed = FALSE, ...) {

  args <- c(as.list(environment()), list(...)) %>%
    .add_item(method = "t_test")
  if(paired) pool.sd <- FALSE
  if(pool.sd){
    res <- pairwise_t_test_psd(
      data, formula, comparisons = comparisons, ref.group = ref.group,
      p.adjust.method = p.adjust.method, detailed = detailed, ...
    )
  }
  else{
    res <- mean_test_pairwise(
      data, formula, method = "t.test",
      comparisons = comparisons, ref.group = ref.group,
      p.adjust.method = p.adjust.method, paired = paired,
      detailed = detailed, ...
    )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "t_test"))
  }


pairwise_t_test_psd <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", alternative = "two.sided",
  detailed = FALSE
  )
  {
  . <- NULL
  if(is_grouped_df(data)){
    results <- data %>%
      doo(pairwise_t_test_psd, formula, comparisons,
          ref.group, p.adjust.method, alternative = alternative)
    return(results)
  }

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  # Convert group into factor if this is not already the case
  data <- data %>% .as_factor(group, ref.group = ref.group)
  outcome.values <- data %>% pull(!!outcome)
  group.values <- data %>% pull(!!group)

  # Compute pairwise t-test
  group1 <- group2 <- p.value <- NULL
  results <- stats::pairwise.t.test(
    outcome.values, group.values,
    p.adjust.method = "none", pool.sd = TRUE,
    alternative = alternative
    ) %>%
    tidy() %>%
    select(group2, group1, p.value)
  colnames(results) <- c("group1", "group2", "p")

  results <- results %>%
    mutate(method = "T-test") %>%
    add_column(.y. = outcome, .before = 1)

  # If ref.group specified, keep only comparisons against reference
  if(!is.null(ref.group)){
    results <- results %>%
      filter(group1 == ref.group)
  }

  # If a comparison list is provided, extract the comparisons of interest
  if(!is.null(comparisons)){
    results <- comparisons %>%
      purrr::map_dfr(~ results %>% filter(group1 %in% .x & group2 %in% .x) )
  }

  p <- p.adj <- NULL
  results <- results %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p") %>%
    add_significance("p.adj") %>%
    mutate(
      p = signif(p, digits = 3),
      p.adj = signif(p.adj, digits = 3)
    )
  if(!detailed) results <- remove_details(results, method = "t.test")
  results
}


# t_test performs pairwise two sample t-test comparing each grouping
# variable levels against all (i.e. basemean)
one_vs_all_t_test <- function(data, formula, p.adjust.method = "holm", ...)
{
  mean_test_one_vs_all (
    data, formula, method = "t.test",
    p.adjust.method = p.adjust.method, ...
    )
}
