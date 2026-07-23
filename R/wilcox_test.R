#' @include utilities.R utilities_two_sample_test.R
#' @importFrom stats wilcox.test
NULL
#'Wilcoxon Tests
#'
#'
#'@description Provides a pipe-friendly framework to performs one and two sample
#'  Wilcoxon tests.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/two-groups/wilcoxon-test-in-r}{Wilcoxon Test in R}
#'  for a worked walkthrough.
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
#'  If \code{ref.group = "all"}, pairwise two sample tests are performed for
#'  comparing each grouping variable levels against all (i.e. basemean).
#'@param mu a number specifying an optional parameter used to form the null
#'  hypothesis.
#'@param comparisons A list of length-2 vectors specifying the groups of
#'  interest to be compared. For example to compare groups "A" vs "B" and "B" vs
#'  "C", the argument is as follow: \code{comparisons = list(c("A", "B"), c("B",
#'  "C"))}
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@param effect.size logical. Default is FALSE. If TRUE, a rank effect-size
#'  column is added: for an independent-samples test \code{cliff.delta} (Cliff's
#'  delta) and its \code{magnitude}; for a paired test the matched-pairs
#'  \code{rank.biserial} correlation (no magnitude, as no threshold set is
#'  calibrated for it), computed on the same paired differences the test used
#'  (matched by \code{id} when supplied). Not defined for a one-sample test.
#'@param id (optional) character string specifying the column that contains the
#'  sample/subject identifier, used only for a \strong{paired} test
#'  (\code{paired = TRUE}). When supplied, observations of the two compared
#'  groups are matched by \code{id} (instead of by row order), and only subjects
#'  present in both groups are used. For more than two groups, the matching is
#'  done independently for each pairwise comparison, so different comparisons can
#'  be based on different numbers of pairs (per-comparison pairwise deletion).
#'  This makes paired tests work when some observations are missing or the groups
#'  have unequal sizes. The default (\code{id = NULL}) keeps the previous
#'  behaviour (groups paired in row order).
#'@param error.as.na logical. If \code{TRUE}, a comparison that cannot be
#'  computed (for example a group with fewer than two observations, or data that
#'  are essentially constant) returns an \code{NA} result row with a warning
#'  instead of stopping with an error; the other comparisons (or groups, for a
#'  grouped analysis) are still computed. Default is \code{FALSE} (the comparison
#'  errors as before).
#'@param ... other arguments to be passed to the function
#'  \code{\link[stats]{wilcox.test}}.
#'
#'@details - \code{pairwise_wilcox_test()} applies the standard two sample
#'  Wilcoxon test to all possible pairs of groups. This method calls the
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
#'
#'  - a nonparametric confidence interval and an estimator for the pseudomedian
#'  (one-sample case) or for the difference of the location parameters
#'  \code{x-y} is computed, where x and y are the compared samples or groups.
#'  The column \code{estimate} and the confidence intervals are displayed in the
#'  test result when the option \code{detailed = TRUE} is specified in the
#'  \code{wilcox_test()} and \code{pairwise_wilcox_test()} functions. Read more
#'  about the calculation of the estimate in the details section of the R base
#'  function \code{wilcox.test()} documentation by typing \code{?wilcox.test} in
#'  the R console.
#'
#'  - With \code{effect.size = TRUE}, an independent-samples test is annotated
#'  with Cliff's delta and a paired test with the matched-pairs rank-biserial
#'  correlation, \eqn{(R^+ - R^-)/(R^+ + R^-)} over the signed ranks of the
#'  paired differences (Kerby, 2014).
#'
#'@references Kerby, D. S. (2014). The simple difference formula: An approach to
#'  teaching nonparametric correlation. \emph{Comprehensive Psychology}, 3, 11.IT.3.1.
#'
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n,n1,n2}: Sample counts. \item \code{statistic}: Test statistic used
#'  to compute the p-value. \item \code{p}: p-value. \item \code{p.adj}: the
#'  adjusted p-value. \item \code{method}: the statistical test used to compare
#'  groups. \item \code{p.signif, p.adj.signif}: the significance level of
#'  p-values and adjusted p-values, respectively. \item \code{estimate}: an
#'  estimate of the location parameter (Only present if argument \code{detailed
#'  = TRUE}). This corresponds to the pseudomedian (for one-sample case) or to
#'  the difference of the location parameter (for two-samples case). \itemize{
#'  \item The pseudomedian of a distribution \code{F} is the median of the
#'  distribution of \code{(u+v)/2}, where \code{u} and \code{v} are independent, each
#'  with distribution \code{F}. If \code{F} is symmetric, then the pseudomedian
#'  and median coincide. \item Note that in the two-sample case the estimator
#'  for the difference in location parameters does not estimate the difference
#'  in medians (a common misconception) but rather the median of the difference
#'  between a sample from x and a sample from y. } \item \code{conf.low,
#'  conf.high}: a confidence interval for the location parameter. (Only present
#'  if argument conf.int = TRUE.) }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'
#'@note When a \code{ref.group} is specified, the reference group is taken as
#'  \code{group1} and the other group as \code{group2}, and the comparison is
#'  computed as \code{group1} versus \code{group2} (i.e. \code{ref.group} versus
#'  the other group), following the \code{\link[stats]{wilcox.test}} convention.
#'  With \code{detailed = TRUE}, the \code{estimate} is the Hodges-Lehmann
#'  location shift of \code{group1} relative to \code{group2}, so a positive
#'  \code{estimate} means the reference group is shifted higher; flip its sign
#'  (\code{mutate(estimate = -estimate)}) if you want a positive sign to mean
#'  "higher in the non-reference group". (The \code{statistic} is the
#'  rank-sum/signed-rank \code{W}, which is not a signed difference.)
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
#'@describeIn wilcox_test Wilcoxon test
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/two-groups/wilcoxon-test-in-r}{Wilcoxon Test in R}.
#'@export
wilcox_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm",
  paired = FALSE, exact = NULL, alternative = "two.sided",
  mu = 0, conf.level = 0.95, detailed = FALSE, id = NULL, error.as.na = FALSE,
  effect.size = FALSE
)
{
  env <- as.list(environment())
  args <- env %>%
    add_item(method = "wilcox_test")
  if(!isTRUE(effect.size)) args <- remove_item(args, "effect.size")
  params <- env %>%
    remove_item("effect.size") %>%
    remove_null_items() %>%
    # only request the (Hollander-Wolfe) CI/estimate when detailed = TRUE: it is
    # not shown otherwise, and its uniroot step errors on degenerate/all-tied data (#79, #167)
    add_item(conf.int = detailed, method = "wilcox.test")

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(isTRUE(effect.size) && number.of.groups < 2)
    stop("`effect.size = TRUE` requires two or more groups: a rank effect size ",
         "is not defined for a one-sample Wilcoxon test.", call. = FALSE)
  if(!is.null(id) && !is.null(ref.group) && ref.group %in% c("all", ".all.")){
    stop("`id` (paired matching) is not supported with ref.group = 'all': ",
         "pairing subjects against the pooled grand-mean group is not defined.",
         call. = FALSE)
  }
  if(number.of.groups > 2 & !is.null(ref.group)){
    if(ref.group %in% c("all", ".all.")){
      params$data <- create_data_with_all_ref_group(data, outcome, group)
      params$ref.group <- "all"
    }
  }
  test.func <- two_sample_test
  if(number.of.groups > 2) test.func <- pairwise_two_sample_test
  res <- do.call(test.func, params)
  if(isTRUE(effect.size)){
    res <- add_wilcox_effsize(
      res, data, formula, comparisons = comparisons, ref.group = ref.group,
      paired = paired, id = id, number.of.groups = number.of.groups
    )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "wilcox_test"))
}

# Join the Wilcoxon effect size onto a Wilcoxon result. For an INDEPENDENT test
# that is Cliff's delta (`cliff.delta` + Romano `magnitude`); for a PAIRED test
# it is the matched-pairs rank-biserial correlation (`rank.biserial`, no
# magnitude -- no threshold set is calibrated for it), computed on the SAME
# paired differences the test used (id-matched when id is supplied).
add_wilcox_effsize <- function(res, data, formula, comparisons, ref.group,
                               paired, id, number.of.groups){
  if(number.of.groups < 2)
    stop("`effect.size = TRUE` requires two or more groups: a rank effect size ",
         "is not defined for a one-sample Wilcoxon test.", call. = FALSE)
  if(isTRUE(paired)){
    es <- paired_rank_biserial_table(data, formula, comparisons, ref.group, id)
    warn_undefined_rank_biserial(es)
    return(join_effect_size(res, es, "rank.biserial"))
  }
  es <- cliff_delta(
    data, formula, comparisons = comparisons, ref.group = ref.group
  )
  join_effect_size(res, keep_only_tbl_df_classes(es), "cliff.delta")
}

# two_sample_test() suppresses warnings raised inside the stat function, so the
# undefined matched-pairs rank-biserial (all paired differences zero -> NA) is
# reported from the exported surface, mirroring warn_undefined_boot_ci().
warn_undefined_rank_biserial <- function(es){
  effsize.col <- intersect(c("effsize", "rank.biserial"), colnames(es))[1]
  if(is.na(effsize.col)) return(invisible(es))
  undefined <- is.na(es[[effsize.col]])
  if(any(undefined)){
    warning(
      "The matched-pairs rank-biserial correlation is undefined for ",
      sum(undefined), " of ", nrow(es), " comparison(s): all paired ",
      "differences are zero, so there is nothing to rank. It is returned as NA.",
      call. = FALSE
    )
  }
  invisible(es)
}

# Matched-pairs rank-biserial table, one row per comparison, routed through the
# same two_sample_test()/pairwise_two_sample_test() engine (with paired = TRUE
# and, when supplied, id) that the paired Wilcoxon test uses -- so the effect
# size rests on exactly the paired differences the test did. Returns a tibble
# with the identifying columns and an `effsize` column (no magnitude).
paired_rank_biserial_table <- function(data, formula, comparisons, ref.group, id){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  params <- list(
    data = data, formula = formula, method = "rank.biserial",
    paired = TRUE, id = id, ref.group = ref.group,
    comparisons = comparisons, detailed = FALSE
  ) %>% remove_null_items()
  if(number.of.groups > 2 && !is.null(ref.group) && ref.group %in% c("all", ".all.")){
    params$data <- create_data_with_all_ref_group(data, outcome, group)
    params$ref.group <- "all"
  }
  test.func <- two_sample_test
  if(number.of.groups > 2) test.func <- pairwise_two_sample_test
  do.call(test.func, params) %>%
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything()) %>%
    rename(effsize = "estimate") %>%
    keep_only_tbl_df_classes()
}

# The stat function called by two_sample_test()/pairwise_two_sample_test() for a
# paired comparison: the matched-pairs rank-biserial correlation of the Wilcoxon
# signed-rank test. Rank the absolute non-zero paired differences; then
# r = (R+ - R-) / (R+ + R-), the difference in the proportions of favourable and
# unfavourable signed ranks (Kerby, 2014). Equals effectsize::rank_biserial(paired
# = TRUE). x and y are the paired-aligned vectors the engine supplies.
rank.biserial <- function(x, y = NULL, ci = FALSE, conf.level = 0.95,
                          ci.type = "perc", nboot = 1000, ...,
                          boot.parallel = getOption("boot.parallel", "no"),
                          boot.ncpus = getOption("boot.ncpus", 1L)){
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  if(is.null(y))
    stop("The matched-pairs rank-biserial requires two paired samples.", call. = FALSE)
  differences <- (x - y)[is.finite(x - y)]
  estimate <- get_rank_biserial(differences)
  if(ci == TRUE){
    # Percentile bootstrap over the PAIRS (the differences), as for cohens_d()/
    # cliff_delta(): resampling the differences keeps each subject's pair intact.
    pairs.df <- data.frame(diff = differences)
    stat.func <- function(data, subset) get_rank_biserial(data$diff[subset])
    CI <- get_boot_ci(
      pairs.df, stat.func, conf.level = conf.level, type = ci.type,
      nboot = nboot, parallel = boot.parallel, ncpus = boot.ncpus
    )
  }
  RVAL <- list(
    statistic = NA, p.value = NA, method = "Matched-pairs rank-biserial",
    data.name = DNAME, estimate = estimate
  )
  if(ci){
    attr(CI, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CI))
  }
  names(RVAL$estimate) <- "rank-biserial"
  class(RVAL) <- "htest"
  RVAL
}

# Matched-pairs rank-biserial from the paired differences: drop zero differences
# (Wilcoxon convention), rank |differences|, then (R+ - R-)/(R+ + R-). With no
# non-zero difference left the statistic is undefined (0/0): NA rather than an
# error, so the other comparisons of a pairwise call are still computed (same
# contract as the undefined bootstrap CI, #290).
get_rank_biserial <- function(differences){
  differences <- differences[differences != 0]
  if(length(differences) < 1L) return(NA_real_)
  ranks <- rank(abs(differences))
  (sum(ranks[differences > 0]) - sum(ranks[differences < 0])) /
    sum(ranks)
}



#'@describeIn wilcox_test performs pairwise two sample Wilcoxon test.
#'@export
pairwise_wilcox_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", detailed = FALSE, ..., effect.size = FALSE)
  {
  args <- as.list(environment()) %>%
    .add_item(method = "wilcox_test")
  if(!isTRUE(effect.size)) args <- remove_item(args, "effect.size")

  res <- pairwise_two_sample_test(
    data, formula, method = "wilcox.test",
    comparisons = comparisons, ref.group = ref.group,
    p.adjust.method = p.adjust.method, detailed = detailed,
    conf.int = detailed, ...
  )
  if(isTRUE(effect.size)){
    res <- add_wilcox_effsize(
      res, data, formula, comparisons = comparisons, ref.group = ref.group,
      paired = isTRUE(list(...)$paired), id = list(...)$id, number.of.groups = 2
    )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "wilcox_test"))
}
