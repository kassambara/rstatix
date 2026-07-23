#' @include utilities.R t_test.R
NULL
#'Dunn's Test of Multiple Comparisons
#'
#'@description Performs Dunn's test for pairwise multiple comparisons of the
#'  ranked data. The mean rank of the different groups is compared. Used for
#'  post-hoc test following Kruskal-Wallis test.
#'
#'  The default of the \code{rstatix::dunn_test()} function is to perform a
#'  two-sided Dunn test like the well known commercial softwares, such as SPSS
#'  and GraphPad. This is not the case for some other R packages
#'  (\code{dunn.test} and \code{jamovi}), where the default is to perform
#'  one-sided test. This discrepancy is documented at
#'  \href{https://github.com/kassambara/rstatix/issues/50}{https://github.com/kassambara/rstatix/issues/50}.
#'
#'  If a reference group is specified (via \code{ref.group}), then each of the
#'  remaining group levels is compared only to the reference (control) group, and
#'  the p-value adjustment for multiple comparisons is computed over only these
#'  \code{k - 1} comparisons (instead of all \code{k(k - 1)/2} pairwise
#'  comparisons). Note that this affects the adjusted p-values: it is not
#'  equivalent to filtering the full pairwise result afterwards, which would still
#'  adjust over all pairs.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/kruskal-wallis-test-in-r}{Kruskal-Wallis Test in R}
#'  for a worked walkthrough.
#'
#'@inheritParams t_test
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference (control) group, and the p-value adjustment is
#'  computed over only these comparisons. Note that, unlike \code{t_test()} and
#'  \code{wilcox_test()}, \code{dunn_test()} does not support \code{ref.group =
#'  "all"}.
#'@param effect.size logical. Default is FALSE. If TRUE, an \code{r} column is
#'  added, the effect size \code{r = Z / sqrt(N)} where \code{Z} is Dunn's
#'  z-statistic and \code{N} is the total sample size (the whole-sample rank
#'  variance Dunn's z is standardised on, not the pairwise \code{n1 + n2}). No
#'  magnitude label is attached: there is no threshold set calibrated for it.
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n1,n2}: Sample counts. \item \code{estimate}: mean ranks difference.
#'  \item \code{estimate1, estimate2}: show the mean rank values of the two
#'  groups, respectively. \item \code{statistic}: Test statistic (z-value) used
#'  to compute the p-value. \item \code{p}: p-value. \item \code{p.adj}: the
#'  adjusted p-value. \item \code{method}: the statistical test used to compare
#'  groups. \item \code{p.signif, p.adj.signif}: the significance level of
#'  p-values and adjusted p-values, respectively. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details DunnTest performs the post hoc pairwise multiple comparisons
#'  procedure appropriate to follow up a Kruskal-Wallis test, which is a
#'  non-parametric analog of the one-way ANOVA. The Wilcoxon rank sum test,
#'  itself a non-parametric analog of the unpaired t-test, is possibly
#'  intuitive, but inappropriate as a post hoc pairwise test, because (1) it
#'  fails to retain the dependent ranking that produced the Kruskal-Wallis test
#'  statistic, and (2) it does not incorporate the pooled variance estimate
#'  implied by the null hypothesis of the Kruskal-Wallis test.
#'
#'@references Dunn, O. J. (1964) Multiple comparisons using rank sums
#'  Technometrics, 6(3):241-252.
#' @examples
#' # Simple test
#' ToothGrowth %>% dunn_test(len ~ dose)
#'
#' # Comparison against a reference (control) group
#' # each group is compared to the reference; the p-value
#' # adjustment corrects for only these k - 1 comparisons
#' ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")
#'
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   dunn_test(len ~ dose)
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/kruskal-wallis-test-in-r}{Kruskal-Wallis Test in R}.
#'@export
dunn_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL, detailed = FALSE,
                      effect.size = FALSE){
  if(!is.null(ref.group)) ref.group <- as.character(ref.group)
  args <- as.list(environment()) %>%
    .add_item(method = "dunn_test")
  if(!isTRUE(effect.size)) args <- remove_item(args, "effect.size")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.dunn_test, formula, p.adjust.method, ref.group = ref.group,
          effect.size = effect.size)
  }
  else{
    results <- .dunn_test(data, formula, p.adjust.method, ref.group = ref.group,
                          effect.size = effect.size)
  }

  if(!detailed){
    results <- results %>%
      select(-any_of(c("method", "estimate", "estimate1", "estimate2")))
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "dunn_test"))
}


.dunn_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL,
                       effect.size = FALSE){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups == 1){
    stop("all observations are in the same group")
  }

  data <- data %>%
    select(!!!syms(c(outcome, group))) %>%
    get_complete_cases() %>%
    .as_factor(group)

  x <- data %>% pull(!!outcome)
  g <- data %>% pull(!!group)
  group.size <- data %>% get_group_size(group)
  if (!all(is.finite(g)))
    stop("all group levels must be finite")
  if(!is.null(ref.group)){
    ref.group <- as.character(ref.group)
    if(!(ref.group %in% levels(g))){
      stop(
        "Specified reference group ('", ref.group, "') is not a level of the ",
        "grouping variable. Valid levels are: ",
        paste(levels(g), collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  x.rank <- rank(x)
  mean.ranks <- tapply(x.rank, g, mean, na.rm=TRUE)
  grp.sizes <- tapply(x, g, length)
  n <- length(x)
  C <- get_ties(x.rank, n)

  compare.meanrank <- function(i, j){
    mean.ranks[i] - mean.ranks[j]
  }
  compare.stats <- function(i,j) {
    dif <- mean.ranks[i] - mean.ranks[j]
    A <- n * (n+1) / 12
    B <- (1 / grp.sizes[i] + 1 / grp.sizes[j])
    zval <- dif / sqrt((A - C) * B)
    zval
  }
  compare.levels <- function(i, j) {
    dif <- abs(mean.ranks[i] - mean.ranks[j])
    A <- n * (n+1) / 12
    B <- (1 / grp.sizes[i] + 1 / grp.sizes[j])
    zval <- dif / sqrt((A - C) * B)
    pval <- 2 * stats::pnorm(abs(zval), lower.tail = FALSE)
    pval
  }
  ESTIMATE <- stats::pairwise.table(
    compare.meanrank, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("diff")

  PSTAT <- stats::pairwise.table(
    compare.stats, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("statistic")

  # Assemble the full per-pair table (estimate, statistic, raw p) BEFORE adjusting
  # p-values, so that, when a reference group is specified, the multiple-comparison
  # adjustment is applied to only the retained (k - 1) comparisons (#101).
  PVAL <- stats::pairwise.table(
    compare.levels, levels(g),
    p.adjust.method = "none"
    ) %>%
    tidy_squared_matrix("p") %>%
    mutate(method = "Dunn Test", .y. = outcome) %>%
    add_column(statistic = PSTAT$statistic, .before = "p") %>%
    add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything())

  # Reference group: keep only comparisons against the reference and orient it as
  # group1 (consistent with t_test()/wilcox_test()). The per-pair z-statistic and
  # the two-sided raw p are unchanged; only the set of comparisons (and thus the
  # p-value adjustment) differs.
  if(!is.null(ref.group)){
    PVAL <- PVAL %>%
      filter(.data$group1 == ref.group | .data$group2 == ref.group)
    to.flip <- PVAL$group2 == ref.group
    if(any(to.flip)){
      g1 <- PVAL$group1; g2 <- PVAL$group2
      PVAL$group1[to.flip] <- g2[to.flip]
      PVAL$group2[to.flip] <- g1[to.flip]
      # estimate (mean-rank difference) and statistic (z) are direction-dependent
      PVAL$estimate[to.flip]  <- -PVAL$estimate[to.flip]
      PVAL$statistic[to.flip] <- -PVAL$statistic[to.flip]
    }
  }

  PVAL <- PVAL %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj")

  n1 <- group.size[PVAL$group1]
  n2 <- group.size[PVAL$group2]
  mean.ranks1 <- mean.ranks[PVAL$group1]
  mean.ranks2 <- mean.ranks[PVAL$group2]
  res <- PVAL %>%
    add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    add_column(estimate1 = mean.ranks1, estimate2 = mean.ranks2, .after = "estimate")
  if(isTRUE(effect.size)){
    # Effect size r = Z / sqrt(N), with N the TOTAL number of observations in the
    # design (Dunn's z is standardised on the whole-sample rank variance, not the
    # pairwise n1 + n2 -- using the latter can push r above 1). r inherits the
    # sign of the (ref.group-oriented) z, so it points from group1 to group2. No
    # magnitude column: there is no magnitude threshold calibrated for Dunn's r.
    res <- res %>% mutate(r = .data$statistic / sqrt(n))
  }
  res
}



get_ties <- function(x, n) {
  x.sorted <- sort(x)
  pos <- 1
  tiesum <- 0
  while (pos <= n) {
    val <- x.sorted[pos]
    nt <- length(x.sorted[x.sorted == val])
    pos <- pos + nt
    if (nt > 1){
      tiesum <- tiesum + nt^3  - nt
    }
  }
  tiesum / (12 * (n - 1))
}


