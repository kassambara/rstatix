#' @include utilities.R dunn_test.R
NULL
#'Conover's All-Pairs Rank Comparison Test
#'
#'@description Performs Conover's test (also known as the Conover-Iman test) for
#'  pairwise multiple comparisons of the ranked data, following a significant
#'  Kruskal-Wallis test. It is closely related to \code{\link{dunn_test}()}, but
#'  uses the pooled within-group rank variance and refers the test statistic to a
#'  \emph{t}-distribution (with \eqn{N - k} degrees of freedom) instead of the
#'  standard normal distribution. The Conover-Iman test is generally more
#'  powerful than Dunn's test, but should only be used as a post-hoc procedure
#'  when the Kruskal-Wallis test is itself significant (Conover, 1999).
#'
#'  If a reference group is specified (via \code{ref.group}), then each of the
#'  remaining group levels is compared only to the reference (control) group, and
#'  the p-value adjustment for multiple comparisons is computed over only these
#'  \code{k - 1} comparisons (instead of all \code{k(k - 1)/2} pairwise
#'  comparisons), exactly as for \code{\link{dunn_test}()}.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/kruskal-wallis-test-in-r}{Kruskal-Wallis Test in R}
#'  for a worked walkthrough.
#'
#'@inheritParams dunn_test
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference (control) group, and the p-value adjustment is
#'  computed over only these comparisons. Note that, like \code{dunn_test()},
#'  \code{conover_test()} does not support \code{ref.group = "all"}.
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n1,n2}: Sample counts. \item \code{estimate}: mean ranks difference.
#'  \item \code{estimate1, estimate2}: show the mean rank values of the two
#'  groups, respectively. \item \code{statistic}: Test statistic (t-value) used
#'  to compute the p-value. \item \code{df}: degrees of freedom (\eqn{N - k},
#'  the same for every comparison). \item \code{p}: p-value. \item \code{p.adj}:
#'  the adjusted p-value. \item \code{method}: the statistical test used to
#'  compare groups. \item \code{p.adj.signif}: the significance level of the
#'  adjusted p-values. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details The Conover-Iman pairwise statistic for comparing groups \eqn{i} and
#'  \eqn{j} is \deqn{t_{ij} = \frac{\bar{R}_i - \bar{R}_j}{\sqrt{S^2 \,
#'  \frac{N - 1 - H}{N - k} \left(\frac{1}{n_i} + \frac{1}{n_j}\right)}}} where
#'  \eqn{\bar{R}} are the mean ranks, \eqn{H} is the (tie-corrected)
#'  Kruskal-Wallis statistic, \eqn{N} is the total sample size, \eqn{k} is the
#'  number of groups, and \eqn{S^2} is the variance of the ranks
#'  (\eqn{S^2 = N(N+1)/12} when there are no ties; otherwise
#'  \eqn{S^2 = \frac{1}{N - 1}\left(\sum r^2 - \frac{N(N+1)^2}{4}\right)}). The
#'  statistic is referred to a \emph{t}-distribution with \eqn{N - k} degrees of
#'  freedom.
#'
#'  In the returned table each row is oriented with \eqn{i = } \code{group2} and
#'  \eqn{j = } \code{group1}: \code{estimate} is \eqn{\bar{R}_{group2} -
#'  \bar{R}_{group1}} and \code{statistic} carries its sign, the same convention
#'  as \code{\link{dunn_test}()}.
#'
#'  The results match \code{PMCMRplus::kwAllPairsConoverTest()}.
#'
#'@references Conover, W. J. (1999) Practical Nonparametric Statistics, 3rd
#'  edition. Wiley.
#'
#'  Conover, W. J. and Iman, R. L. (1979) On multiple-comparisons procedures.
#'  Technical Report LA-7677-MS, Los Alamos Scientific Laboratory.
#' @seealso \code{\link{dunn_test}}, \code{\link{kruskal_test}}
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/kruskal-wallis-test-in-r}{Kruskal-Wallis Test in R}.
#' @examples
#' # Simple test
#' ToothGrowth %>% conover_test(len ~ dose)
#'
#' # Comparison against a reference (control) group
#' # each group is compared to the reference; the p-value
#' # adjustment corrects for only these k - 1 comparisons
#' ToothGrowth %>% conover_test(len ~ dose, ref.group = "0.5")
#'
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   conover_test(len ~ dose)
#'@export
conover_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL, detailed = FALSE){
  if(!is.null(ref.group)) ref.group <- as.character(ref.group)
  args <- as.list(environment()) %>%
    .add_item(method = "conover_test")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.conover_test, formula, p.adjust.method, ref.group = ref.group)
  }
  else{
    results <- .conover_test(data, formula, p.adjust.method, ref.group = ref.group)
  }

  if(!detailed){
    results <- results %>%
      select(-any_of(c("method", "estimate", "estimate1", "estimate2")))
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "conover_test"))
}


.conover_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL){
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
  mean.ranks <- tapply(x.rank, g, mean, na.rm = TRUE)
  grp.sizes <- tapply(x, g, length)
  n <- length(x)
  k <- nlevels(g)
  df.value <- n - k

  # Tie-corrected Kruskal-Wallis statistic H and the pooled rank variance S^2,
  # following Conover (1999). S^2 reduces to N(N+1)/12 when there are no ties.
  rank.sums <- tapply(x.rank, g, sum)
  H.uncorrected <- (12 / (n * (n + 1))) * sum(rank.sums^2 / grp.sizes) - 3 * (n + 1)
  ties <- table(x.rank)
  tie.correction <- 1 - sum(ties^3 - ties) / (n^3 - n)
  H <- H.uncorrected / tie.correction
  S2 <- (1 / (n - 1)) * (sum(x.rank^2) - n * (n + 1)^2 / 4)
  pooled.scale <- S2 * ((n - 1 - H) / (n - k))

  # The Conover-Iman statistic is undefined when there is no residual degree of
  # freedom (N - k < 1, e.g. one observation per group) or no rank variability
  # (all values tied, or every group internally constant), in which case the
  # pooled scale is zero or non-finite and the t-ratio would be NaN/Inf. Fail
  # with a clear message rather than a cryptic downstream error.
  if(df.value < 1 || !is.finite(pooled.scale) || pooled.scale <= 0){
    stop(
      "Conover test is undefined for these data: there is no residual variability ",
      "in the ranks (e.g. each group has a single observation, or all values are ",
      "tied). The Kruskal-Wallis test and its post-hoc require N - k >= 1 and some ",
      "within-group rank variation.",
      call. = FALSE
    )
  }

  compare.meanrank <- function(i, j){
    mean.ranks[i] - mean.ranks[j]
  }
  compare.stats <- function(i, j){
    dif <- mean.ranks[i] - mean.ranks[j]
    B <- (1 / grp.sizes[i] + 1 / grp.sizes[j])
    dif / sqrt(pooled.scale * B)
  }
  compare.levels <- function(i, j){
    tval <- compare.stats(i, j)
    2 * stats::pt(abs(tval), df = df.value, lower.tail = FALSE)
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
  # adjustment is applied to only the retained (k - 1) comparisons (cf. dunn_test).
  PVAL <- stats::pairwise.table(
    compare.levels, levels(g),
    p.adjust.method = "none"
  ) %>%
    tidy_squared_matrix("p") %>%
    mutate(method = "Conover test", .y. = outcome, df = df.value) %>%
    add_column(statistic = PSTAT$statistic, .before = "p") %>%
    add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything())

  # Reference group: keep only comparisons against the reference and orient it as
  # group1 (consistent with t_test()/wilcox_test()/dunn_test()). The per-pair
  # t-statistic and the two-sided raw p are unchanged; only the set of comparisons
  # (and thus the p-value adjustment) differs.
  if(!is.null(ref.group)){
    PVAL <- PVAL %>%
      filter(.data$group1 == ref.group | .data$group2 == ref.group)
    to.flip <- PVAL$group2 == ref.group
    if(any(to.flip)){
      g1 <- PVAL$group1; g2 <- PVAL$group2
      PVAL$group1[to.flip] <- g2[to.flip]
      PVAL$group2[to.flip] <- g1[to.flip]
      # estimate (mean-rank difference) and statistic (t) are direction-dependent
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
  PVAL %>%
    add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    add_column(estimate1 = mean.ranks1, estimate2 = mean.ranks2, .after = "estimate") %>%
    select(all_of(c(".y.", "group1", "group2", "n1", "n2", "estimate", "estimate1", "estimate2", "statistic", "df", "p", "p.adj", "p.adj.signif", "method")))
}
