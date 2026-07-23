#' @include utilities.R friedman_test.R
NULL
#'Nemenyi Post-Hoc Test for Friedman Rank Sums
#'
#'@description Performs the Nemenyi (Wilcoxon-Nemenyi-McDonald-Thompson) all-pairs
#'  post-hoc test for a two-way balanced complete block design, following a
#'  significant Friedman rank sum test. The treatment rank sums are compared
#'  pairwise and the test statistic is referred to the studentized range
#'  distribution, which already accounts for the multiplicity of the all-pairs
#'  comparisons (so, as for \code{\link{tukey_hsd}()}, there is no separate
#'  p-value adjustment step). It should only be used as a post-hoc procedure when
#'  the Friedman test is itself significant.
#'
#'  The Nemenyi test is the rank-based, repeated-measures analogue of Tukey's
#'  HSD. Unlike \code{\link{friedman_conover_test}()} (the Durbin-Conover test),
#'  it does not borrow the residual rank variance and is therefore more
#'  conservative.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/friedman-test-in-r}{Friedman Test in R}
#'  for a worked walkthrough.
#'
#'@inheritParams friedman_conover_test
#'@param detailed logical value. If TRUE, returns the rank-sum estimate and the
#'  test method in the output.
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared treatments in the pairwise tests. \item
#'  \code{n1,n2}: the number of blocks (subjects) contributing to each treatment.
#'  \item \code{estimate}: the rank-sum difference. \item \code{estimate1,
#'  estimate2}: the rank sums of the two treatments, respectively. \item
#'  \code{statistic}: the studentized-range test statistic. \item \code{p.adj}:
#'  the p-value (already adjusted for multiple comparisons via the studentized
#'  range distribution). \item \code{method}: the statistical test used to
#'  compare groups. \item \code{p.adj.signif}: the significance level of the
#'  adjusted p-values. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details For a balanced complete block design with \eqn{b} blocks and \eqn{k}
#'  treatments, the observations within each block are ranked. Let \eqn{R_j} be
#'  the sum of the within-block ranks for treatment \eqn{j}. The pairwise
#'  statistic for treatments \eqn{i} and \eqn{j} is \deqn{q_{ij} =
#'  \frac{|R_i - R_j|}{\sqrt{b\,k\,(k+1)/12}}} and the p-value is obtained from
#'  the studentized range distribution with \eqn{k} groups and infinite degrees
#'  of freedom.
#'
#'  The p-values match \code{PMCMRplus::frdAllPairsNemenyiTest()}. That function
#'  reports the magnitude of the statistic; the value returned here carries the
#'  sign of the rank-sum difference between the two groups, so the two agree
#'  only where that difference is positive.
#'
#'@references Nemenyi, P. (1963) Distribution-free Multiple Comparisons. PhD
#'  Thesis, Princeton University.
#'
#'  Hollander, M., Wolfe, D. A. (1973) Nonparametric Statistical Methods. Wiley.
#' @seealso \code{\link{friedman_test}}, \code{\link{friedman_conover_test}},
#'   \code{\link{friedman_effsize}}
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/friedman-test-in-r}{Friedman Test in R}.
#' @examples
#' # A balanced complete block design: 3 treatments measured on 6 subjects
#' df <- data.frame(
#'   id        = factor(rep(1:6, 3)),
#'   treatment = factor(rep(c("A", "B", "C"), each = 6)),
#'   score     = c(4, 6, 3, 5, 4, 5,    7, 8, 6, 7, 9, 6,    6, 9, 7, 8, 8, 9)
#' )
#'
#' # Omnibus Friedman test
#' df %>% friedman_test(score ~ treatment | id)
#'
#' # Nemenyi all-pairs post-hoc
#' df %>% friedman_nemenyi_test(score ~ treatment | id)
#'@name friedman_nemenyi_test
#'@export
friedman_nemenyi_test <- function(data, formula, detailed = FALSE){
  args <- as.list(environment()) %>%
    .add_item(method = "friedman_nemenyi_test")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.friedman_nemenyi_test, formula)
  }
  else{
    results <- .friedman_nemenyi_test(data, formula)
  }
  if(!detailed){
    results <- results %>%
      select(-any_of(c("method", "estimate", "estimate1", "estimate2")))
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "friedman_nemenyi_test"))
}


.friedman_nemenyi_test <- function(data, formula){
  vars <- get_friedman_vars(formula)
  outcome <- vars$dv; treatment <- vars$within; block <- vars$wid

  data <- data %>%
    select(!!!syms(c(outcome, treatment, block))) %>%
    .as_factor(treatment)
  treatments <- levels(data %>% pull(!!treatment))
  k <- length(treatments)
  if(k < 2){
    stop("the treatment (within-subjects) factor must have at least two levels")
  }

  block.values <- data %>% pull(!!block)
  treat.values <- factor(data %>% pull(!!treatment), levels = treatments)
  outcome.values <- data %>% pull(!!outcome)
  if(any(is.na(outcome.values))){
    stop(
      "friedman_nemenyi_test() does not allow missing values in the outcome: ",
      "an unreplicated complete block design must have one non-missing ",
      "observation per subject and treatment.",
      call. = FALSE
    )
  }
  counts <- table(block.values, treat.values)
  if(any(counts != 1)){
    stop(
      "friedman_nemenyi_test() requires a balanced complete block design: ",
      "each subject (block) must have exactly one observation per treatment ",
      "(no missing or replicated cells).",
      call. = FALSE
    )
  }
  m <- tapply(outcome.values, list(block.values, treat.values), function(z) z[1])
  m <- m[, treatments, drop = FALSE]
  b <- nrow(m)
  if(b < 2){
    stop(
      "friedman_nemenyi_test() requires at least two blocks (subjects); a ",
      "single block does not provide a meaningful post-hoc comparison.",
      call. = FALSE
    )
  }

  R <- t(apply(m, 1, rank))                 # within-block ranks (ties: average)
  rank.sums <- colSums(R)
  names(rank.sums) <- treatments
  # studentized-range scale: depends only on the design (b, k), so the statistic
  # is always defined (no zero-variance degenerate case).
  denom <- sqrt(b * k * (k + 1) / 12)

  compare.estimate <- function(i, j){
    rank.sums[i] - rank.sums[j]
  }
  compare.stats <- function(i, j){
    (rank.sums[i] - rank.sums[j]) / denom
  }
  compare.levels <- function(i, j){
    # studentized range already accounts for all-pairs multiplicity
    stats::ptukey(abs(compare.stats(i, j)), nmeans = k, df = Inf, lower.tail = FALSE)
  }
  ESTIMATE <- stats::pairwise.table(
    compare.estimate, treatments, p.adjust.method = "none"
  ) %>% tidy_squared_matrix("diff")
  PSTAT <- stats::pairwise.table(
    compare.stats, treatments, p.adjust.method = "none"
  ) %>% tidy_squared_matrix("statistic")

  PVAL <- stats::pairwise.table(
    compare.levels, treatments, p.adjust.method = "none"
  ) %>%
    tidy_squared_matrix("p.adj") %>%
    mutate(method = "Nemenyi test", .y. = outcome) %>%
    add_column(statistic = PSTAT$statistic, .before = "p.adj") %>%
    add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything()) %>%
    add_significance("p.adj")

  rank.sums1 <- rank.sums[PVAL$group1]
  rank.sums2 <- rank.sums[PVAL$group2]
  PVAL %>%
    add_column(n1 = b, n2 = b, .after = "group2") %>%
    add_column(estimate1 = rank.sums1, estimate2 = rank.sums2, .after = "estimate") %>%
    select(all_of(c(".y.", "group1", "group2", "n1", "n2", "estimate", "estimate1", "estimate2", "statistic", "p.adj", "p.adj.signif", "method")))
}
