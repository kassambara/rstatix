#' @include utilities.R friedman_test.R
NULL
#'Conover's All-Pairs Comparisons Test for Friedman Rank Sums
#'
#'@description Performs Conover's all-pairs comparison test (also known as the
#'  Durbin-Conover test) for a two-way balanced complete block design, following
#'  a significant Friedman rank sum test. It is the repeated-measures analogue of
#'  Conover's test for the Kruskal-Wallis design: the
#'  within-block ranks are compared pairwise using the pooled rank variance and
#'  the statistic is referred to a \emph{t}-distribution with \eqn{(b - 1)(k - 1)}
#'  degrees of freedom (\eqn{b} blocks, \eqn{k} treatments). It should only be
#'  used as a post-hoc procedure when the Friedman test is itself significant
#'  (Conover, 1999).
#'
#'  If a reference group is specified (via \code{ref.group}), then each of the
#'  remaining treatments is compared only to the reference (control) treatment,
#'  and the p-value adjustment for multiple comparisons is computed over only
#'  these \code{k - 1} comparisons (as for \code{\link{dunn_test}()}).
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/friedman-test-in-r}{Friedman Test in R}
#'  for a worked walkthrough.
#'
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{a ~ b | c}, where \code{a}
#'  (numeric) is the dependent variable name; \code{b} is the within-subjects
#'  factor variable (the treatment); and \code{c} is the column name containing
#'  the individuals/subjects (block) identifier. Should be unique per individual.
#'@param p.adjust.method method to adjust p-values for multiple comparisons. Used
#'  when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Default is
#'  "holm".
#'@param ref.group a character string specifying the reference treatment. If
#'  specified, each of the treatment levels is compared to the reference
#'  (control), and the p-value adjustment is computed over only these
#'  comparisons.
#'@param detailed logical value. If TRUE, returns the rank-sum estimate and the
#'  test method in the output.
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared treatments in the pairwise tests. \item
#'  \code{n1,n2}: the number of blocks (subjects) contributing to each treatment.
#'  \item \code{estimate}: the rank-sum difference. \item \code{estimate1,
#'  estimate2}: the rank sums of the two treatments, respectively. \item
#'  \code{statistic}: Test statistic (t-value) used to compute the p-value. \item
#'  \code{df}: degrees of freedom (\eqn{(b - 1)(k - 1)}). \item \code{p}: p-value.
#'  \item \code{p.adj}: the adjusted p-value. \item \code{method}: the
#'  statistical test used to compare groups. \item \code{p.adj.signif}: the
#'  significance level of the adjusted p-values. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details For a balanced complete block design with \eqn{b} blocks and \eqn{k}
#'  treatments, the observations within each block are ranked. Let \eqn{R_j} be
#'  the sum of the within-block ranks for treatment \eqn{j} and let \eqn{A =
#'  \sum r^2} be the sum of the squared within-block ranks. The pairwise
#'  statistic for treatments \eqn{i} and \eqn{j} is \deqn{t_{ij} =
#'  \frac{R_i - R_j}{\sqrt{\dfrac{2\,(b\,A - \sum_j R_j^2)}{(b - 1)(k - 1)}}}}
#'  referred to a \emph{t}-distribution with \eqn{(b - 1)(k - 1)} degrees of
#'  freedom. This is the Conover (1999) post-hoc, also known as the
#'  Durbin-Conover test. In the returned table each row is oriented with
#'  \eqn{i = } \code{group2} and \eqn{j = } \code{group1}: \code{estimate} is
#'  \eqn{R_{group2} - R_{group1}} and \code{statistic} carries its sign, the
#'  same convention as \code{\link{conover_test}()} and
#'  \code{\link{dunn_test}()}.
#'
#'  The p-values match \code{PMCMRplus::frdAllPairsConoverTest()}. That function
#'  reports the \emph{t} statistic for the reversed comparison, so its sign is
#'  the opposite of the one returned here; the magnitude is the same.
#'
#'@references Conover, W. J. (1999) Practical Nonparametric Statistics, 3rd
#'  edition. Wiley.
#' @seealso \code{\link{friedman_test}}, \code{\link{friedman_nemenyi_test}},
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
#' # Conover (Durbin-Conover) all-pairs post-hoc
#' df %>% friedman_conover_test(score ~ treatment | id)
#'
#' # Comparison against a reference (control) treatment
#' df %>% friedman_conover_test(score ~ treatment | id, ref.group = "A")
#'@name friedman_conover_test
#'@export
friedman_conover_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL, detailed = FALSE){
  if(!is.null(ref.group)) ref.group <- as.character(ref.group)
  args <- as.list(environment()) %>%
    .add_item(method = "friedman_conover_test")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.friedman_conover_test, formula, p.adjust.method, ref.group = ref.group)
  }
  else{
    results <- .friedman_conover_test(data, formula, p.adjust.method, ref.group = ref.group)
  }
  if(!detailed){
    results <- results %>%
      select(-any_of(c("method", "estimate", "estimate1", "estimate2")))
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "friedman_conover_test"))
}


.friedman_conover_test <- function(data, formula, p.adjust.method = "holm", ref.group = NULL){
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
  if(!is.null(ref.group)){
    if(!(ref.group %in% treatments)){
      stop(
        "Specified reference group ('", ref.group, "') is not a level of the ",
        "treatment variable. Valid levels are: ",
        paste(treatments, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  # Build the balanced complete-block matrix (rows = blocks, cols = treatments).
  # The Durbin-Conover post-hoc requires every block to have exactly one
  # observation per treatment (no missing or duplicated cells).
  block.values <- data %>% pull(!!block)
  treat.values <- factor(data %>% pull(!!treatment), levels = treatments)
  outcome.values <- data %>% pull(!!outcome)
  # A missing outcome would still count as a present cell below (table() counts
  # rows, not non-NA values) and rank() would then assign the missing cell a
  # rank, silently corrupting the rank sums. Reject NA outcomes outright, as the
  # omnibus friedman_test() does (an unreplicated complete block design has no
  # missing cells).
  if(any(is.na(outcome.values))){
    stop(
      "friedman_conover_test() does not allow missing values in the outcome: ",
      "an unreplicated complete block design must have one non-missing ",
      "observation per subject and treatment.",
      call. = FALSE
    )
  }
  counts <- table(block.values, treat.values)
  if(any(counts != 1)){
    stop(
      "friedman_conover_test() requires a balanced complete block design: ",
      "each subject (block) must have exactly one observation per treatment ",
      "(no missing or replicated cells).",
      call. = FALSE
    )
  }
  m <- tapply(outcome.values, list(block.values, treat.values), function(z) z[1])
  m <- m[, treatments, drop = FALSE]
  b <- nrow(m)

  R <- t(apply(m, 1, rank))                 # within-block ranks (ties: average)
  rank.sums <- colSums(R)
  names(rank.sums) <- treatments
  A <- sum(R^2)
  df.value <- (b - 1) * (k - 1)
  pooled <- 2 * (b * A - sum(rank.sums^2)) / df.value

  # The statistic is undefined when there is no residual rank variability
  # (e.g. every block ranks the treatments in exactly the same order, so the
  # denominator collapses to zero) or when there is only a single block.
  if(df.value < 1 || !is.finite(pooled) || pooled <= 0){
    stop(
      "Conover (Durbin-Conover) test is undefined for these data: there is no ",
      "residual variability in the within-block ranks (e.g. every subject ranks ",
      "the treatments in the same order), or there are too few blocks.",
      call. = FALSE
    )
  }
  denom <- sqrt(pooled)

  compare.estimate <- function(i, j){
    rank.sums[i] - rank.sums[j]
  }
  compare.stats <- function(i, j){
    (rank.sums[i] - rank.sums[j]) / denom
  }
  compare.levels <- function(i, j){
    2 * stats::pt(abs(compare.stats(i, j)), df = df.value, lower.tail = FALSE)
  }
  ESTIMATE <- stats::pairwise.table(
    compare.estimate, treatments, p.adjust.method = "none"
  ) %>% tidy_squared_matrix("diff")
  PSTAT <- stats::pairwise.table(
    compare.stats, treatments, p.adjust.method = "none"
  ) %>% tidy_squared_matrix("statistic")

  # Assemble the full per-pair table before adjusting p-values, so that with a
  # reference group the adjustment is applied over only the retained k - 1
  # comparisons (cf. dunn_test()/conover_test()).
  PVAL <- stats::pairwise.table(
    compare.levels, treatments, p.adjust.method = "none"
  ) %>%
    tidy_squared_matrix("p") %>%
    mutate(method = "Durbin-Conover", .y. = outcome, df = df.value) %>%
    add_column(statistic = PSTAT$statistic, .before = "p") %>%
    add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything())

  if(!is.null(ref.group)){
    PVAL <- PVAL %>%
      filter(.data$group1 == ref.group | .data$group2 == ref.group)
    to.flip <- PVAL$group2 == ref.group
    if(any(to.flip)){
      g1 <- PVAL$group1; g2 <- PVAL$group2
      PVAL$group1[to.flip] <- g2[to.flip]
      PVAL$group2[to.flip] <- g1[to.flip]
      PVAL$estimate[to.flip]  <- -PVAL$estimate[to.flip]
      PVAL$statistic[to.flip] <- -PVAL$statistic[to.flip]
    }
  }

  PVAL <- PVAL %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj")

  rank.sums1 <- rank.sums[PVAL$group1]
  rank.sums2 <- rank.sums[PVAL$group2]
  PVAL %>%
    add_column(n1 = b, n2 = b, .after = "group2") %>%
    add_column(estimate1 = rank.sums1, estimate2 = rank.sums2, .after = "estimate") %>%
    select(all_of(c(".y.", "group1", "group2", "n1", "n2", "estimate", "estimate1", "estimate2", "statistic", "df", "p", "p.adj", "p.adj.signif", "method")))
}
