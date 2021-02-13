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
#'@inheritParams t_test
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
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   dunn_test(len ~ dose)
#'@export
dunn_test <- function(data, formula, p.adjust.method = "holm", detailed = FALSE){
  args <- as.list(environment()) %>%
    .add_item(method = "dunn_test")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.dunn_test, formula, p.adjust.method )
  }
  else{
    results <- .dunn_test(data, formula, p.adjust.method)
  }

  if(!detailed){
    results <- results %>%
      select(-.data$method, -.data$estimate, -.data$estimate1, -.data$estimate2)
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "dunn_test"))
}


.dunn_test <- function(data, formula, p.adjust.method = "holm"){
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

  PVAL <- stats::pairwise.table(
    compare.levels, levels(g),
    p.adjust.method = "none"
    ) %>%
    tidy_squared_matrix("p") %>%
    mutate(method = "Dunn Test", .y. = outcome) %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    add_column(statistic = PSTAT$statistic, .before = "p") %>%
    add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    select(.data$.y., .data$group1, .data$group2, .data$estimate, everything())

  n1 <- group.size[PVAL$group1]
  n2 <- group.size[PVAL$group2]
  mean.ranks1 <- mean.ranks[PVAL$group1]
  mean.ranks2 <- mean.ranks[PVAL$group2]
  PVAL %>%
    add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    add_column(estimate1 = mean.ranks1, estimate2 = mean.ranks2, .after = "estimate")
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


