#' @include utilities.R t_test.R
NULL
#'Dunn's Test of Multiple Comparisons
#'
#'@description Performs Dunn's test for pairwise multiple comparisons of the
#'  ranked data. The mean rank of the different groups is compared. Used for
#'  post-hoc test following Kruskal-Wallis test.
#'@inheritParams t_test
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{statistic}: Test statistic (z-value) used to compute the p-value. \item
#'  \code{p}: p-value. \item \code{p.adj}: the adjusted p-value. \item
#'  \code{method}: the statistical test used to compare groups. \item
#'  \code{p.signif, p.adj.signif}: the significance level of p-values and
#'  adjusted p-values, respectively. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list holding
#'  the test arguments.
#' @references
#' Dunn, O. J. (1964) Multiple comparisons using rank sums Technometrics, 6(3):241-252.
#' @examples
#' ToothGrowth %>% dunn_test(len ~ dose)
#'@export
dunn_test <- function(data, formula, p.adjust.method = "holm"){
  args <- as.list(environment()) %>%
    .add_item(method = "dunn_test")

  if(is_grouped_df(data)){
    results <- data %>%
      doo(dunn_test, formula, p.adjust.method ) %>%
      set_attrs(args = args) %>%
      add_class(c("rstatix_test", "dunn_test"))
    return(results)
  }
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
  PVAL %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "dunn_test"))
}


get_complete_cases <- function(data){
  data %>%
    filter(complete.cases(data))
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
# transform squared matrix into tidy data frame
tidy_squared_matrix <- function(data, value){
  data %>%
    as_tibble(rownames = "group2") %>%
    gather(key = "group1", value = !!value, -.data$group2) %>%
    stats::na.omit() %>% as_tibble() %>%
    select(.data$group1, everything())
}


