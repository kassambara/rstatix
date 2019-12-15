#' @include utilities.R
NULL
#'Exact Binomial Test
#'
#'@description Performs exact binomial test and pairwise comparisons following a
#'  significant exact multinomial test. Wrapper around the R base function
#'  \code{link[stats]{binom.test}()} that returns a data frame as a result.
#'
#'@inheritParams stats::binom.test
#'@param x numeric vector containing the counts.
#'@param p a vector of probabilities of success. The length of p must be the
#'  same as the number of groups specified by x, and its elements must be
#'  greater than 0 and less than 1.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@seealso \link{multinom_test}
#'@return return a data frame containing the p-value and its significance. with
#'  some the following columns: \itemize{ \item \code{group, group1, group2}:
#'  the categories or groups being compared. \item \code{statistic}: the number
#'  of successes. \item \code{parameter}: the number of trials. \item \code{p}:
#'  p-value of the test. \item \code{p.adj}: the adjusted p-value. \item
#'  \code{method}: the used statistical test. \item \code{p.signif,
#'  p.adj.signif}: the significance level of p-values and adjusted p-values,
#'  respectively. \item \code{estimate}: the estimated probability of success.
#'  \item \code{alternative}: a character string describing the alternative
#'  hypothesis. \item \code{conf.low,conf.high}: Lower and upper bound on a
#'  confidence interval  for the probability of success.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'
#' @examples
#' # Exact binomial test
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: 160 mice with cancer including 95 male and 65 female
#' # Q1: Does cancer affect more males than females?
#' binom_test(x = 95, n = 160)
#' # => yes, there are a significant difference
#'
#'
#' # Q2: compare the observed proportion of males
#' # to an expected proportion (p = 3/5)
#' binom_test(x = 95, n = 160, p = 3/5)
#' # => there are no significant difference
#'
#' # Multinomial test
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data
#' tulip <- c(red = 81, yellow = 50, white = 27)
#' # Question 1: are the color equally common ?
#' # this is a test of homogeneity
#' res <- multinom_test(tulip)
#' res
#' attr(res, "descriptives")
#'
#' # Pairwise comparisons between groups
#' pairwise_binom_test(tulip, p.adjust.method = "bonferroni")
#'
#'
#' # Question 2: comparing observed to expected proportions
#' # this is a goodness-of-fit test
#' expected.p <- c(red = 0.5, yellow = 0.33, white = 0.17)
#' res <- multinom_test(tulip, expected.p)
#' res
#' attr(res, "descriptives")
#'
#' # Pairwise comparisons against a given probabilities
#' pairwise_binom_test_against_p(tulip, expected.p)

#' @describeIn binom_test performs exact binomial test. Wrapper around the R
#'   base function \code{\link[stats]{binom.test}} that returns a dataframe as a
#'   result.
#' @export
binom_test <- function(x, n, p = 0.5,
                       alternative = "two.sided",
                       conf.level = 0.95, detailed = FALSE){
  args <- as.list(environment()) %>%
    add_item(method = "exact_binom_test")
  if(length(x) == 2) n <- sum(x)
  results <- stats::binom.test(x, n, p, alternative, conf.level) %>%
    tidy() %>%
    rename(p = .data$p.value) %>%
    add_significance("p") %>%
    add_columns(n = n, .before = 1)
  if(!detailed){
    to.keep <- c("n", "estimate", "conf.low", "conf.high", "p", "p.signif")
    results <- results[, to.keep]
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "exact_binom_test"))
}

#' @describeIn binom_test performs pairwise comparisons (binomial test)
#'   following a significant exact multinomial test.
#' @export
pairwise_binom_test <- function(x, p.adjust.method = "holm", alternative = "two.sided",
                                conf.level = 0.95){
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  compare_pair <- function(levs, x){
    levs <- as.character(levs)
    lev1 <- levs[1]
    lev2 <- levs[2]
    binom_test(
      x[lev1], x[lev1] + x[lev2], p = 0.5,
      alternative = alternative, conf.level = conf.level
      ) %>%
      add_columns(group1 = levs[1], group2 = levs[2], .before = 1)
  }
  args <- as.list(environment()) %>%
    add_item(method = "exact_binom_test")
  comparisons <- names(x) %>%
    .possible_pairs()
  results <- comparisons %>%
    map(compare_pair, x) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    select(-.data$p.signif)
    # select(.data$group1, .data$group2, .data$p, .data$p.adj, .data$p.adj.signif)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "exact_binom_test"))
}

#' @describeIn binom_test performs pairwise comparisons (binomial test)
#'   following a significant exact multinomial test for given probabilities.
#' @export
pairwise_binom_test_against_p <- function(x, p = rep(1/length(x), length(x)), p.adjust.method = "holm",
                                          alternative = "two.sided", conf.level = 0.95){
  if (sum(p) != 1) {
    stop("sum of probabilities must be 1")
  }
  if (length(x) != length(p)) {
    stop("'x' and 'p' lengths differ")
  }
  groups <- names(x)
  if(is.null(groups)) {
    names(groups) <- paste0("grp", 1:length(x))
  }
  if(inherits(x, "table")){
    x <- as.vector(x)
  }
  names(x) <- groups

  args <- as.list(environment()) %>%
    add_item(method = "exact_binom_test")
  input <- data.frame(x = x, n = sum(x), p = p)
  results <- purrr::pmap(
    input, binom_test, alternative = alternative,
    conf.level = conf.level
    ) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    select(-.data$p.signif) %>%
    # select(.data$p, .data$p.adj, .data$p.adj.signif) %>%
    add_columns(group = groups, observed = x, expected = p*sum(x), .before = 1)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "exact_binom_test"))
}


