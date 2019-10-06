#' @include utilities.R
NULL
#'Chi-squared Test for Count Data
#'@description Performs chi-squared tests, including goodness-of-fit,
#'  homogeneity and independence tests.
#'@inheritParams  stats::chisq.test
#'@param res.chisq an object of class \code{chisq_test}.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param ... other arguments passed to the function \code{{chisq_test}()}.
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{n}: the number of participants. \item \code{group, group1, group2}:
#'  the categories or groups being compared. \item \code{statistic}: the value
#'  of Pearson's chi-squared test statistic. \item \code{df}: the degrees of
#'  freedom of the approximate chi-squared distribution of the test statistic.
#'  NA if the p-value is computed by Monte Carlo simulation. \item \code{p}:
#'  p-value. \item \code{p.adj}: the adjusted p-value. \item \code{method}: the
#'  used statistical test. \item \code{p.signif, p.adj.signif}: the significance
#'  level of p-values and adjusted p-values, respectively. \item
#'  \code{observed}: observed counts. \item
#'  \code{expected}: the expected counts under the null hypothesis.
#'  }
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'
#' @examples
#' # Chi-square goodness of fit test
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' tulip <- c(red = 81, yellow = 50, white = 27)
#' # Q1: Are the colors equally common?
#' chisq_test(tulip)
#' pairwise_chisq_gof_test(tulip)
#' # Q2: comparing observed to expected proportions
#' chisq_test(tulip, p = c(1/2, 1/3, 1/6))
#' pairwise_chisq_test_against_p(tulip, p = c(1/2, 1/3, 1/6))
#'
#' # Homogeneity of proportions between groups
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: Titanic
#' xtab <- as.table(rbind(
#'   c(203, 118, 178, 212),
#'   c(122, 167, 528, 673)
#' ))
#' dimnames(xtab) <- list(
#'   Survived = c("Yes", "No"),
#'   Class = c("1st", "2nd", "3rd", "Crew")
#' )
#' xtab
#' # Chi-square test
#' chisq_test(xtab)
#' # Compare the proportion of survived between groups
#' pairwise_prop_test(xtab)


#' @describeIn chisq_test performs chi-square tests including goodness-of-fit,
#'   homogeneity and independence tests.
#' @export
chisq_test <- function(x, y = NULL, correct = TRUE,
                       p = rep(1/length(x), length(x)), rescale.p = FALSE,
                       simulate.p.value = FALSE, B = 2000){
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(inherits(x, c("matrix", "table"))) n <- sum(x)
  else n <- length(x)
  res.chisq <- stats::chisq.test(
    x, y,  correct = correct, p = p, rescale.p = rescale.p,
    simulate.p.value = simulate.p.value, B = B
    )
  as_tidy_stat(res.chisq, stat.method = "Chi-square test") %>%
    add_significance("p") %>%
    add_columns(n = n, .before = 1) %>%
    set_attrs(args = args, test = res.chisq) %>%
    add_class(c("rstatix_test", "chisq_test"))
}

#' @describeIn chisq_test perform pairwise comparisons between groups following a global
#'   chi-square goodness of fit test.
#' @export
pairwise_chisq_gof_test <- function(x, p.adjust.method = "holm", ...){
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  compare_pair <- function(levs, x, ...){
    levs <- as.character(levs)
    suppressWarnings(chisq_test(x[levs],  ...)) %>%
      add_columns(group1 = levs[1], group2 = levs[2], .before = "statistic")
  }
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  comparisons <- names(x) %>%
    .possible_pairs()
  results <- comparisons %>%
    map(compare_pair, x, ...) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    select(-.data$p.signif, -.data$method)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "chisq_test"))
}

#' @describeIn chisq_test perform pairwise comparisons after a global
#'   chi-squared test for given probabilities. For each group, the observed and
#'   the expected proportions are shown. Each group is compared to the sum of
#'   all others.
#' @export
pairwise_chisq_test_against_p <- function(x, p = rep(1/length(x), length(x)), p.adjust.method = "holm", ...){
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  if (sum(p) != 1) {
    stop(
      "Make sure that the `p` argument is correctly specified.",
      "sum of probabilities must be 1."
      )
  }
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  results <- list()
  for (i in 1:length(x)) {
    res.chisq <- suppressWarnings(chisq_test(c(x[i], sum(x) - x[i]), p = c(p[i], 1 - p[i]), ...))
    res.desc <- chisq_descriptives(res.chisq)
    res.chisq <- res.chisq %>%
      add_columns(observed = res.desc$observed[1], expected = res.desc$expected[1], .before = 1)
    results[[i]] <- res.chisq
  }
  results <- results %>%
    bind_rows() %>%
    add_columns(group = names(x), .before = 1) %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    select(-.data$p.signif, -.data$method)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "chisq_test"))
}


#' @describeIn chisq_test returns the descriptive statistics of the chi-square
#'   test. These include, observed and expected frequencies, proportions,
#'   residuals and standardized residuals.
#' @export
chisq_descriptives <- function(res.chisq){
  res <- attr(res.chisq, "test") %>% augment()
  colnames(res) <- gsub(pattern = "^\\.", replacement = "", colnames(res))
  res
}

#' @describeIn chisq_test returns the expected counts from the chi-square test result.
#' @export
expected_freq <- function(res.chisq){
  attr(res.chisq, "test")$expected
}

#' @describeIn chisq_test returns the observed counts from the chi-square test result.
#' @export
observed_freq <- function(res.chisq){
  attr(res.chisq, "test")$observed
}

#' @describeIn chisq_test returns the Pearson residuals, \code{(observed - expected) / sqrt(expected)}.
#' @export
pearson_residuals <- function(res.chisq){
  attr(res.chisq, "test")$residuals
}

#' @describeIn chisq_test returns the standardized residuals
#' @export
std_residuals <- function(res.chisq){
  attr(res.chisq, "test")$stdres
}



