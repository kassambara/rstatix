#' @include utilities.R
NULL
#'Test for Trend in Proportions
#'@description Performs chi-squared test for trend in proportion. This test is
#'  also known as Cochran-Armitage trend test.
#'
#'  Wrappers around the R base function \code{\link[stats]{prop.trend.test}()} but
#'  returns a data frame for easy data visualization.
#'@inheritParams stats::prop.test
#'@param xtab a cross-tabulation (or contingency table) with two columns and
#'  multiple rows (rx2 design). The columns give the counts of successes and
#'  failures respectively.
#' @param score group score. If \code{NULL}, the default is group number.
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{n}: the number of participants.  \item \code{statistic}: the value of
#'  Chi-squared trend test statistic. \item \code{df}: the degrees of
#'  freedom.
#'  \item \code{p}: p-value. \item
#'  \code{method}: the used statistical test. \item \code{p.signif}: the significance level of p-values and adjusted p-values,
#'  respectively.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#' @examples
#' # Proportion of renal stone (calculi) across age
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data
#' xtab <- as.table(rbind(
#'   c(384, 536, 335),
#'   c(951, 869, 438)
#' ))
#' dimnames(xtab) <- list(
#'   stone = c("yes", "no"),
#'   age = c("30-39", "40-49", "50-59")
#' )
#' xtab
#' # Compare the proportion of survived between groups
#' prop_trend_test(xtab)

#' @export
prop_trend_test <- function(xtab, score = NULL){
  args <- as.list(environment()) %>%
    add_item(method = "chisq_trend_test")
  if(is.data.frame(xtab)) xtab <- as.matrix(xtab)

  if(inherits(xtab, c("matrix", "table"))){
    if(ncol(xtab) > 2 & nrow(xtab) == 2) xtab <- t(xtab)
  }
  total <- sum(xtab)
  events <- xtab[, 1]
  trials <- rowSums(xtab)
  if(is.null(score)) score <- seq_along(events)

  results <- stats::prop.trend.test(events, trials, score) %>%
    as_tidy_stat() %>%
    add_significance("p") %>%
    mutate(method = "Chi-square trend test") %>%
    add_columns(n = total, .before = 1) %>%
    select(.data$n, .data$statistic, .data$p, .data$p.signif, everything())

  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "chisq_trend_test"))
}
