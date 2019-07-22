#' @include utilities.R utilities_two_sample_test.R
#' @importFrom stats qbinom
#' @importFrom stats pbinom
NULL
#'Sign Test
#'
#'@description Performs one-sample and two-sample sign tests.
#'@inheritParams t_test
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ treatment}.
#'@param mu a single number representing the value of the population median
#'  specified by the null hypothesis.
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'@param ... other arguments passed to the function \code{sign_test()}
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{group1,group2}: the
#'  compared groups in the pairwise tests. \item \code{n,n1,n2}: Sample counts.
#'  \item \code{statistic}: Test statistic used to compute the p-value. That is
#'  the S-statistic (the number of positive differences between the data and the
#'  hypothesized median), with names attribute \code{"S"}. \item \code{df,
#'  parameter}: degrees of freedom. Here, the total number of valid differences.
#'  \item \code{p}: p-value.  \item \code{method}: the statistical test used to
#'  compare groups. \item \code{p.signif, p.adj.signif}: the significance level
#'  of p-values and adjusted p-values, respectively. \item \code{estimate}:
#'  estimate of the effect size. It corresponds to the median of the
#'  differences. \item \code{alternative}: a character string describing the
#'  alternative hypothesis. \item \code{conf.low,conf.high}: Lower and upper
#'  bound on a confidence interval of the estimate. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@note This function is a reimplementation of the function \code{SignTest()}
#'  from the \code{DescTools} package.
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-sample test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% sign_test(len ~ 1, mu = 0)
#'
#'
#' # Two-samples paired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% sign_test(len ~ supp)
#'
#'
#' # Compare supp levels after grouping the data by "dose"
#' #::::::::::::::::::::::::::::::::::::::::
#' df %>%
#'   group_by(dose) %>%
#'   sign_test(data =., len ~ supp) %>%
#'   adjust_pvalue(method = "bonferroni") %>%
#'   add_significance("p.adj")
#'
#' # pairwise comparisons
#' #::::::::::::::::::::::::::::::::::::::::
#' # As dose contains more than two levels ==>
#' # pairwise test is automatically performed.
#' df %>% sign_test(len ~ dose)
#'
#' # Comparison against reference group
#' #::::::::::::::::::::::::::::::::::::::::
#' # each level is compared to the ref group
#' df %>% sign_test(len ~ dose, ref.group = "0.5")
#'
#'
#'@describeIn sign_test Sign test
#'@export
sign_test <- function(data, formula, comparisons = NULL, ref.group = NULL,
                      p.adjust.method = "holm", alternative = "two.sided",
                      mu = 0, conf.level = 0.95, detailed = FALSE){

  args <- as.list(environment()) %>%
    .add_item(method = "sign_test")

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)

  if(!is.null(ref.group)){
    if(ref.group %in% c("all", ".all."))
      stop("ref.group can't be 'all'.")
  }
  # Case of one sample test
  if(number.of.groups == 1){
    res <- one_sample_sign_test(
      data = data, formula = formula,
      alternative = alternative, mu = mu,
      conf.level = conf.level, detailed = detailed
    )
  }
  # Case of two independents or paired groups
  else if (number.of.groups == 2) {
    res <- two_sample_sign_test(
      data = data, formula = formula,
      alternative = alternative,
      conf.level = conf.level, ref.group = ref.group,
      detailed = detailed
    )
  }
  # Pairwise comparisons
  else if(number.of.groups > 2){
      res <- pairwise_sign_test(
        data = data, formula = formula,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method,
        alternative = alternative, conf.level = conf.level,
        detailed = detailed
      )
  }
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "sign_test"))
}



one_sample_sign_test <- function(data, formula, mu = 0, ...){
  two_sample_test(data, formula, method = "sign.test", mu = mu, ...)
}
two_sample_sign_test <- function(data, formula,   ...)
{
  two_sample_test(data, formula, method = "sign.test",  ...)
}


#'@describeIn sign_test performs pairwise two sample Wilcoxon test.
#'@export
pairwise_sign_test <- function(
  data, formula, comparisons = NULL, ref.group = NULL,
  p.adjust.method = "holm", detailed = FALSE, ...)
{
  args <- as.list(environment()) %>%
    .add_item(method = "sign_test")

  res <- pairwise_two_sample_test(
    data, formula, method = "sign.test",
    comparisons = comparisons, ref.group = ref.group,
    p.adjust.method = p.adjust.method, detailed = detailed, ...
  )
  res %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "sign_test"))
}


sign.test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                             mu = 0, conf.level = 0.95, ...) {

  alternative <- match.arg(alternative)
  check_two_samples_test_args(x = x, y = y, mu = mu,  conf.level = conf.level)

  if (!is.null(y)) {
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    METHOD <- "Paired-samples Sign-Test"
    x <- (x - y)
  }
  else {
    DNAME <- deparse(substitute(x))
    x <- x[is.finite(x)]
    METHOD <- "One-sample Sign-Test"
  }

  d <- (x - mu)
  n.valid <- sum(d > 0) + sum(d < 0)
  if(n.valid > 0) {
    RVAL <- stats::binom.test(x=sum(d > 0), n=n.valid, p=0.5, alternative = alternative, conf.level = conf.level )
  } else {
    RVAL <- stats::binom.test(x=1, n=1)
  }

  RVAL$method <- METHOD
  RVAL$data.name <- DNAME
  names(mu) <- if (!is.null(y)) "median difference" else "median"

  names(RVAL$statistic) <- "S"
  RVAL$estimate <- median(d + mu, na.rm=TRUE)
  names(RVAL$parameter) <- "number of differences"
  mci <- get_median_ci(d + mu, conf.level=conf.level, alternative=alternative)
  RVAL$conf.int <- mci
  attr(RVAL$conf.int, "conf.level") = round(attr(mci,"conf.level"), 3)

  names(RVAL$estimate) <- "median of the differences"
  RVAL$null.value <- mu
  class(RVAL) <- "htest"
  return(RVAL)
}



get_median_ci <- function( x, conf.level = 0.95,
                            alternative = c("two.sided", "less", "greater")){
  # http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
  # http://de.scribd.com/doc/75941305/Confidence-Interval-for-Median-Based-on-Sign-Test
  x <- stats::na.omit(x)
  n <- length(x)
  switch( match.arg(alternative)
          , "two.sided" = {
            k <- qbinom(p = (1 - conf.level) / 2, size=n, prob=0.5, lower.tail=TRUE)
            ci <- sort(x)[c(k, n - k + 1)]
            attr(ci, "conf.level") <- 1 - 2 * pbinom(k-1, size=n, prob=0.5)
          }
          , "greater" = {
            k <- qbinom(p = (1 - conf.level), size=n, prob=0.5, lower.tail=TRUE)
            ci <- c(sort(x)[k], Inf)
            attr(ci, "conf.level") <- 1 - pbinom(k-1, size=n, prob=0.5)
          }
          , "less" = {
            k <- qbinom(p = conf.level, size=n, prob=0.5, lower.tail=TRUE)
            ci <- c(-Inf, sort(x)[k])
            attr(ci, "conf.level") <- pbinom(k, size=n, prob=0.5)
          }
  )
  return(ci)
}
