#' @include utilities.R utilities_two_sample_test.R
#' @importFrom stats sd
#' @importFrom stats var
NULL

#'Compute Cohen's d Measure of Effect Size
#'
#'@description Compute the effect size for t-test. T-test conventional effect
#'  sizes, proposed by Cohen, are: 0.2 (small effect), 0.5 (moderate effect) and
#'  0.8 (large effect).
#'
#'  Cohen's \code{d} is calculated as the difference between means or mean minus
#'  \code{mu} divided by the estimated standardized deviation.
#'
#'  For independent samples t-test, there are two possibilities implemented. If
#'  the t-test did not make a homogeneity of variance assumption, (the Welch
#'  test), the variance term will mirror the Welch test, otherwise a pooled
#'  estimate is used.
#'
#'  If a paired samples t-test was requested, then effect size desired is based
#'  on the standard deviation of the differences.
#'
#'  It can also returns confidence intervals by bootstap.
#'
#'@inheritParams wilcox_effsize
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param paired a logical indicating whether you want a paired test.
#'@param mu theoretical mean, use for one-sample t-test. Default is 0.
#'@param var.equal a logical variable indicating whether to treat the two
#'  variances as being equal. If TRUE then the pooled variance is used to
#'  estimate the variance otherwise the Welch (or Satterthwaite) approximation
#'  to the degrees of freedom is used. Used only for unpaired or independent samples test.
#'@param hedges.correction logical indicating whether apply the Hedges
#'  correction by multiplying the usual value of Cohen's d by
#'  \code{(N-3)/(N-2.25)} (for unpaired t-test) and by \code{(n1-2)/(n1-1.25)} for paired t-test;
#'  where \code{N} is the total size of the two groups being compared (N = n1 +
#'  n2).
#'@details Quantification of the effect size magnitude is performed using the
#'  thresholds defined in Cohen (1992). The magnitude is assessed using the
#'  thresholds provided in (Cohen 1992), i.e. \code{|d| < 0.2} "negligible",
#'  \code{|d| < 0.5} "small", \code{|d| < 0.8} "medium", otherwise "large".
#'@references \itemize{ \item Cohen, J. (1988). Statistical power analysis for
#'  the behavioral sciences (2nd ed.). New York:Academic Press. \item Cohen, J.
#'  (1992). A power primer. Psychological Bulletin, 112, 155-159. \item Hedges,
#'  Larry & Olkin, Ingram. (1985). Statistical Methods in Meta-Analysis.
#'  10.2307/1164953. \item Navarro, Daniel. 2015. Learning Statistics with R: A
#'  Tutorial for Psychology Students and Other Beginners (Version 0.5). }
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n,n1,n2}: Sample counts. \item \code{effsize}: estimate of the effect
#'  size (\code{d} value). \item \code{magnitude}: magnitude of effect size.
#'  \item \code{conf.low,conf.high}: lower and upper bound of the effect size
#'  confidence interval.}
#' @examples
#' # One-sample t test effect size
#' ToothGrowth %>% cohens_d(len ~ 1, mu = 0)
#'
#' # Two indepedent samples t-test effect size
#' ToothGrowth %>% cohens_d(len ~ supp, var.equal = TRUE)
#'
#' # Paired samples effect size
#' df <- data.frame(
#'   id = 1:5,
#'   pre  = c(110, 122, 101, 120, 140),
#'   post = c(150, 160, 110, 140, 155)
#' )
#' df <- df %>% gather(key = "treatment", value = "value", -id)
#' head(df)
#'
#' df %>% cohens_d(value ~ treatment, paired = TRUE)
#'@export
cohens_d <- function(data, formula, comparisons = NULL, ref.group = NULL, paired = FALSE, mu = 0,
                     var.equal = FALSE, hedges.correction = FALSE,
                     ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000){
  env <- as.list(environment())
  args <- env %>% .add_item(method = "cohens_d")
  params <- env %>%
    remove_null_items() %>%
    add_item(method = "cohens.d", detailed = FALSE)

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups > 2 & !is.null(ref.group)){
    if(ref.group %in% c("all", ".all.")){
      params$data <- create_data_with_all_ref_group(data, outcome, group)
      params$ref.group <- "all"
    }
  }
  test.func <- two_sample_test
  if(number.of.groups > 2) test.func <- pairwise_two_sample_test
  res <- do.call(test.func, params) %>%
    select(.data$.y., .data$group1, .data$group2, .data$estimate, everything()) %>%
    rename(effsize = .data$estimate) %>%
    mutate(magnitude = get_cohens_magnitude(.data$effsize)) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "cohens_d"))
  res
}



# Cohens d core function -------------------------------
cohens.d <- function(x, y = NULL, mu = 0, paired = FALSE, var.equal = FALSE,
                     hedges.correction = FALSE,
                     ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000, ...){
  check_two_samples_test_args(
    x = x, y = y, mu = mu, paired = paired,
    conf.level = conf.level
  )

  if (!is.null(y)) {
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired) {
      OK <- complete.cases(x, y)
      x <- x[OK] - y[OK]
      y <- NULL
      mu <- 0
      METHOD <- "Paired T-test"
    }
    else {
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
      METHOD <- "Independent T-test"
    }
  }
  else {
    DNAME <- deparse(substitute(x))
    METHOD <- "One-sample T-test"
    x <- x[is.finite(x)]
  }

  if(is.null(y)){
   formula <- x ~ 1
   y <- rep(mu, length(x))
  }
  else{
    group <- rep(c("grp1", "grp2"), times = c(length(x), length(y))) %>%
      factor()
    x <- c(x, y)
    y <- group
    formula <- x ~ y
  }
  data <- data.frame(x, y)
  results <- get_cohens_d(
    data, formula, paired = paired, var.equal = var.equal,
    mu = mu, hedges.correction = hedges.correction
    )
  # Confidence interval of the effect size r
  if (ci == TRUE) {
    stat.func <- function(data, subset) {
      get_cohens_d(
        data, formula = formula, subset = subset,
        paired = paired, var.equal = var.equal,
        mu = mu, hedges.correction = hedges.correction
      )$d
    }
    CI <- get_boot_ci(
      data, stat.func, conf.level = conf.level,
      type = ci.type, nboot = nboot
    )
    results <- results %>% mutate(conf.low = CI[1], conf.high = CI[2])
  }
  RVAL <- list(statistic = NA, p.value = NA, null.value = mu, method = METHOD,
               data.name = DNAME, estimate = results$d)
  if (ci) {
    attr(CI, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CI))
  }
  names(RVAL$estimate) <- "Cohen's d"
  class(RVAL) <- "htest"
  RVAL
}

# Helper to compute cohens d -----------------------------------
get_cohens_d <- function(data, formula, subset = NULL, paired = FALSE, mu = 0, var.equal = FALSE,
                         hedges.correction = FALSE){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(!is.null(subset)) data <- data[subset, ]
  if(.is_empty(group))
    number.of.groups <- 1  # Null model
  else
    number.of.groups <- data %>%
    pull(group) %>% unique() %>% length()
  if(number.of.groups == 1){
    x <- data %>% pull(outcome)
    d <- one_sample_d(x, mu)
  }
  else if(number.of.groups == 2){
    groups <- data %>% pull(group)
    data <- data %>% split(groups)
    x <- data[[1]] %>% pull(outcome)
    y <- data[[2]] %>% pull(outcome)
    if(paired){
      d <- paired_sample_d(x, y)
    }
    else{
      d <- two_independent_sample_d(x, y, var.equal)
    }
  }
  else{
    stop("The grouping factors contain more than 2 levels.")
  }
  # Hedge's correction
  if(hedges.correction){
    if(paired){
      n <- length(x)
      d <- d*(n - 2)/(n - 1.25)
    }
    else{
      n <- length(x) + length(y)
      d <- d * (n - 3)/(n - 2.25)
    }
  }

  tibble(
    d,
    magnitude = get_cohens_magnitude(d)
  )
}

one_sample_d <- function(x, mu = 0){
  (mean(x) - mu)/sd(x)
}
two_independent_sample_d <- function(x, y, var.equal = TRUE){
  if(var.equal){
    squared.dev <- (c(x - mean(x), y - mean(y)))^2
    n <- length(squared.dev)
    SD <- sqrt(sum(squared.dev)/(n-2))
  }
  else {
    SD <- sqrt((var(x) + var(y))/2)
  }
  mean.diff <- mean(x) - mean(y)
  mean.diff/SD
}
paired_sample_d <- function(x, y){
  mean(x-y)/sd(x-y)
}


get_cohens_magnitude <- function(d){
  magnitude.levels = c(0.2,0.5,0.8)
  magnitude = c("negligible","small","moderate","large")
  d.index <- findInterval(abs(d), magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}

