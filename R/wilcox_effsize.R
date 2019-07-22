#' @include utilities.R utilities_two_sample_test.R
NULL

#'Wilcoxon Effect Size
#'@description Compute Wilcoxon effect size (\code{r}) for: \itemize{ \item
#'  one-sample test (Wilcoxon one-sample signed-rank test); \item paired
#'  two-samples test (Wilcoxon two-sample paired signed-rank test) and \item
#'  independent two-samples test ( Mann-Whitney, two-sample rank-sum test). }
#'
#'  It can also returns confidence intervals by bootstap.
#'
#'  The effect size \code{r} is calculated as \code{Z} statistic divided by
#'  square root of the sample size (N) (\eqn{Z/\sqrt{N}}). The \code{Z} value is
#'  extracted from either \code{coin::wilcoxsign_test()} (case of one- or
#'  paired-samples test) or \code{coin::wilcox_test()} (case of independent
#'  two-samples test).
#'
#'  Note that \code{N} corresponds to total sample size for independent samples
#'  test and to total number of pairs for paired samples test.
#'
#'  The \code{r} value varies from 0 to close to 1. The interpretation values
#'  for r commonly in published litterature and on the internet are: \code{0.10
#'  - < 0.3} (small effect), \code{0.30 - < 0.5} (moderate effect) and \code{>=
#'  0.5} (large effect).
#'
#'@inheritParams wilcox_test
#'@param ci If TRUE, returns confidence intervals by bootstrap. May be slow.
#'@param conf.level The level for the confidence interval.
#'@param ci.type The type of confidence interval to use. Can be any of "norm",
#'  "basic", "perc", or "bca". Passed to \code{boot::boot.ci}.
#'@param nboot The number of replications to use for bootstrap.
#'@param ... Additional arguments passed to the functions
#'  \code{coin::wilcoxsign_test()} (case of one- or paired-samples test) or
#'  \code{coin::wilcox_test()} (case of independent two-samples test).
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n,n1,n2}: Sample counts. \item \code{effsize}: estimate of the effect
#'  size (\code{r} value). \item \code{magnitude}: magnitude of effect size.
#'  \item \code{conf.low,conf.high}: lower and upper bound of the effect size
#'  confidence interval.}
#'@references Maciej Tomczak and Ewa Tomczak. The need to report effect size
#'  estimates revisited. An overview of some recommended measures of effect
#'  size. Trends in Sport Sciences. 2014; 1(21):19-25.
#' @examples
#' if(require("coin")){
#'
#' # One-sample Wilcoxon test effect size
#' ToothGrowth %>% wilcox_effsize(len ~ 1, mu = 0)
#'
#' # Independent two-samples wilcoxon effect size
#' ToothGrowth %>% wilcox_effsize(len ~ supp)
#'
#'
#' # Paired-samples wilcoxon effect size
#' ToothGrowth %>% wilcox_effsize(len ~ supp, paired = TRUE)
#'
#' # Pairwise comparisons
#' ToothGrowth %>% wilcox_effsize(len ~ dose)
#'
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   wilcox_effsize(len ~ dose)
#'
#' }
#'@export
wilcox_effsize <- function(data, formula, comparisons = NULL, ref.group = NULL,
                                paired = FALSE, alternative = "two.sided",
                                mu = 0, ci = FALSE, conf.level = 0.95, ci.type = "perc",
                                nboot = 1000,  ...){

  env <- as.list(environment())
  args <- env %>% .add_item(method = "wilcox_effsize")
  params <- c(env, list(...)) %>%
    remove_null_items() %>%
    add_item(method = "coin.wilcox.test", detailed = FALSE)

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
    mutate(magnitude = get_wilcox_effsize_magnitude(.data$effsize)) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "wilcox_effsize"))
  res
}



# Wilcoxon test using coin R package; returns effect size
coin.wilcox.test <- function(x, y = NULL, mu = 0, paired = FALSE, alternative = c("two.sided", "less", "greater"),
                     ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000, ...){
  required_package("coin")

  alternative <- match.arg(alternative)
  check_two_samples_test_args(
    x = x, y = y, mu = mu, paired = paired,
    conf.level = conf.level
    )

  if (!is.null(y)) {
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired) {
      # Transform paired test into one-sample test problem
      OK <- complete.cases(x, y)
      x <- x[OK] - y[OK]
      y <- NULL
      METHOD <- "Paired Wilcoxon test (coin)"
    }
    else {
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
      METHOD <- "Independent Wilcoxon test (coin)"
    }
  }
  else {
    DNAME <- deparse(substitute(x))
    METHOD <- "One-sample Wilcoxon test (coin)"
    x <- x[is.finite(x)]
  }

  if(is.null(y)){
    y <- rep(mu, length(x))
    test.type <- "symmetry"
  }
  else{
    group <- rep(c("grp1", "grp2"), times = c(length(x), length(y))) %>%
      factor()
    x <- c(x, y)
    y <- group
    test.type <- "independence"
  }
  data <- data.frame(x, y)
  results <- coin_wilcox_test(
    data, x ~ y, type = test.type,
    alternative = alternative,  ...
    )
  # Confidence interval of the effect size r
  if (ci == TRUE) {
    stat.func <- function(data, subset) {
      coin_wilcox_test(
        data, formula = x ~ y, subset = subset,
        type = test.type, alternative = alternative, ...
        )$r
    }
    CI <- get_boot_ci(
      data, stat.func, conf.level = conf.level,
      type = ci.type, nboot = nboot
      )
    results <- results %>% mutate(conf.low = CI[1], conf.high = CI[2])
  }
  RVAL <- list(statistic = results$z, parameter = results$n, p.value = results$p,
               null.value = mu, alternative = alternative, method = METHOD,
               data.name = DNAME, estimate = results$r)
  if (ci) {
    attr(CI, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CI))
  }
  names(RVAL$statistic) <- "Z"
  names(RVAL$parameter) <- "n"
  names(RVAL$estimate) <- "Effect size (r)"
  class(RVAL) <- "htest"
  RVAL
}

# Perform wilcoxon test using coin package
coin_wilcox_test <- function(data, formula, subset = NULL, type = c("independence", "symmetry"),  ...){
  type <- match.arg(type)
  coin_wilcox_test_func <- switch (
    type,
    independence = coin::wilcox_test,
    symmetry = coin::wilcoxsign_test
  )
  if(!is.null(subset)) data <- data[subset, ]
  res.wilcox <-suppressWarnings(coin_wilcox_test_func(formula, data = data,...))
  n <- nrow(data)
  z <- as.vector(coin::statistic(res.wilcox, type = "standardized"))
  p <- coin::pvalue(res.wilcox)
  r <- abs(z)/sqrt(n) # Effect size
  tibble(n = n, z = z, r = r, p = p)
}

get_wilcox_effsize_magnitude <- function(d){
  magnitude.levels = c(0.3, 0.5, Inf)
  magnitude = c("small","moderate","large")
  d.index <- findInterval(abs(d), magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}
