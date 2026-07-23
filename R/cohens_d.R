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
#'  It can also return confidence intervals for the effect size, either by
#'  bootstrap (the default) or by an analytic, deterministic method (see
#'  \code{ci.method}).
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/two-groups/cohens-d-effect-size}{Cohen’s d Effect Size in R}
#'  for a worked walkthrough.
#'
#'@inheritParams wilcox_effsize
#'@param ci.method the method used to compute the confidence interval when
#'  \code{ci = TRUE}. Either \code{"boot"} (default) for a percentile bootstrap,
#'  or \code{"analytic"} for a deterministic interval obtained by inverting the
#'  noncentral \emph{t} distribution (Steiger, 2004; Cumming & Finch, 2001) --
#'  the same machinery \code{\link{anova_test}(ci = )} uses for partial
#'  eta-squared. The analytic interval covers the one-sample, paired and
#'  two-sample (equal-variance and Welch) cases, is reproducible across runs
#'  (no seed), and matches \code{effectsize::cohens_d(ci = )}. With
#'  \code{hedges.correction = TRUE} the estimate and both bounds are scaled by
#'  the documented \eqn{(N - 3)/(N - 2.25)} approximation, so they are not
#'  identical to \code{effectsize::hedges_g(ci = )}, which applies the exact
#'  gamma-function correction at the design's degrees of freedom. The gap is
#'  proportional to the effect size and grows as the samples shrink.
#'  When the interval is not defined for a given input (for
#'  example a degenerate group), the bootstrap is used as a fallback. The
#'  default \code{"boot"} leaves the returned interval unchanged from previous
#'  versions.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param paired a logical indicating whether you want a paired test.
#'@param id (optional) character string with the name of the column holding the
#'  sample/subject identifier, used only for a \strong{paired} test
#'  (\code{paired = TRUE}). When supplied, the two groups are matched by
#'  \code{id} (instead of by row order) before the paired d is computed, as in
#'  \code{\link{t_test}()}. Only complete pairs (subjects measured in both
#'  groups) are used.
#'@param mu the theoretical mean (one-sample test) or the hypothesized difference
#'  in means (two-sample test). It is subtracted from the mean difference before
#'  standardizing, so a non-zero \code{mu} shifts the effect size accordingly.
#'  Default is 0.
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
#'  Tutorial for Psychology Students and Other Beginners (Version 0.5). \item
#'  Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
#'  and tests of close fit in the analysis of variance and contrast analysis.
#'  Psychological Methods, 9(2), 164-182. \item Cumming, G., & Finch, S. (2001).
#'  A primer on the understanding, use, and calculation of confidence intervals
#'  that are based on central and noncentral distributions. Educational and
#'  Psychological Measurement, 61(4), 532-574. }
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
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/two-groups/cohens-d-effect-size}{Cohen’s d Effect Size in R}.
#'@export
cohens_d <- function(data, formula, comparisons = NULL, ref.group = NULL, paired = FALSE, mu = 0,
                     var.equal = FALSE, hedges.correction = FALSE,
                     ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000,
                     boot.parallel = getOption("boot.parallel", "no"),
                     boot.ncpus = getOption("boot.ncpus", 1L), id = NULL,
                     ci.method = c("boot", "analytic")){
  ci.method <- match.arg(ci.method)
  env <- as.list(environment())
  # boot.parallel/boot.ncpus only steer how the bootstrap is computed, never the
  # result, and their defaults depend on the user's options(); keep them out of
  # the stashed args so attr(x, "args") stays deterministic.
  args <- env %>%
    remove_item(c("boot.parallel", "boot.ncpus")) %>%
    .add_item(method = "cohens_d")
  # id (paired matching by subject) is optional; keep it out of the stashed args
  # when unused so attr(x, "args") is unchanged for existing calls.
  if(is.null(id)) args <- remove_item(args, "id")
  # ci.method chooses how the interval is computed, not the point estimate; keep
  # it out of the stashed args on the default ("boot") so attr(x, "args") is
  # unchanged for existing calls.
  if(ci.method == "boot") args <- remove_item(args, "ci.method")
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
    select(all_of(c(".y.", "group1", "group2", "estimate")), everything()) %>%
    rename(effsize = "estimate") %>%
    mutate(magnitude = get_cohens_magnitude(.data$effsize)) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "cohens_d"))
  warn_undefined_boot_ci(res, ci)
  res
}



# Cohens d core function -------------------------------
cohens.d <- function(x, y = NULL, mu = 0, paired = FALSE, var.equal = FALSE,
                     hedges.correction = FALSE,
                     ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000,
                     ci.method = c("boot", "analytic"), ...,
                     boot.parallel = getOption("boot.parallel", "no"),
                     boot.ncpus = getOption("boot.ncpus", 1L)){
  ci.method <- match.arg(ci.method)
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
      # keep the user-supplied `mu`: a paired test reduces to a one-sample test
      # on the differences, where `mu` is the hypothesized mean difference (#200)
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

  # Raw group vectors, kept before the reshaping below: the analytic CI needs
  # them (x = the differences for a paired test, or NULL for one-sample).
  x.raw <- x
  y.raw <- y
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
  # Confidence interval of the effect size
  if (ci == TRUE) {
    CI <- NULL
    if (ci.method == "analytic") {
      # Deterministic noncentral-t interval; NA for a degenerate case (below),
      # in which case fall back to the bootstrap so a CI is still returned.
      CI <- cohens_d_analytic_ci(
        x.raw, y.raw, mu = mu, paired = paired, var.equal = var.equal,
        hedges.correction = hedges.correction, conf.level = conf.level
      )
      if (any(is.na(CI))) CI <- NULL
    }
    if (is.null(CI)) {
      stat.func <- function(data, subset) {
        get_cohens_d(
          data, formula = formula, subset = subset,
          paired = paired, var.equal = var.equal,
          mu = mu, hedges.correction = hedges.correction
        )$d
      }
      CI <- get_boot_ci(
        data, stat.func, conf.level = conf.level,
        type = ci.type, nboot = nboot, parallel = boot.parallel, ncpus = boot.ncpus
      )
    }
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

  unpaired.two.samples <- paired == FALSE & number.of.groups == 2

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
      d <- paired_sample_d(x, y, mu)
    }
    else{
      d <- two_independent_sample_d(x, y, var.equal, mu)
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
    else if (unpaired.two.samples){
      n <- length(x) + length(y)
      d <- d * (n - 3)/(n - 2.25)
    }
    else{
      stop(
        "Hedge's Correction for One Sample Test is not supported.\n",
        "Please use `hedges.correction = FALSE` (default) for one sample test.",
        call. = FALSE
        )
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
two_independent_sample_d <- function(x, y, var.equal = TRUE, mu = 0){
  if(var.equal){
    squared.dev <- (c(x - mean(x), y - mean(y)))^2
    n <- length(squared.dev)
    SD <- sqrt(sum(squared.dev)/(n-2))
  }
  else {
    SD <- sqrt((var(x) + var(y))/2)
  }
  mean.diff <- mean(x) - mean(y)
  (mean.diff - mu)/SD
}
paired_sample_d <- function(x, y, mu = 0){
  (mean(x-y) - mu)/sd(x-y)
}

# Analytic (deterministic) confidence interval for Cohen's d / Hedges' g, by
# inverting the noncentral t distribution -- the same machinery as
# partial_eta_squared_ci(), with t in place of F. The observed t = d * sqrt(n_eff)
# is noncentral-t; the ncp bounds solve pt(t, df, ncp) = {1 - alpha/2, alpha/2},
# and ncp rescales back to d by / sqrt(n_eff). Returns c(NA, NA) for a degenerate
# input so the caller can fall back to the bootstrap. Matches
# effectsize::cohens_d(ci = ) / hedges_g(ci = ). Refs: Steiger (2004);
# Cumming & Finch (2001).
cohens_d_nct_ci <- function(d, n_eff, df, conf.level = 0.95, hedges.J = 1){
  if(any(is.na(c(d, n_eff, df))) || !is.finite(d) || n_eff <= 0 || df <= 0)
    return(c(NA_real_, NA_real_))
  alpha <- 1 - conf.level
  t.obs <- d * sqrt(n_eff)
  # pt(t.obs, df, ncp) decreases in ncp; find the ncp where it hits the target.
  # suppressWarnings(): base R's noncentral-t routine can warn about incomplete
  # precision at extreme ncp; the returned bound is still valid.
  find_ncp <- function(target.prob){
    suppressWarnings({
      f <- function(ncp) stats::pt(t.obs, df, ncp) - target.prob
      step <- abs(t.obs) + 1
      lo <- t.obs - step; hi <- t.obs + step
      while(f(lo) < 0){ lo <- lo - step; if(lo < -1e7) return(lo) }
      while(f(hi) > 0){ hi <- hi + step; if(hi >  1e7) return(hi) }
      stats::uniroot(f, interval = c(lo, hi))$root
    })
  }
  ncp.low  <- find_ncp(1 - alpha/2)
  ncp.high <- find_ncp(alpha/2)
  c(ncp.low, ncp.high) / sqrt(n_eff) * hedges.J
}

# Effective n, df and (uncorrected) d for the noncentral-t interval, per design.
# x/y are the raw group vectors from cohens.d(): for a paired or one-sample test
# y is NULL and x already holds the differences (paired) or the sample values
# (one-sample). hedges.J mirrors get_cohens_d()'s small-sample factor and is
# applied to the interval, not to the noncentral t (which is on the uncorrected
# d scale).
cohens_d_analytic_ci <- function(x, y, mu = 0, paired = FALSE, var.equal = FALSE,
                                 hedges.correction = FALSE, conf.level = 0.95){
  if(is.null(y)){
    # one-sample, or a paired test reduced to a one-sample test on the differences
    n <- length(x)
    if(n < 2 || !is.finite(stats::sd(x)) || stats::sd(x) == 0)
      return(c(NA_real_, NA_real_))
    d0 <- one_sample_d(x, mu)
    n_eff <- n
    df <- n - 1
    J <- if(hedges.correction && paired) (n - 2)/(n - 1.25) else 1
  } else {
    n1 <- length(x); n2 <- length(y)
    if(n1 < 2 || n2 < 2) return(c(NA_real_, NA_real_))
    vx <- stats::var(x); vy <- stats::var(y)
    d0 <- two_independent_sample_d(x, y, var.equal = var.equal, mu = mu)
    if(var.equal){
      n_eff <- n1 * n2 / (n1 + n2)
      df <- n1 + n2 - 2
    } else {
      # data-dependent n_eff so that d0 * sqrt(n_eff) equals the Welch t statistic
      se2 <- vx/n1 + vy/n2
      if(!is.finite(se2) || se2 == 0) return(c(NA_real_, NA_real_))
      n_eff <- ((vx + vy)/2) / se2
      df <- se2^2 / ((vx/n1)^2/(n1 - 1) + (vy/n2)^2/(n2 - 1))   # Welch-Satterthwaite
    }
    J <- if(hedges.correction) (n1 + n2 - 3)/(n1 + n2 - 2.25) else 1
  }
  if(!is.finite(d0)) return(c(NA_real_, NA_real_))
  cohens_d_nct_ci(d0, n_eff = n_eff, df = df, conf.level = conf.level, hedges.J = J)
}


get_cohens_magnitude <- function(d){
  magnitude.levels = c(0.2,0.5,0.8)
  magnitude = c("negligible","small","moderate","large")
  d.index <- findInterval(abs(d), magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}

