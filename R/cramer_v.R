#' @include utilities.R
#'
NULL
#'Compute Cramer's V
#'@description Compute Cramer's V, which measures the strength of the
#'  association between categorical variables.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/categorical/chi-square-test-of-independence-in-r}{Chi-Square Test of Independence in R}
#'  for a worked walkthrough.
#'@inheritParams stats::chisq.test
#'@param correct logical. If TRUE, Yates' continuity correction is applied when
#'  computing the chi-square statistic, which only affects 2x2 tables. Default is
#'  FALSE. Yates' correction improves the chi-square approximation to the
#'  \emph{null} distribution of the test statistic, so it belongs to the
#'  \emph{test} (see \code{\link{chisq_test}()}) rather than to an effect size:
#'  it shrinks the statistic and therefore biases Cramer's V downward. Set
#'  \code{correct = TRUE} to reproduce the value returned by earlier versions of
#'  rstatix, which applied the correction by default.
#'@param ... other arguments passed to the function
#'  \code{\link[stats]{chisq.test}()}.
#'@param ci logical. If TRUE, a confidence interval for Cramer's V is added to
#'  the result and a one-row data frame is returned instead of a single value.
#'  Default is FALSE.
#'@param conf.level The level of the confidence interval. Default is 0.95. Only
#'  used when \code{ci = TRUE}.
#'@return By default, a single numeric value: Cramer's V.
#'
#'  When \code{ci = TRUE}, a one-row tibble with the columns \code{effsize}
#'  (Cramer's V), \code{conf.low} and \code{conf.high} -- the same confidence
#'  interval columns that \code{\link{anova_test}(ci = )} returns.
#'@details Cramer's V is \eqn{V = \sqrt{\chi^2 / (N (k - 1))}} (Cramer, 1946),
#'  where \eqn{\chi^2} is the Pearson chi-square statistic, \eqn{N} the total
#'  count and \eqn{k} the smaller of the two table dimensions.
#'
#'  The confidence interval is obtained by inverting the noncentral chi-square
#'  distribution (Smithson, 2003; Steiger, 2004): the noncentrality parameters
#'  \eqn{\lambda} whose distributions place the observed chi-square statistic at
#'  the \eqn{1 - \alpha/2} and \eqn{\alpha/2} quantiles are found by root finding,
#'  and each is converted with \eqn{V = \sqrt{\lambda / (N (k - 1))}}. The
#'  interval is computed from the same chi-square statistic as the point estimate,
#'  so \code{correct = TRUE} shifts both.
#'
#'  The bounds are clipped to \eqn{[0, 1]}, the range of Cramer's V. They are
#'  \code{NA}, with a warning, when the statistic or its degrees of freedom are
#'  undefined -- for instance when the table has an empty row or column, or when
#'  \code{simulate.p.value = TRUE} is passed on to
#'  \code{\link[stats]{chisq.test}()}, which then reports no degrees of freedom.
#'
#'  The interval usually brackets the reported \code{effsize}, but it does not in
#'  the near-independence corner: when the observed chi-square falls below the
#'  \eqn{\alpha/2} quantile of its central distribution, no noncentrality is
#'  consistent with the data at that quantile, both bounds collapse to
#'  \eqn{[0, 0]}, and the (necessarily positive) point estimate lies above them.
#'  This is a property of the noncentral inversion rather than of this
#'  implementation, and it only arises for effect sizes indistinguishable from
#'  zero.
#'
#'  At the default \code{correct = FALSE}, the results match
#'  \code{effectsize::cramers_v(adjust = FALSE, ci = , alternative =
#'  "two.sided")} away from the near-independence corner above (where numerical
#'  inversions differ), and \code{DescTools::CramerV(conf.level = )} to about
#'  four decimals (its inversion uses a looser tolerance), except at a
#'  chi-square of exactly zero, where \code{DescTools} returns \code{NA}
#'  bounds and this function returns the collapsed \code{[0, 0]} interval.
#'  Neither package applies Yates' continuity correction, so \code{correct =
#'  TRUE} values have no counterpart there (\code{DescTools}'s own
#'  \code{correct} argument selects the Bergsma bias correction, a different
#'  adjustment).
#'@references Cramer, H. (1946). Mathematical Methods of Statistics. Princeton
#'  University Press.
#'
#'  Smithson, M. (2003). Confidence Intervals. Sage Publications.
#'
#'  Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
#'  and tests of close fit in the analysis of variance and contrast analysis.
#'  Psychological Methods, 9, 164-182.
#'@examples
#'
#' # Data preparation
#' df <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(df) <- list(
#'   gender = c("F", "M"),
#'   party = c("Democrat","Independent", "Republican")
#' )
#' df
#' # Compute cramer's V
#' cramer_v(df)
#'
#' # Add a confidence interval
#' cramer_v(df, ci = TRUE)
#'
#' # Yates' continuity correction only affects 2x2 tables. It belongs to the
#' # test, not to the effect size, so it is off by default.
#' tab <- as.table(rbind(c(20, 30), c(35, 15)))
#' cramer_v(tab)
#' cramer_v(tab, correct = TRUE)
#'
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/categorical/chi-square-test-of-independence-in-r}{Chi-Square Test of Independence in R}.
#'@export
cramer_v <- function(x, y = NULL, correct = FALSE, ..., ci = FALSE, conf.level = 0.95) {
  test <- stats::chisq.test(x, y, correct = correct, ...)
  chi2 <- test$statistic
  N <- sum(test$observed)
  k <- min(dim(test$observed))
  V <- sqrt(chi2/(N * (k - 1)))
  V <- as.numeric(V)
  if(!isTRUE(ci)) return(V)
  if(!is.numeric(conf.level) || length(conf.level) != 1L ||
     is.na(conf.level) || conf.level <= 0 || conf.level >= 1){
    stop("`conf.level` must be a single number between 0 and 1.", call. = FALSE)
  }
  bounds <- cramer_v_ci(
    chi2 = as.numeric(chi2), df = as.numeric(test$parameter),
    N = N, k = k, conf.level = conf.level
  )
  if(anyNA(bounds)){
    warning(
      "The confidence interval for Cramer's V could not be computed because the ",
      "chi-square statistic or its degrees of freedom are undefined (this happens ",
      "with an empty row or column, and with `simulate.p.value = TRUE`, which ",
      "reports no degrees of freedom). `conf.low` and `conf.high` are returned as NA.",
      call. = FALSE
    )
  }
  tibble(effsize = V, conf.low = bounds[1], conf.high = bounds[2])
}

# Confidence interval for Cramer's V by inverting the noncentral chi-square
# distribution. The noncentral-F analogue used for partial eta squared lives in
# R/anova_summary.R (partial_eta_squared_ci); the two share the same structure.
cramer_v_ci <- function(chi2, df, N, k, conf.level = 0.95){
  undefined <- c(NA_real_, NA_real_)
  if(any(is.na(c(chi2, df, N, k))) || !is.finite(chi2) || chi2 < 0 ||
     df <= 0 || N <= 0 || k <= 1) return(undefined)
  alpha <- 1 - conf.level
  # Smallest lambda for which P(chi2 <= observed | ncp = lambda) == target.prob;
  # lambda = 0 when even the central chi-square already gives a smaller
  # probability (the observed statistic is too small to bound the ncp away
  # from 0). suppressWarnings(): at extreme noncentrality base R's noncentral
  # chi-square routine can warn about non-convergence while still returning a
  # usable bound.
  find_lambda <- function(target.prob){
    suppressWarnings({
      if(stats::pchisq(chi2, df, ncp = 0) < target.prob) return(0)
      upper <- 2
      while(stats::pchisq(chi2, df, ncp = upper) > target.prob){
        upper <- upper * 2
        if(upper > 1e8) return(upper)
      }
      stats::uniroot(
        function(lambda) stats::pchisq(chi2, df, ncp = lambda) - target.prob,
        interval = c(0, upper)
      )$root
    })
  }
  lambda.low <- find_lambda(1 - alpha/2)
  lambda.high <- find_lambda(alpha/2)
  to_v <- function(lambda) sqrt(lambda / (N * (k - 1)))
  # Cramer's V lies in [0, 1]; the inversion can put the upper bound past 1 when
  # the association is near-perfect, so clip (as kruskal_effsize does for eta2).
  pmin(pmax(c(to_v(lambda.low), to_v(lambda.high)), 0), 1)
}
