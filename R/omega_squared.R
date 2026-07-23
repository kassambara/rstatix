#' @include utilities.R eta_squared.R
NULL

#' Omega Squared for ANOVA
#' @description Compute (classic, full) omega-squared and partial omega-squared
#'   for all terms in a between-subjects ANOVA model. Omega squared is a
#'   less-biased alternative to eta squared, estimating the population effect
#'   size rather than the sample one.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}
#'  for a worked walkthrough.
#' @param model an object of class \code{aov} or \code{anova} (a between-subjects
#'   design). Repeated-measures and mixed models are not supported, as for
#'   \code{\link{eta_squared}()}.
#' @return a named numeric vector of effect sizes, one per model term. A
#'   negative point estimate (which can arise for a term with F < 1) is floored
#'   at 0, since omega squared estimates a non-negative proportion of variance.
#' @references Olejnik, S., & Algina, J. (2003). Generalized eta and omega
#'   squared statistics: Measures of effect size for some common research
#'   designs. \emph{Psychological Methods}, 8(4), 434-447.
#' @seealso \code{\link{eta_squared}()}, \code{\link{anova_test}()}.
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}.
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Fit the model
#' res.aov <- aov(len ~ supp * dose, data = df)
#'
#' # Effect size
#' omega_squared(res.aov)
#' partial_omega_squared(res.aov)

#' @describeIn omega_squared compute the classic (full) omega squared.
#' @export
omega_squared <- function(model){
  aovstat <- model %>%
    aov_stat_summary() %>%
    aov_stat_core("omega")
  # Floor negative estimates at 0: omega squared estimates a non-negative
  # proportion of variance, and a term with F < 1 yields a negative raw value.
  # Floored in place so the term names are preserved.
  aovstat[aovstat < 0] <- 0
  aovstat
}

#' @describeIn omega_squared compute partial omega squared.
#' @export
partial_omega_squared <- function(model){
  aov.sum <- aov_stat_summary(model)
  n_terms <- nrow(aov.sum) - 1
  ms.resid <- aov.sum[["meansq"]][nrow(aov.sum)]
  # total sample size: the term df, the residual df and the intercept
  n_obs <- sum(aov.sum[["df"]]) + 1
  aovstat <- purrr::map_dbl(1:n_terms, function(x){
    ss.term <- aov.sum[["sumsq"]][x]
    df.term <- aov.sum[["df"]][x]
    (ss.term - df.term * ms.resid) / (ss.term + (n_obs - df.term) * ms.resid)
  })
  names(aovstat) <- aov.sum[["term"]][1:n_terms]
  # Floor negative estimates at 0 (see omega_squared()); in place to keep names.
  aovstat[aovstat < 0] <- 0
  aovstat
}
