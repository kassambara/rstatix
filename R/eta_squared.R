#' @include utilities.R
NULL
#' Effect Size for ANOVA
#' @description Compute eta-squared and partial eta-squared for all terms in an
#'   ANOVA model.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}
#'  for a worked walkthrough.
#' @param model an object of class \code{aov} or \code{anova}.
#' @param ci confidence level for a confidence interval on the effect size. If a
#'   number between 0 and 1 (e.g. \code{0.95}), the function returns a tibble with
#'   one row per model term and the columns \code{Effect}, \code{effsize},
#'   \code{conf.low} and \code{conf.high} instead of the bare named vector. The
#'   interval is computed in base R by inverting the noncentral F distribution
#'   (Steiger, 2004), and matches
#'   \code{effectsize::eta_squared(ci = , alternative = "two.sided")} —
#'   \code{partial = FALSE} for \code{eta_squared()} and \code{partial = TRUE} for
#'   \code{partial_eta_squared()} — to about four decimals for the non-partial
#'   bounds of a small pseudo-F (that function's inversion uses a looser
#'   tolerance), and more closely everywhere else. Default is \code{NULL} (no
#'   interval; the bare named vector is returned, unchanged).
#' @return a named numeric vector of effect sizes, one per model term; or, when
#'   \code{ci} is a confidence level, a tibble with the columns \code{Effect},
#'   \code{effsize}, \code{conf.low} and \code{conf.high}.
#' @references Steiger, J. H. (2004). Beyond the F test: Effect size confidence
#'   intervals and tests of close fit in the analysis of variance and contrast
#'   analysis. \emph{Psychological Methods}, 9, 164-182.
#' @describeIn eta_squared compute eta squared
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Compute ANOVA
#' res.aov <- aov(len ~ supp*dose, data = df)
#' summary(res.aov)
#'
#' # Effect size
#' eta_squared(res.aov)
#' partial_eta_squared(res.aov)
#'
#' # Effect size with confidence interval
#' eta_squared(res.aov, ci = 0.95)
#' partial_eta_squared(res.aov, ci = 0.95)
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}.

#' @export
eta_squared <- function(model, ci = NULL){
  es <- model %>%
    aov_stat_summary() %>%
    aov_stat_core("eta")
  if(is.null(ci)) return(es)
  aov_effsize_ci(model, es, ci)
}

#' @describeIn eta_squared compute partial eta squared.
#' @export
partial_eta_squared <- function(model, ci = NULL){
  es <- model %>%
    aov_stat_summary() %>%
    aov_stat_core("peta")
  if(is.null(ci)) return(es)
  aov_effsize_ci(model, es, ci)
}


aov_stat_summary <- function (model)
{
  if (!inherits(model, c("aov", "anova")))
    model <- stats::anova(model)
  aov.sum <- broom::tidy(model)
  if (!tibble::has_name(aov.sum, "meansq"))
    aov.sum <- tibble::add_column(aov.sum, meansq = aov.sum$sumsq/aov.sum$df,
                                  .after = "sumsq")
  drop_intercept_row(aov.sum)
}

# A car::Anova() table (type = 3) carries an "(Intercept)" row. Its sum of
# squares belongs to neither the effect decomposition nor the residual, so it
# must not enter SS_total (eta squared, omega squared), the sample-size
# inference sum(df) + 1 (partial omega squared), or the per-term output rows.
# Tables without such a row (aov, stats::anova(), car type 2) pass through
# unchanged.
drop_intercept_row <- function(aov.sum){
  aov.sum[trimws(aov.sum[["term"]]) != "(Intercept)", , drop = FALSE]
}

aov_stat_core <- function(aov.sum, type){
    meansq.resid <- aov.sum[["meansq"]][nrow(aov.sum)]
    ss.total <- sum(aov.sum[["sumsq"]])
    ss.resid <- aov.sum[["sumsq"]][nrow(aov.sum)]
    n_terms <- nrow(aov.sum) - 1
    if (type == "omega") {
      aovstat <- purrr::map_dbl(1:n_terms, function(x) {
        ss.term <- aov.sum[["sumsq"]][x]
        df.term <- aov.sum[["df"]][x]
        (ss.term - df.term * meansq.resid)/(ss.total + meansq.resid)
      })
    }
    else if (type == "eta") {
      aovstat <- purrr::map_dbl(1:n_terms, ~aov.sum[["sumsq"]][.x]/sum(aov.sum[["sumsq"]]))
    }
    else if (type %in% c("cohens.f", "peta")) {
      aovstat <- purrr::map_dbl(1:n_terms, ~aov.sum[["sumsq"]][.x]/(aov.sum[["sumsq"]][.x] +
                                                                      ss.resid))
    }
    if (type == "cohens.f")
      aovstat <- sqrt(aovstat/(1 - aovstat))
    names(aovstat) <- aov.sum[["term"]][1:n_terms]
    aovstat
  }


# Confidence interval for an ANOVA effect size (eta-squared or partial
# eta-squared), one row per term. The noncentral-F interval is defined for the
# partial estimand (partial_eta_squared_ci() inverts pf(ncp=)). To get the
# interval for whichever estimate was supplied, feed that estimate back through
# the same machinery as a "pseudo-F", f* = (es/df) / ((1 - es)/df.error). For a
# partial eta-squared this pseudo-F equals the term's real F, so the result is
# the partial interval; for a (non-partial) eta-squared it is the equivalent F
# that reproduces effectsize::eta_squared(partial = FALSE). This is the same
# construction effectsize uses (see F_to_pve / .es_aov_simple).
aov_effsize_ci <- function(model, es, ci = 0.95){
  if(length(ci) != 1L || !is.numeric(ci) || is.na(ci) || ci <= 0 || ci >= 1)
    stop("`ci` must be a single number between 0 and 1.", call. = FALSE)
  aov.sum <- aov_stat_summary(model)
  df.error <- aov.sum[["df"]][nrow(aov.sum)]
  terms <- aov.sum[["term"]][seq_along(es)]
  df.terms <- aov.sum[["df"]][seq_along(es)]
  bounds <- mapply(
    function(eta, df.term){
      f.star <- (eta / df.term) / ((1 - eta) / df.error)
      partial_eta_squared_ci(f.star, df.term, df.error, conf.level = ci)
    },
    es, df.terms, SIMPLIFY = FALSE
  )
  bounds <- do.call(rbind, bounds)
  # Unrounded, like eta_squared() itself and like effectsize. Agreement with
  # effectsize::eta_squared(ci = ) is bounded by the two root-finders: ours
  # solves the defining equation to ~1e-8 or better; effectsize's non-partial
  # inversion can be ~5e-4 off for a small pseudo-F (its tolerance is looser).
  tibble::tibble(
    Effect    = terms,
    effsize   = as.numeric(es),
    conf.low  = as.numeric(bounds[, 1]),
    conf.high = as.numeric(bounds[, 2])
  )
}
