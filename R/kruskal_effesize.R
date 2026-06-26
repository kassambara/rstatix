#' @include utilities.R
NULL

#'Kruskal-Wallis Effect Size
#'
#'@description Compute the effect size for Kruskal-Wallis test as the eta
#'  squared based on the H-statistic: \code{eta2[H] = (H - k + 1)/(n - k)};
#'  where \code{H} is the value obtained in the Kruskal-Wallis test; \code{k} is
#'  the number of groups; \code{n} is the total number of observations.
#'
#'
#'  The eta-squared estimate assumes values from 0 to 1 and multiplied by 100%
#'  indicates the percentage of variance in the dependent variable explained by
#'  the independent variable. The interpretation values commonly in published
#'  litterature are: \code{0.01- < 0.06} (small effect), \code{0.06 - < 0.14}
#'  (moderate effect) and \code{>= 0.14} (large effect).
#'
#'  Note that \code{eta2[H]} is a bias-corrected estimator, so the raw formula can
#'  return a small negative value for a near-null effect (very small \code{H}). In
#'  that case the estimate is floored to 0, keeping the reported effect size within
#'  its valid \code{[0, 1]} range.
#'
#' Confidence intervals are calculated by bootstap.
#'
#'@inheritParams wilcox_effsize
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y variable used in the test. \item \code{n}: Sample
#'  counts. \item \code{effsize}: estimate of the effect size. \item
#'  \code{magnitude}: magnitude of effect size. \item \code{conf.low,conf.high}:
#'  lower and upper bound of the effect size confidence interval.}
#'
#'@references Maciej Tomczak and Ewa Tomczak. The need to report effect size
#'  estimates revisited. An overview of some recommended measures of effect
#'  size. Trends in Sport Sciences. 2014; 1(21):19-25.
#'
#'  http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
#'
#'  http://www.psy.gla.ac.uk/~steve/best/effect.html
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Kruskal-wallis rank sum test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% kruskal_effsize(len ~ dose)
#'
#' # Grouped data
#' df %>%
#'   group_by(supp) %>%
#'   kruskal_effsize(len ~ dose)
#' @export
kruskal_effsize <- function(data, formula, ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000){
  args <- as.list(environment()) %>%
    .add_item(method = "kruskal_effsize")
  data %>%
    doo(
      .kruskal_effsize, formula, ci = ci, conf.level = conf.level,
      ci.type = ci.type, nboot = nboot
    ) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "kruskal_effsize"))
}

.kruskal_effsize <- function(data, formula, ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000){
  results <- eta_squared_h(data, formula)
  # Confidence interval of the effect size r
  if (ci == TRUE) {
    stat.func <- function(data, subset) {
      eta_squared_h(data, formula, subset = subset)$effsize
    }
    CI <- get_boot_ci(
      data, stat.func, conf.level = conf.level,
      type = ci.type, nboot = nboot
    )
    results <- results %>%
      add_columns(conf.low = CI[1], conf.high = CI[2], .after = "effsize")
  }
  results %>% mutate(magnitude = get_eta_squared_magnitude(.data$effsize))
}


eta_squared_h <- function(data, formula, subset = NULL, ...){
  if(!is.null(subset)) data <- data[subset, ]
  res.kw <- kruskal_test(data, formula, ...)
  nb.groups <- res.kw$df + 1
  nb.samples <- res.kw$n
  etasq <- (res.kw$statistic - nb.groups + 1) / (nb.samples - nb.groups)
  # eta2[H] is a bias-corrected estimator (like adjusted R-squared / omega-squared)
  # and can be slightly negative for a near-null effect (small H). Floor it to the
  # documented [0, 1] range so the reported effect size is never < 0 (#217).
  etasq <- max(0, min(1, etasq))
  tibble(
    .y. = get_formula_left_hand_side(formula),
    n = nb.samples,
    effsize = etasq,
    method = "eta2[H]"
    )
}

get_eta_squared_magnitude <- function(d){
  magnitude.levels = c(0.06, 0.14, Inf)
  magnitude = c("small","moderate","large")
  # eta-squared is non-negative; a (degenerate) negative value indicates a
  # negligible effect, so it should map to the smallest bucket. Do NOT take
  # abs(d) here, otherwise a negative effsize is mislabelled (e.g. -0.184 was
  # reported as "large") (#217).
  d.index <- findInterval(d, magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}

