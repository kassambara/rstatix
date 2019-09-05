#' @include utilities.R friedman_test.R
NULL

#' Friedman Test Effect Size (Kendall's W Value)
#'
#'@description Compute the effect size estimate (referred to as \code{w}) for
#'  Friedman test: \code{W = X2/N(K-1)}; where \code{W} is the Kendall's W
#'  value; \code{X2} is the Friedman test statistic value; \code{N} is the sample
#'  size. \code{k} is the number of measurements per subject.
#'
#'  The Kendall’s W coefficient assumes the value from 0 (indicating no
#'  relationship) to 1 (indicating a perfect relationship).
#'
#'  Kendalls uses the Cohen’s interpretation guidelines of \code{0.1 - < 0.3} (small
#'  effect), \code{0.3 - < 0.5} (moderate effect) and \code{>= 0.5} (large
#'  effect)
#'
#'  Confidence intervals are calculated by bootstap.
#'
#'@inheritParams friedman_test
#'@inheritParams wilcox_effsize
#'@param ... other arguments passed to the function \code{\link[stats]{friedman.test}()}
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
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth %>%
#'     filter(supp == "VC") %>%
#'     mutate(id = rep(1:10, 3))
#' head(df)
#'
#' # Friedman test effect size
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% friedman_effsize(len ~ dose | id)

#' @export
friedman_effsize <- function(data, formula, ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000, ...){
  args <- as.list(environment()) %>%
    .add_item(method = "friedman_effsize")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(
        .friedman_effsize, formula, ci = ci, conf.level = conf.level,
        ci.type = ci.type, nboot = nboot, ...
      )
  }
  else{
    results <- .friedman_effsize(
      data, formula, ci = ci, conf.level = conf.level,
      ci.type = ci.type, nboot = nboot, ...
    )
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "friedman_effsize"))
}


.friedman_effsize <- function(data, formula, ci = FALSE, conf.level = 0.95,  ci.type = "perc", nboot = 1000, ...){
  results <- kendall_w(data, formula, ...)
  # Confidence interval of the effect size
  if (ci == TRUE) {
    # Paired data, bootstrap should be performed on wide data
    vars <- get_friedman_vars(formula)
    data.wide <- data %>%
      select(!!!syms(c(vars$wid, vars$dv, vars$within))) %>%
      spread(key = vars$within, value = vars$dv)
    # Boot function
    stat.func <- function(data.wide, subset) {
      if(!is.null(subset)) data.wide <- data.wide[subset, ]
      data.long <- data.wide %>%
        mutate(!!vars$wid := 1:nrow(data.wide)) %>%
        gather(key = !!vars$within, value = !!vars$dv, - !!vars$wid)
      kendall_w(data.long, formula)$effsize
    }
    CI <- get_boot_ci(
      data.wide, stat.func, conf.level = conf.level,
      type = ci.type, nboot = nboot
    )
    results <- results %>%
      add_columns(conf.low = CI[1], conf.high = CI[2], .after = "effsize")
  }
  results %>%
    mutate(magnitude = get_kendall_w_magnitude(.data$effsize))
}


kendall_w <- function(data, formula, subset = NULL, ...){
  if(!is.null(subset)) data <- data[subset, ]
  res.f <- friedman_test(data, formula, ...)
  x2 <- res.f$statistic
  nb.samples <- res.f$n
  k <- nrow(data)/nb.samples # number of measurements per sample
  w <- x2 / (nb.samples * (k-1))
  tibble(
    .y. = get_formula_left_hand_side(formula),
    n = nb.samples,
    effsize = w,
    method = "Kendall W"
    )
}


get_kendall_w_magnitude <- function(d){
  magnitude.levels = c(0.3, 0.5, Inf)
  magnitude = c("small","moderate","large")
  d.index <- findInterval(abs(d), magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}

