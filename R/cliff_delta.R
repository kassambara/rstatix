#' @include utilities.R utilities_two_sample_test.R
NULL
#' Cliff's Delta Effect Size for Ordinal / Non-parametric Comparisons
#'
#' @description Compute Cliff's delta, a non-parametric effect size for the
#'   difference between two groups. It is the standardized version of the
#'   Mann-Whitney statistic and estimates the probability that a randomly drawn
#'   value from one group exceeds a randomly drawn value from the other, minus
#'   the reverse probability:
#'   \eqn{\delta = (\#\{x > y\} - \#\{x < y\}) / (n_1 n_2)}. It ranges from
#'   \code{-1} to \code{1} and, unlike the rank-biserial \code{r}, is unaffected
#'   by ties beyond their contribution to the counts.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/two-groups/wilcoxon-test-in-r}{Wilcoxon Test in R}
#'  for a worked walkthrough.
#'
#' @inheritParams wilcox_effsize
#' @param ... other arguments; accepted for interface compatibility with
#'   \code{\link{cohens_d}()} and \code{\link{wilcox_effsize}()} but not used
#'   (Cliff's delta has no test backend to forward them to). \code{paired} is
#'   rejected: the statistic is defined for two independent samples only.
#' @param data a data frame containing the variables in the formula.
#' @param formula a formula of the form \code{x ~ group} where \code{x} is a
#'   numeric variable and \code{group} is a factor with two or more levels.
#' @param ci if \code{TRUE}, a percentile bootstrap confidence interval is
#'   computed and added as the columns \code{conf.low} and \code{conf.high}, as
#'   for \code{\link{cohens_d}()} and \code{\link{wilcox_effsize}()}.
#'
#' @return a tibble with one row per comparison and the columns \code{.y.},
#'   \code{group1}, \code{group2}, \code{effsize} (Cliff's delta), \code{n1},
#'   \code{n2} and \code{magnitude}; \code{conf.low} / \code{conf.high} are added
#'   when \code{ci = TRUE}.
#'
#' @details The magnitude thresholds are those of Romano et al. (2006):
#'   |delta| < 0.147 "negligible", < 0.33 "small", < 0.474 "medium", otherwise
#'   "large". Cliff's delta is algebraically identical to the rank-biserial
#'   correlation, so the point estimate equals
#'   \code{effectsize::rank_biserial()}.
#'
#' @references
#'   Cliff, N. (1993). Dominance statistics: Ordinal analyses to answer ordinal
#'   questions. \emph{Psychological Bulletin}, 114(3), 494-509.
#'
#'   Romano, J., Kromrey, J. D., Coraggio, J., & Skowronek, J. (2006). Appropriate
#'   statistics for ordinal level data. Annual meeting of the Florida Association
#'   of Institutional Research.
#'
#' @examples
#' # Two-samples Cliff's delta
#' ToothGrowth %>% cliff_delta(len ~ supp)
#'
#' # Pairwise comparisons
#' ToothGrowth %>% cliff_delta(len ~ dose)
#'
#' # Grouped data
#' ToothGrowth %>%
#'   dplyr::group_by(supp) %>%
#'   cliff_delta(len ~ dose)
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/two-groups/wilcoxon-test-in-r}{Wilcoxon Test in R}.
#' @export
cliff_delta <- function(data, formula, comparisons = NULL, ref.group = NULL,
                        ci = FALSE, conf.level = 0.95, ci.type = "perc",
                        nboot = 1000, ...,
                        boot.parallel = getOption("boot.parallel", "no"),
                        boot.ncpus = getOption("boot.ncpus", 1L)){

  # Cliff's delta is defined for two independent samples only. `...` is
  # otherwise absorbed silently (for interface parity with cohens_d() /
  # wilcox_effsize()), so a `paired` request must be rejected explicitly rather
  # than returning the independent-samples statistic for it.
  if(isTRUE(list(...)$paired))
    stop("Cliff's delta is defined for two independent samples; `paired` is not ",
         "supported. For a paired rank-based effect size, use ",
         "wilcox_effsize(paired = TRUE, method = \"rank_biserial\").",
         call. = FALSE)
  env <- as.list(environment())
  # As in cohens_d()/wilcox_effsize(): the bootstrap-execution arguments are not
  # part of the statistical call, so they are excluded from the stashed args.
  args <- env %>%
    remove_item(c("boot.parallel", "boot.ncpus")) %>%
    .add_item(method = "cliff_delta")
  # Cliff's delta is a pure effect size with no associated test statistic, so
  # the tidy output is complete without a `detailed` view (unlike wilcox_effsize,
  # whose detailed form exposes the underlying Wilcoxon statistic). detailed is
  # forced FALSE so remove_details() strips the internal NA statistic/p columns.
  params <- c(env, list(...)) %>%
    remove_null_items() %>%
    add_item(method = "cliff.delta", detailed = FALSE)

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
    mutate(magnitude = get_cliff_delta_magnitude(.data$effsize)) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "cliff_delta"))
  warn_undefined_boot_ci(res, ci)
  res
}


# The stat function called by two_sample_test()/pairwise_two_sample_test() for
# each pair. Returns an htest whose `estimate` is Cliff's delta, mirroring
# cohens.d(). Cliff's delta is a two-independent-samples statistic.
cliff.delta <- function(x, y = NULL, ci = FALSE, conf.level = 0.95,
                        ci.type = "perc", nboot = 1000, ...,
                        boot.parallel = getOption("boot.parallel", "no"),
                        boot.ncpus = getOption("boot.ncpus", 1L)){
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  if(is.null(y))
    stop("Cliff's delta requires two independent samples.", call. = FALSE)
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  delta <- get_cliff_delta(x, y)
  if(ci == TRUE){
    data <- data.frame(
      value = c(x, y),
      .grp. = factor(rep(c("g1", "g2"), times = c(length(x), length(y))))
    )
    stat.func <- function(data, subset){
      d <- data[subset, , drop = FALSE]
      get_cliff_delta(d$value[d$.grp. == "g1"], d$value[d$.grp. == "g2"])
    }
    # Stratified resampling: delta compares two fixed groups, so each replicate
    # keeps n1 and n2. An unstratified resample of the pooled rows can lose a
    # whole (small) group, which aborted the call mid-bootstrap.
    CI <- get_boot_ci(
      data, stat.func, conf.level = conf.level, type = ci.type,
      nboot = nboot, parallel = boot.parallel, ncpus = boot.ncpus,
      strata = data$.grp.
    )
  }
  RVAL <- list(statistic = NA, p.value = NA, method = "Cliff's delta",
               data.name = DNAME, estimate = delta)
  if(ci){
    attr(CI, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CI))
  }
  names(RVAL$estimate) <- "Cliff's delta"
  class(RVAL) <- "htest"
  RVAL
}

# Cliff's delta = (#{x>y} - #{x<y}) / (n1 * n2). Ties contribute 0. Written as a
# difference of pair counts rather than mean(sign(outer(...))) on purpose: a
# sign() call here would make R CMD check read the internal sign.test() function
# as an S3 method of the base sign() generic and emit a spurious consistency NOTE.
# Only NA/NaN observations are removed: an Inf compares like any other value
# (Inf > y counts as a win, Inf vs Inf as a tie), so dropping it would break the
# formula's n1 * n2 denominator and the rank-biserial equivalence.
get_cliff_delta <- function(x, y){
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if(length(x) < 1L || length(y) < 1L)
    stop("not enough non-missing observations", call. = FALSE)
  (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
}

# Magnitude thresholds from Romano et al. (2006).
get_cliff_delta_magnitude <- function(d){
  magnitude.levels <- c(0.147, 0.33, 0.474, Inf)
  magnitude <- c("negligible", "small", "medium", "large")
  d.index <- findInterval(abs(d), magnitude.levels) + 1
  factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
}
