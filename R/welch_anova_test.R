#' @include utilities.R
NULL
#' Welch One-Way ANOVA Test
#'
#' @description Tests for equal means in a one-way design (not assuming equal
#'   variance). A wrapper around the base function
#'   \code{\link[stats]{oneway.test}()}. This is is an alternative to the
#'   standard one-way ANOVA in the situation where the homogeneity of variance
#'   assumption is violated.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}
#'  for a worked walkthrough.
#' @param data a data frame containing the variables in the formula.
#' @param formula a formula specifying the ANOVA model similar to aov. Can be of
#'   the form y ~ group where y is a numeric variable giving the data values and
#'   group is a factor with one or multiple levels giving the corresponding
#'   groups. For example, formula = TP53 ~ cancer_group.
#' @return return a data frame with the following columns: \itemize{ \item
#'   \code{.y.}: the y variable used in the test. \item \code{n}: sample count.
#'   \item \code{statistic}: the value of the test statistic. \item \code{p}:
#'   p-value. \item \code{method}: the statistical test used to compare groups.}
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Welch one-way ANOVA test (not assuming equal variance)
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% welch_anova_test(len ~ dose)
#'
#' # Grouped data
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>%
#'   group_by(supp) %>%
#'   welch_anova_test(len ~ dose)
#' @name welch_anova_test
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}.
#' @export
welch_anova_test <- function(data, formula){
  args <- as.list(environment()) %>%
    .add_item(method = "welch_anova_test")
  data %>%
    doo(oneway_test, formula) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "welch_anova_test"))
}

oneway_test <- function(data, formula){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  # Report the number of observations the test used, not the rows supplied
  # (#334). oneway.test() drops a row whose value for ANY term of the formula is
  # missing, so nrow(data) over-reported n on data with NAs - 60 where 40 rows
  # were usable - and disagreed with anova_test() and kruskal_test() on the same
  # data.
  #
  # Counted from the formula's VARIABLES rather than from a model frame, on
  # purpose. Building a model frame here would evaluate the formula a second
  # time, and for a formula whose terms are not a pure function of the data -
  # anything containing a random draw, say - that changes the statistic itself
  # (n from one evaluation, F from the next), advances .Random.seed twice so
  # every later random number in the caller's script moves, and consumes a
  # once-per-session warning that oneway.test()'s own evaluation would then not
  # re-raise. all.vars() reads the names without evaluating anything.
  #
  # The cost is that a row lost to a non-finite TRANSFORMED value, as in
  # log(len) with a non-positive len, is still counted: that needs the
  # transformation evaluated, which is what this avoids. Such a call keeps the
  # count it had. Only the variables present in `data` are used, so a formula
  # referring to something in the calling environment cannot error here.
  n.vars <- intersect(all.vars(formula), colnames(data))
  n <- if(length(n.vars) > 0){
    sum(stats::complete.cases(data[, n.vars, drop = FALSE]))
  } else nrow(data)
  res <- stats::oneway.test(formula, data = data, var.equal = FALSE)
  tibble(
    .y. = outcome, n = n,
    statistic = round_value(res$statistic, 2),
    DFn = res$parameter[1],
    DFd = res$parameter[2],
    p = round_value(res$p.value, 3),
    method = "Welch ANOVA"
  )
}

