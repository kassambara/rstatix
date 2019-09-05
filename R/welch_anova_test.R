#' @include utilities.R
NULL
#' Welch One-Way ANOVA Test
#'
#' @description Tests for equal means in a one-way design (not assuming equal
#'   variance). A wrapper around the base function
#'   \code{\link[stats]{oneway.test}()}. This is is an alternative to the
#'   standard one-way ANOVA in the situation where the homogeneity of variance
#'   assumption is violated.
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
  res <- stats::oneway.test(formula, data = data, var.equal = FALSE)
  tibble(
    .y. = outcome, n = nrow(data),
    statistic = round_value(res$statistic, 2),
    DFn = res$parameter[1],
    DFd = res$parameter[2],
    p = round_value(res$p.value, 3),
    method = "Welch ANOVA"
  )
}

