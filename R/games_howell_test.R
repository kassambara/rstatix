#' @include utilities.R t_test.R
NULL
#' Games Howell Post-hoc Tests
#'
#'@description Performs Games-Howell test, which is used to compare all possible
#'  combinations of group differences when the assumption of homogeneity of
#'  variances is violated. This post hoc test provides confidence intervals for
#'  the differences between group means and shows whether the differences are
#'  statistically significant.
#'@inheritParams t_test
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n1,n2}: Sample counts. \item \code{mean.diff, conf.low, conf.high}:
#'  mean difference and its confidence intervals. \item \code{statistic}: Test
#'  statistic (t-value) used to compute the p-value. \item \code{df}: degrees of
#'  freedom calculated using Welch’s correction. \item \code{p}: p-value.
#'  \item \code{method}: the statistical test used to compare groups. \item
#'  \code{p.signif}: the significance level of p-values. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details The p-values are computed from the studentized range distribution.
#'
#'@references Aaron Schlege, https://rpubs.com/aaronsc32  and https://rpubs.com/aaronsc32/games-howell-test.
#'
#' @examples
#' ToothGrowth %>% games_howell_test(len ~ dose)
#'
#' @rdname games_howell_test
#'@export
games_howell_test <- function(data, formula, conf.level = 0.95, detailed = FALSE){
  args <- as.list(environment()) %>%
    .add_item(method = "games_howell_test")

  if(is_grouped_df(data)){
    results <- data %>%
      doo(games_howell_test, formula) %>%
      set_attrs(args = args) %>%
      add_class(c("rstatix_test", "games_howell_test"))
    return(results)
  }
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups == 1){
    stop("all observations are in the same group")
  }

  data <- data %>%
    select(!!!syms(c(outcome, group))) %>%
    get_complete_cases() %>%
    .as_factor(group)

  x <- data %>% pull(!!outcome)
  g <- data %>% pull(!!group)
  if (!all(is.finite(g)))
    stop("all group levels must be finite")

  # Statistics for games howell tests
  grp.sizes <- tapply(x, g, length)
  nb.groups <- length(grp.sizes)
  grp.means <- tapply(x, g, mean)
  grp.vars <- tapply(x, g, stats::var)
  # Helper functions
  get_mean_diff <- function(i, j){
    grp.means[i] - grp.means[j]
  }
  get_weltch_sd <- function(i, j){
    sqrt((grp.vars[i]/grp.sizes[i]) + (grp.vars[j]/grp.sizes[j]))
  }
  get_degree_of_freedom <- function(i, j){
    A <- ((grp.vars[i]/grp.sizes[i]) + (grp.vars[j]/grp.sizes[j]))^2
    B <- ((grp.vars[i]/grp.sizes[i])^2)/(grp.sizes[i] - 1)
    C <- ((grp.vars[j]/grp.sizes[j])^2)/(grp.sizes[j] - 1)
    A/(B+C)
  }

  mean.diff <- stats::pairwise.table(
    get_mean_diff, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix()

  weltch.sd <- stats::pairwise.table(
    get_weltch_sd, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix()

  df <- stats::pairwise.table(
    get_degree_of_freedom, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix()

  t <- mean.diff$value/weltch.sd$value
  p <- stats::ptukey(t*sqrt(2), nb.groups, df$value, lower.tail = FALSE)
  se <- weltch.sd$value*sqrt(0.5)

  q <- stats::qtukey(p = conf.level, nb.groups, df = df$value)
  conf.high <- mean.diff$value + q*se
  conf.low <- mean.diff$value - q*se

  n1 <- grp.sizes[mean.diff$group1]
  n2 <- grp.sizes[mean.diff$group2]

  results <- mean.diff %>%
    rename(mean.diff = .data$value) %>%
    mutate(
      conf.low = conf.low, conf.high = conf.high,
      se = se, statistic = t, df = df$value, p = p_round(p, digits = 3),
      method = "Games-Howell"
    ) %>%
    add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    add_column(.y. = outcome, .before = "group1") %>%
    add_significance("p") %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "games_howell_test"))
  if(!detailed){
    results <- results %>%
      select(-.data$se, -.data$method)
  }
  results
}





