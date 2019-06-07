#' @include utilities.R
#' @importFrom stats TukeyHSD
#' @importFrom dplyr everything
#' @importFrom tidyr separate
NULL
#'Tukey Honest Significant Differences
#'
#'
#'@description Provides a pipe-friendly framework to performs Tukey post-hoc
#'  tests. Wrapper around the function \code{\link[stats]{TukeyHSD}()}.
#'
#'  Can handle different inputs formats: aov, lm, formula.
#'@param x an object of class \code{aov}, \code{lm} or \code{data.frame}
#'  containing the variables used in the formula.
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param ... other arguments passed to the function
#'  \code{\link[stats]{TukeyHSD}()}. These include: \itemize{ \item
#'  \strong{which}: A character vector listing terms in the fitted model for
#'  which the intervals should be calculated. Defaults to all the terms. \item
#'  \strong{ordered}: A logical value indicating if the levels of the factor
#'  should be ordered according to increasing average in the sample before
#'  taking differences. If ordered is true then the calculated differences in
#'  the means will all be positive. The significant differences will be those
#'  for which the lwr end point is positive. }
#'@return a tibble data frame containing the results of the different
#'  comparisons.
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Tukey HSD from ANOVA results
#' aov(len ~ dose, data = df) %>% tukey_hsd()
#'
#' # two-way anova with interaction
#' aov(len ~ dose*supp, data = df) %>% tukey_hsd()
#'
#' # Tukey HSD from lm() results
#' lm(len ~ dose, data = df) %>% tukey_hsd()
#'
#' # Tukey HSD from data frame and formula
#' tukey_hsd(df, len ~ dose)
#'
#' # Tukey HSD using grouped data
#' df %>%
#'   group_by(supp) %>%
#'   tukey_hsd(len ~ dose)
#'
#'@export
tukey_hsd <- function(x, ...){
  UseMethod("tukey_hsd", x)
}


#' @export
#' @describeIn tukey_hsd performs tukey post-hoc test from \code{aov()} results.
tukey_hsd.default <- function(x, ...)
{
  comparison <- comparison2 <- adj.p.value <- p.adj <-
    term <- group1 <- group2 <- NULL
  res <- TukeyHSD(x, ...) %>%
    broom::tidy() %>%
    mutate(comparison2 = comparison) %>%
    separate(comparison2, into= c("group2", "group1"), sep = "-") %>%
    rename(p.adj = adj.p.value) %>%
    mutate(p.adj = signif(p.adj, 3)) %>%
    select(term, group1, group2, everything())
  res
}

#' @export
#' @describeIn tukey_hsd performs tukey post-hoc test from \code{lm()} model.
tukey_hsd.lm <- function(x, ...)
{
  stats::aov(x) %>%
    tukey_hsd.default(...)
}


#' @describeIn tukey_hsd performs tukey post-hoc tests using data and formula as
#'   inputs. ANOVA will be automatically performed using the function
#'   \code{\link[stats]{aov}()}
#' @export
tukey_hsd.data.frame <- function(x, formula, ...){
  args <- list(data = x, formula = formula, method = "tukey_hsd")
  if(is_grouped_df(x)){
    results <- x %>%
      doo(tukey_hsd.data.frame, formula, ...) %>%
      set_attrs(args = args) %>%
      add_class(c("rstatix_test", "tukey_hsd"))
    return(results)
  }

  stats::aov(formula, x) %>%
    tukey_hsd.default(...) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "tukey_hsd"))
}
