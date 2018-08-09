#' @include utilities.R
#' @importFrom stats TukeyHSD
#' @importFrom dplyr everything
#' @importFrom tidyr separate
NULL
#' Tukey Honest Significant Differences
#'
#'
#'@description Provides a pipe-friendly framework to performs Tukey post-hoc tests.
#'  Wrapper around the function \code{\link[stats]{TukeyHSD}()}.
#' @inheritParams stats::TukeyHSD
#'@param data a data.frame containing the variables in the formula.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#' @param ... other arguments passed to the function \code{\link[stats]{TukeyHSD}()}.
#'@return a tibble data frame containing the results of the different comparisons.
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Anova
#' res.aov <- aov(len ~ dose, data = df)
#' # Tukey hsd
#' tukey_hsd(res.aov)
#'
#'@describeIn tukey_hsd performs tukey post-hoc test from ANOVA results, usually an aov fit.
#'@export
tukey_hsd <- function(x, ...)
{
  comparison <- comparison2 <- adj.p.value <- p.adj <-
    term <- group1 <- group2 <- NULL
  res <- TukeyHSD(x, ...) %>%
    broom::tidy() %>%
    mutate(comparison2 = comparison) %>%
    separate(comparison2, into= c("group2", "group1"), sep = "-") %>%
    rename(p.adj = adj.p.value) %>%
    mutate(p.adj = signif(p.adj, 2)) %>%
    select(term, group1, group2, everything())
  res
}


#' @describeIn tukey_hsd performs tukey post-hoc tests using data and formula as
#'   inputs. ANOVA will be automatically performed using the function
#'   \code{\link[stats]{aov}()}
#' @export
tukey_hsd2 <- function(data, formula, ...){

  if(is_grouped_df(data)){
    . <- NULL
    results <- data %>%
      do(tukey_hsd2(data =., formula, ...)) %>%
      ungroup()
    return(results)
  }

  x <- stats::aov(formula, data)
  tukey_hsd(x, ...)
}



