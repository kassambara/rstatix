#' @include utilities.R
#' @importFrom stats median
NULL
#' Levene's Test
#'
#' @description Provide a pipe-friendly framework to easily compute Levene's
#'   test for homogeneity of variance across groups.
#'
#'   Wrapper around the function \code{\link[car]{leveneTest}()}, which can
#'   additionally handles a grouped data.
#' @param data a data frame for evaluating the formula or a model
#' @param formula a formula
#' @param center The name of a function to compute the center of each group;
#'   mean gives the original Levene's test; the default, median, provides a more
#'   robust test.
#' @return a data frame with the following columns: df1, df2
#'   (df.residual), statistic and p.
#'
#' @examples
#' # Prepare the data
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Compute Levene's Test
#' df %>% levene_test(len ~ dose)
#'
#' # Grouped data
#' df %>%
#'   group_by(supp) %>%
#'   levene_test(len ~ dose)
#'
#' @export
levene_test <- function(data, formula, center = median){
  if(is_grouped_df(data)){
    results <- data %>%
      doo(~levene_test(., formula = formula, center = center))
    return(results)
  }
  else if(is_lm(data)){
    results <- car::leveneTest(data, center = center)
  }
  else{
    results <- car::leveneTest(formula, data, center = center)
  }
  results <- broom::tidy(results) %>%
    rename(
      df1 = .data$df,
      df2 = .data$df.residual,
      p = .data$p.value
      ) %>%
    select(.data$df1, .data$df2, .data$statistic, .data$p)
  results
}
