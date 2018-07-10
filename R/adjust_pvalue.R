#' @include utilities.R
NULL
#' Adjust P-values for Multiple Comparisons
#' @description A pipe-friendly function to add an adjusted p-value column into
#'   a data frame.
#' @param data a data frame containing a p-value column
#' @param p.col column name containing p-values
#' @param output.col the output column name to hold the adjusted p-values
#' @param method method for adjusting p values (see
#'   \code{\link[stats]{p.adjust}}). Allowed values include "holm", "hochberg",
#'   "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't want to
#'   adjust the p value (not recommended), use p.adjust.method = "none".
#' @return a data frame
#'
#' @examples
#' # Perform pairwise comparisons and adjust p-values
#' ToothGrowth %>%
#'  t_test(len ~ dose) %>%
#'  adjust_pvalue()
#'
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(data, p.col, output.col, method = "holm"){

  p.adjust <- stats::p.adjust
  p.adjust.method <-  method

  # Guess p-value columns if missing
  if(missing(p.col))
    p.col <- .guess_pvalue_column(data)
  if(missing(output.col))
    output.col <- paste0(p.col, ".adj")

  # Adjust p-value
  data %>%
    mutate(
      !!output.col := p.adjust(!!sym(p.col), method = p.adjust.method)
    )
}
