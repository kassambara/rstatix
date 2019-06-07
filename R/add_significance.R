#' @include utilities.R
NULL
#' Add P-value Significance Symbols
#' @description Add p-value significance symbols into a data frame.
#' @param data a data frame containing a p-value column.
#' @param p.col column name containing p-values.
#' @param output.col the output column name to hold the adjusted p-values.
#' @param cutpoints numeric vector used for intervals.
#' @param symbols character vector, one shorter than cutpoints, used as
#'   significance symbols.
#'
#' @return a data frame
#'
#' @examples
#' # Perform pairwise comparisons and adjust p-values
#' ToothGrowth %>%
#'  t_test(len ~ dose) %>%
#'  adjust_pvalue() %>%
#'  add_significance("p.adj")
#'
#' @rdname add_significance
#' @export
add_significance <- function(
  data, p.col, output.col,
  cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05,  1),
  symbols = c("****", "***", "**", "*",  "ns")
)
{
  .attributes <- get_test_attributes(data)
  if(missing(p.col))
    p.col <- .guess_pvalue_column(data)
  if(missing(output.col))
    output.col <- paste0(p.col, ".signif")

  .p.signif <- data %>% pull(p.col) %>%
    stats::symnum(cutpoints = cutpoints, symbols = symbols) %>%
    as.character()

  data %>%
    mutate(!!output.col := .p.signif) %>%
    set_test_attributes(.attributes)
}

