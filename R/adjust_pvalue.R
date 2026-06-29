#' @include utilities.R
NULL
#' Adjust P-values for Multiple Comparisons
#' @description A pipe-friendly function to add an adjusted p-value column into
#'   a data frame. Supports grouped data.
#' @details For \strong{grouped data} (and, equivalently, when a test is run on
#'   data grouped with \code{dplyr::group_by()} using an in-test
#'   \code{p.adjust.method}), the p-value adjustment is computed \strong{within
#'   each group separately}, not across all groups. If you instead want a single
#'   family of comparisons adjusted across all groups, run the test without
#'   adjustment (\code{p.adjust.method = "none"}) and then call
#'   \code{adjust_pvalue()} on the combined result (see the grouped example
#'   below).
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
#' # Grouped data: adjustment within vs across groups
#' # Per-group adjustment (within each supp level):
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   t_test(len ~ dose)                    # in-test holm, adjusted within each group
#'
#' # One family across ALL comparisons (all groups together):
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   t_test(len ~ dose, p.adjust.method = "none") %>%
#'   adjust_pvalue(method = "holm")
#'
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(data, p.col = NULL, output.col = NULL, method = "holm"){
  if (is_grouped_df(data)) {
    res <- data %>%
      doo(adjust_pvalue, p.col, output.col, method = method)
    return(res)
  }
  .attributes <- get_test_attributes(data)
  if(!is.null(.attributes$args)){
    .attributes$args$p.adjust.method = method
  }
  p.adjust <- stats::p.adjust
  p.adjust.method <-  method

  # Guess p-value columns if missing
  if(is.null(p.col))
    p.col <- data %>% p_detect("p")
  if(is.null(p.col))
    return(data)
  else if(!(p.col %in% colnames(data)))
    stop("The column ", p.col, "does not exist in the data")
  if(is.null(output.col))
    output.col <- paste0(p.col, ".adj")
  # Adjust p-value
  data %>%
    keep_only_tbl_df_classes() %>%
    mutate(
      !!output.col := p.adjust(!!sym(p.col), method = p.adjust.method)
    ) %>%
    set_test_attributes(.attributes)
}

