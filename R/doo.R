#' @include utilities.R
NULL
#'Alternative to dplyr::do for Doing Anything
#'
#'
#'@description Provides a flexible alternative to the \code{dplyr:do()} function.
#'  Technically it uses \code{nest() + mutate() + map()} to apply arbitrary
#'  computation to a grouped data frame.
#'
#'  The output is a data frame. If the applied function returns a data frame,
#'  then the output will be automatically unnested. Otherwise, the output includes the grouping
#'  variables and a column named ".results." (by default), which is a "list-columns"
#'  containing the results for group combinations.
#'
#'@param data a (grouped) data frame
#'@param .f A function, formula, or atomic vector. For example
#'  \code{~t.test(len ~ supp, data = .)}.
#' @param ... Additional arguments passed on to .f
#' @param result the column name to hold the results. Default is ".results.".
#' @return a data frame
#' @examples
#' # Example 1: pipe-friendly t_test().
#' # Two possibilities of usage are available
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Use this
#' ToothGrowth %>%
#'   group_by(dose) %>%
#'   doo(~t_test(data =., len ~ supp))
#'
#' # Or this
#' ToothGrowth %>%
#'   group_by(dose) %>%
#'   doo(t_test, len ~ supp)
#'
#' # Example 2: R base function t.test() (not pipe friendly)
#' # One possibility of usage
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' comparisons <- ToothGrowth %>%
#'    group_by(dose) %>%
#'    doo(~t.test(len ~ supp, data =.))
#' comparisons
#' comparisons$.results.
#'
#' # Example 3: R base function combined with tidy()
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' ToothGrowth %>%
#'    group_by(dose) %>%
#'    doo(~t.test(len ~ supp, data =.) %>% tidy())
#'@export
doo <- function(data, .f, ..., result = ".results."){
  .nested <- data %>%
    nest() %>%
     mutate(data = map(data, droplevels))
  .computed <- .nested %>%
    pull(data) %>%
    map(.f, ...)
  .results <- .nested %>%
    select(-data) %>%
    mutate(!!result := .computed)
  if(inherits(.computed[[1]], c("data.frame", "tbl_df"))){
    # Suppress warning such as:
    #  Binding character and factor vector, coercing into character vector
    .results <- suppressWarnings(unnest(.results))
  }
  if(is_grouped_df(data)){
    .groups <- dplyr::group_vars(data)
    .results <- dplyr::arrange(.results, !!!syms(.groups))
  }
  .results
}

