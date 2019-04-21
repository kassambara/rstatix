#' @include utilities.R
NULL
#'Alternative to dplyr::do for Doing Anything
#'
#'
#'@description Provides a flexibe alternative to the \code{dplyr:do()} function.
#'  Technically it uses \code{nest() + mutate() + map()} to apply arbitrary
#'  computation to a grouped data frame.
#'
#'  The output is a data frame. If the applied function returns a data frame,
#'  then the output is unnested. Otherwise, the output includes the grouping
#'  variables and a column named ".results." (by defauly), which is a "list-columns"
#'  containing the results for group combinations.
#'
#'@param data a grouped data frame
#'@param formula Expressions to be applied to each group. For example
#'  \code{~t.test(len ~ supp, data = .)}.
#' @param result the column name to hold the results. Default is ".results.".
#' @return a data frame
#' @examples
#' # Example 1: pipe-friendly t_test()
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' ToothGrowth %>%
#'   group_by(dose) %>%
#'   doo(~t_test(data =., len ~ supp))
#'
#' # Example 2: R base function t.test()
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
doo <- function(data, formula, result = ".results."){

  if(!is_grouped_df(data)){
    stop("data should be a grouped data frame ",
         "as returned by dplyr::group_by")
  }
  if(!inherits(formula, "formula")){
    stop("The function should be provide as formula. ",
         "Example:  ~t.test(len ~ supp, data = .)")
  }
  .nested <- data %>%
    nest() %>%
    mutate(data = map(data, droplevels))
  .computed <- .nested %>%
    pull(data) %>%
    map(formula)
  .results <- .nested %>%
    select(-data) %>%
    mutate(!!result := .computed)
  if(inherits(.computed[[1]], c("data.frame", "tbl_df"))){
    .results <- unnest(.results)
  }
  .results
}
