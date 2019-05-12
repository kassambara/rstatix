#' @include utilities.R
NULL
#' Sample n Rows By Group From a Table
#'
#' @description sample n rows by group from a table using the \code{\link[dplyr]{sample_n}()} function.
#'
#' @param data a data frame
#' @param ... Variables to group by
#' @param size the number of rows to select
#' @param replace with or without replacement?
#'
#' @examples
#' ToothGrowth %>% sample_n_by(dose, supp, size = 2)
#' @name sample_n_by
#' @export
sample_n_by <- function(data, ..., size = 1, replace = FALSE){
  data %>%
    group_by(...) %>%
    dplyr::sample_n(size = size, replace = replace) %>%
    dplyr::ungroup()
}
