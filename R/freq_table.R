#' @include utilities.R
NULL
#'Compute Frequency Table
#'@description compute frequency table.
#'@param data a data frame
#'@param ... One or more unquoted expressions (or variable names) separated by
#'  commas. Used to specify variables of interest.
#'@param vars optional character vector containing variable names.
#'@param na.rm logical value. If TRUE (default), remove missing values in the
#'  variables used to create the frequency table.
#'@return a data frame
#' @examples
#' data("ToothGrowth")
#' ToothGrowth %>% freq_table(supp, dose)
#'@export
freq_table <- function(data, ..., vars = NULL, na.rm = TRUE){
  if(is.vector(data) | is.factor(data)){
    data <- data.frame(group = data)
    vars <- "group"
  }
  data <- data %>%
    df_select(..., vars = vars)
  vars <- colnames(data)
  if(length(vars) == 0){
    stop("Specify at least one variable")
  }
  if(na.rm){
    data <- data %>%
      filter(stats::complete.cases(data))
  }
  results <- data %>%
    group_by(!!!syms(vars)) %>%
    summarise(n = n()) %>%
    mutate(prop = round(.data$n *100 / sum (.data$n), 1)) %>%
    dplyr::ungroup()
  results
}


spread_table <- function(data, vars){
  last.var <- dplyr::last(vars)
  grouping.vars <- utils::head(vars, -2)
  if(length(vars) >= 2){
    data <- data %>%
      select(-.data$prop) %>%
      group_by(!!!syms(grouping.vars)) %>%
      nest() %>%
      mutate(data = map(.data$data, spread, key = last.var, value = "n"))
  }
  data

}
