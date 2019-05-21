#' @include utilities.R
NULL
# Frequency Table
freq_table <- function(data, ..., vars = NULL){
  vars <- c(get_dot_vars(...), vars) %>%
    unique()
  results <- data %>%
    group_by(!!!syms(vars)) %>%
    summarise(n = n()) %>%
    mutate(prop = round(.data$n *100 / sum (.data$n), 1))
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
