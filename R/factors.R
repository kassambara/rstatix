#' @include utilities.R
NULL

#'Factors
#'
#'@description Provides pipe-friendly functions to convert simultaneously
#'  multiple variables into a factor variable.
#'
#'  Helper functions are also available to set the reference level and the
#'  levels order.
#'
#'@param data a data frame
#'@param ... one unquoted expressions (or variable name) specifying the name of
#'  the variables you want to convert into factor. Alternative to the argument
#'  \code{vars}.
#'@param vars a character vector specifying the variables to convert into
#'  factor.
#'@param name a factor variable name.
#'@param ref the reference level
#'@param order a character vector specifying the order of the
#'  factor levels
#'
#'@examples
#' # Create a demo data
#' df <- tibble(
#'   group = c("a", "a", "b", "b", "c", "c"),
#'   time = c("t1", "t2", "t1", "t2", "t1", "t2"),
#'   value = c(5, 6, 1, 3, 4, 5)
#' )
#' df
#' # Convert group and time into factor variable
#' result <- df %>% convert_as_factor(group, time)
#' result
#' # Show group levels
#' levels(result$group)
#'
#' # Set c as the reference level (the first one)
#' result <- result %>%
#'   set_ref_level("group", ref = "c")
#' levels(result$group)
#'
#' # Set the order of levels
#' result <- result %>%
#'   reorder_levels("group", order = c("b", "c", "a"))
#' levels(result$group)

#' @describeIn factors Convert one or multiple variables into factor.
#' @export
convert_as_factor <- function(data, ..., vars = NULL){
  vars <- c(get_dot_vars(...), vars) %>%
    unique()
  if(!.is_empty(vars)){
    data <- data %>%
      dplyr::mutate_at(vars, as.factor)
  }
  data
}

#' @describeIn factors Change a factor reference level or group.
#' @export
set_ref_level <- function(data, name, ref){
  data[[name]] <- stats::relevel(data[[name]], ref)
  data
}

#' @describeIn factors Change the order of a factor levels
#' @export
reorder_levels <- function(data, name, order){
  data[[name]] <- factor(data[[name]], levels = order)
  data
}
