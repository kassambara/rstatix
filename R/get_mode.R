#' @include utilities.R
NULL
#' Compute Mode
#'
#' @description Compute the mode in a given vector. Mode is the most frequent
#'   value.
#'
#' @param x a vector. Can be numeric, factor or character vector.
#'
#' @examples
#'
#' # Mode of numeric vector
#' x <- c(1:5, 6, 6, 7:10)
#' get_mode(x)
#'
#' # Bimodal
#' x <- c(1:5, 6, 6, 7, 8, 9, 9, 10)
#' get_mode(x)
#'
#' # No mode
#' x <- c(1, 2, 3, 4, 5)
#' get_mode(x)
#'
#' # Nominal vector
#' fruits <-  c(rep("orange", 10), rep("apple", 5), rep("lemon", 2))
#' get_mode(fruits)
#' @export
get_mode <- function(x){
  .x <- factor(x)
  .table <- table(.x)
  .max <- max(.table)
  if(all(.table == .max)){
    .mode <- NA
  }
  else{
   .mode <- names(.table)[.table == .max]
  }
  if(is.numeric(x)){
    .mode <- as.numeric(.mode)
  }
  .mode
}
