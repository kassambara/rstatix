#' @include utilities.R
NULL
#'Make Clean Names
#'
#'
#'@description Pipe-friendly function to make syntactically valid names out of
#'  character vectors.
#'
#' @param data a data frame or vector
#' @return a data frame or a vector depending on the input data
#'
#' @examples
#'
#' # Vector
#' make_clean_names(c("a and b", "a-and-b"))
#' make_clean_names(1:10)
#'
#' # data frame
#' df <- data.frame(
#' `a and b` = 1:4,
#' `c and d` = 5:8,
#'  check.names = FALSE
#' )
#' df
#' make_clean_names(df)
#'
#' @export
make_clean_names <- function(data){
  if(is.vector(data)){
    data <- make.names(data)
  }
  else if(inherits(data, c("data.frame", "matrix"))){
    .colnames <- colnames(data) %>% make.names()
    colnames(data) <- .colnames
  }
  data
}
