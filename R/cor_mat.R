#' @include utilities.R
NULL
#'Correlation Matrix with P-values
#'@description Compute correlation matrix with p-values. Numeric columns in the
#'  data are detected and automatically selected.
#'@inheritParams cor_test
#'@param x an object of class \code{cor_mat}
#'@param cutpoints numeric vector used for intervals. Default values are
#'  \code{c(0, 0.25, 0.5, 0.75, 1)}.
#'@param symbols character vector, one shorter than cutpoints, used as
#'  correlation coefficient symbols. Default values are \code{c(" ", ".",  "+",
#'  "*")}.
#'@param vars a character vector containing at least two variable names.
#'@param ... Additional arguments. \itemize{ \item In \code{cor_mat()}: Other
#'  arguments passed to the function \code{\link{cor_test}()} \item In
#'  \code{subset_cor_mat()}: One or more unquoted expressions separated by
#'  commas. These arguments are passed to the function
#'  \code{\link[dplyr]{select}}() }
#'@return a data frame
#'@seealso cor_test
#' @examples
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' df <- mtcars[, c(1,3,4,5,6,7)]
#' cor.mat <- cor_mat(df)
#' cor.mat
#'
#' # Pull lower/upper triangle parts
#' #::::::::::::::::::::::::::::::::::::::::::
#'
#' # Pull lower triangular part
#' cor.mat %>% pull_lower_triangle()
#'
#' # Pull upper triangular part
#' cor.mat %>% pull_upper_triangle()
#'
#'
#' # Replace correlation coefficient by symbols
#' #::::::::::::::::::::::::::::::::::::::::::
#'cor.mat %>%
#'  replace_cor_by_symbols(
#'    cutpoints = c(0, 0.25, 0.5, 0.75, 1),
#'    symbols = c(" ", ".",  "+", "*")
#'  ) %>%
#'  pull_lower_triangle()
#'
#' # Reorder by correlation and get p-values
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Reorder
#' cor.mat %>%
#'   reorder_cor_mat()
#' # Get p-values of the reordered cor_mat
#' cor.mat %>%
#'   reorder_cor_mat() %>%
#'   get_cor_mat_pval()
#'
#' # Subsetting correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#'
#' # Select some variables of interest
#' cor.mat %>%
#'   subset_cor_mat(mpg, drat, wt)
#'
#' # Remove variables
#' cor.mat %>%
#'   subset_cor_mat(-mpg, -wt)
#'
#'@describeIn cor_mat compute correlation matrix with p-values.
#'@export
cor_mat <- function(data, vars = NULL, method = "pearson", ...){

  res.cor.test <- cor_test(data, vars = vars, method = method, ...)
  cor.mat <- res.cor.test %>%
    spread_cor_test(value = "cor")

  attr(cor.mat, "cor_test") <- res.cor.test
  structure(cor.mat, class = c(class(cor.mat), "cor_mat") )
}

#' @describeIn cor_mat return a correlation matrix p-values.
#' @export
get_cor_mat_pval <- function(x){

  if(!.is_cor_mat(x)){
    stop("x should be an object of class cor_mat")
  }
  col.vars <- colnames(x)[-1]
  row.vars <- pull(x, 1)[-1]
  pvals <- x %>%
    attr("cor_test") %>%
    spread_cor_test(value = "p") %>%
    .respect_variables_order(row.vars = row.vars, col.vars = col.vars)

  pvals
}


#' @describeIn cor_mat reorder correlation matrix, according to the coefficients,
#'   using the hierarchical clustering method.
#' @export
reorder_cor_mat <- function(x){

  cor.test <- attr(x, "cor_test")

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  hc <- stats::as.dist(1 - x) %>%
    stats::hclust(method = "complete")

  x <- x[hc$order, hc$order] %>%
    .matrix_to_tibble()

  if(!is.null(cor.test)){
    attr(x, "cor_test") <- cor.test
    x <- structure(x, class = c(class(x), "cor_mat") )
  }

  x
}


#' @describeIn cor_mat replace correlation coefficients by symbols
#' @export
replace_cor_by_symbols <- function( x, cutpoints = c(0, 0.25, 0.5, 0.75, 1),
                                    symbols = c(" ", ".",  "+", "*"))
{

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  res <- stats::symnum(
    abs(x), cutpoints = cutpoints, symbols = symbols,
    abbr.colnames = FALSE
    ) %>%
    structure(class = "matrix") %>% # overwrite "noquote" class
    .matrix_to_dataframe()

  structure(res, class = c("data.frame", "tbl_df"))
}

#' @describeIn cor_mat subset of a correlation matrix
#' @export
subset_cor_mat <- function(x, ..., vars = NULL){

  if(!inherits(x, "tbl_df"))
    x <- .matrix_to_tibble(x)
  vars2 <- rlang::quos(...)

  . <- name <- NULL

  if(length(vars2) > 0){
    .name <- x %>% pull(name)
    x <- x %>%
      select(...) %>%
      mutate(name = .name) %>%
      select(name, everything())
    vars <- c(vars, colnames(x)[-1])
  }
  if(!is.null(vars)){
    vars <- unique(vars)
      x <- x %>%
        select(!!c("name", vars)) %>%
        filter(name %in% vars)
  }

  x
}

