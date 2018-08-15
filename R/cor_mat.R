#' @include utilities.R
NULL
#' Correlation Matrix with P-values
#' @description Compute correlation matrix with p-values. Numeric columns in the
#'   data are detected and automatically selected.
#' @inheritParams cor_test
#' @param x a correlation matrix
#' @param cutpoints numeric vector used for intervals. Default values are \code{c(0, 0.25, 0.5, 0.75, 1)}.
#' @param symbols character vector, one shorter than cutpoints, used as
#'   correlation coefficient symbols. Default values are \code{c(" ", ".",  "+", "*")}.
#' @param vars a character vector containing at least two variable names.
#' @param ... Other arguments passed to the function \code{\link{cor_test}()}
#' @return a data frame
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
#' @describeIn cor_mat compute correlation matrix with p-values.
#' @export
cor_mat <- function(data, vars = NULL,  ...){

  res.cor.test <- cor_test(data, vars = vars, ...)


  var1 <- var2 <- cor <- NULL
  vars <- res.cor.test %>%
    pull(var2) %>% unique()

  cor.mat <- res.cor.test %>%
    select(var1, var2, cor) %>%
    spread(key = "var1", value = "cor") %>%
    .respect_variables_order(vars)
  colnames(cor.mat)[1] <- "name"
  attr(cor.mat, "cor_test") <- res.cor.test

  structure(cor.mat, class = c(class(cor.mat), "cor_mat") )
}

#' @describeIn cor_mat reorder correlation matrix, according the
#'   coefficients, using the hierarchical clustering method.
reorder_cor_mat <- function(x){

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  hc <- stats::as.dist(1 - x) %>%
    stats::hclust(method = "complete")

  x[hc$order, hc$order] %>%
    .matrix_to_tibble()
}


#' @describeIn cor_mat Replace correlation coefficients by symbols
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

#' @describeIn cor_mat Subset of a correlation matrix
#' @export
subset_cor_mat <- function(x, vars){

  vars <- unique(vars)
  if(length(vars) == 1)
    stop("You should provide, at least, two variables", call. = FALSE)

  if(inherits(x, "tbl_df"))
    x <- .tibble_to_matrix(x)

  x[vars, vars] %>% .matrix_to_tibble()
}


#' @describeIn cor_mat alias of subset_cor_mat()
#' @export
pull_variables <- function(x, vars){
  subset_cor_mat(x,vars)
}


# Helper function
#:::::::::::::::::::::::::::::::::::::
.respect_variables_order <- function(x, vars){
  subset_cor_mat(x, vars)
}
