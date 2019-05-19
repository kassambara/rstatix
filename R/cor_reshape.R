#' @include utilities.R cor_mat.R
NULL
#' Reshape Correlation Data
#' @description Reshape correlation analysis results. Key functions: \itemize{
#'   \item \code{cor_gather()}: takes a correlation matrix and collapses (i.e. melt) it into a paired list
#'   (long format). \item \code{cor_spread()}: spread a long correlation data format across
#'   multiple columns. Particularly, it takes the results of \code{\link{cor_test}}
#'   and transforms it into a correlation matrix. }
#' @param data a data frame or matrix.
#' @param drop.na logical. If TRUE, drop rows containing missing values after gathering the data.
#' @param value column name containing the value to spread.
#' @seealso \code{\link{cor_mat}()}, \code{\link{cor_reorder}()}
#' @examples
#' # Data preparation
#' #::::::::::::::::::::::::::::::::::::::::::
#' mydata <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec)
#' head(mydata, 3)
#'
#' # Reshape a correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Compute a correlation matrix
#' cor.mat <- mydata %>% cor_mat()
#' cor.mat
#'
#' # Collapse the correlation matrix into long format
#' # paired list data frame
#' long.format <- cor.mat %>% cor_gather()
#' long.format
#'
#' # Spread a correlation data format
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Spread the correlation coefficient value
#' long.format %>% cor_spread(value = "cor")
#' # Spread the p-value
#' long.format %>% cor_spread(value = "p")


#' @describeIn cor_reshape takes a correlation matrix and collapses (or melt) it into long
#'   format data frame (paired list)
#' @export
cor_gather <- function(data, drop.na = TRUE){

  rowname <- column <- NULL

  if(inherits(data, "cor_mat")){
    cor.value <- data
    p.value <- data %>% cor_get_pval()
  }
  else if(inherits(data, "cor_mat_tri")){
    cor.value <- data %>% as_numeric_triangle()
    p.value <- data %>%
      cor_get_pval() %>%
      as_numeric_triangle()
  }
  else if(inherits(data, "rcorr")){
    cor.value <- data$r %>% as_tibble(rownames = "rowname")
    p.value <- data$P %>% as_tibble(rownames = "rowname")
  }
  else {
    cor.value <- data %>% as_tibble(rownames = "rowname")
    p.value <- NULL
  }

  cor.value <- cor.value %>%
    gather(key = "column", value = "cor", -rowname)

  if(!is.null(p.value)){
    p.value <- p.value %>%
      gather(key = "column", value = "p", -rowname)
    cor.value <- cor.value %>%
      left_join(p.value, by = c("rowname", "column"))
    colnames(cor.value) <- c("var1", "var2", "cor", "p")
  }
  else{
    colnames(cor.value) <- c("var1", "var2", "cor")
  }

  if(drop.na)
    cor.value <- cor.value %>% drop_na()

  cor.value
}


#' @describeIn cor_reshape spread a long correlation data frame into wide format
#'   (correlation matrix).
#' @export
cor_spread <- function(data, value = "cor"){

  var1 <- var2 <- cor <- p <- NULL
  row.vars <- data %>% pull(var1) %>% unique()
  col.vars <- data %>% pull(var2) %>% unique()

  res <- data %>%
    select(var1, var2, !!value) %>%
    spread(key = "var2", value = value) %>%
    rename(rowname = var1) %>%
    respect_variables_order(row.vars = row.vars, col.vars = col.vars)

  colnames(res)[1] <- "rowname"

  res
}


# Helper functions
# :::::::::::::::::::::::::::::::::::::::::::::::::::::

# Reorder a correlation matrix according
# to the order of variables in vars
respect_variables_order <- function(data, vars, row.vars = vars, col.vars = vars){
  data %>% subset_matrix(row.vars = row.vars, col.vars = col.vars)
}



