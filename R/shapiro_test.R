#' @include utilities.R
#' @importFrom stats shapiro.test
#' @importFrom stats complete.cases
NULL
#' Shapiro-Wilk Normality Test
#'
#' @description Provides a pipe-friendly framework to performs Shapiro-Wilk test
#'   of normality. Support grouped data and multiple variables for multivariate
#'   normality tests. Wrapper around the R base function
#'   \code{\link[stats]{shapiro.test}()}. Can handle grouped data. Read more:
#'   \href{https://www.datanovia.com/en/lessons/normality-test-in-r/}{Normality
#'   Test in R}.
#' @param data a data frame. Columns are variables.
#' @param ... One or more unquoted expressions (or variable names) separated by
#'   commas. Used to select a variable of interest.
#' @param vars optional character vector containing variable names. Ignored when
#'   dot vars are specified.
#' @return  a data frame containing the value of the Shapiro-Wilk statistic and
#'   the corresponding p.value.
#' @examples
#'
#' # Shapiro Wilk normality test for one variable
#' iris %>% shapiro_test(Sepal.Length)
#'
#' # Shapiro Wilk normality test for two variables
#' iris %>% shapiro_test(Sepal.Length, Petal.Width)
#'
#' # Multivariate normality test
#' mshapiro_test(iris[, 1:3])
#'

#' @describeIn shapiro_test univariate Shapiro-Wilk normality test
#' @export
shapiro_test <- function(data, ..., vars = NULL){
  if(is_grouped_df(data)){
    results <- data %>%
      doo(shapiro_test, ..., vars = vars)
    return(results)
  }
  else if(is.numeric(data)){
    results <- shapiro.test(data)
    data.name <- deparse(substitute(data))
    results <-  tidy(results) %>%
      add_column(variable = data.name, .before = 1) %>%
      select(-.data$method)
    return(results)
  }
  vars <- c(get_dot_vars(...), vars) %>%
    unique()
  n.vars <- length(vars)
  if(.is_empty(vars)){
    stop("Specify one or more variables...")
  }
  data <- data %>%
    select(!!!syms(vars))
  variable <- value <- method <-  NULL
  data <- data %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))
  data %>%
    group_by(variable) %>%
    doo(~tidy(shapiro.test(.$value))) %>%
    select(-.data$method) %>%
    rename(p = .data$p.value)
}


#' @describeIn shapiro_test multivariate Shapiro-Wilk normality test. This is a
#'   modified copy of the \code{mshapiro.test()} function of the package
#'   mvnormtest, for internal convenience.
#' @export
mshapiro_test <- function(data)
{
  if(is_grouped_df(data)){
    results <- data %>%
      doo(~shapiro.test(.))
  }
  x <- data
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  x <- x[complete.cases(x), ]
  x <- t(x)
  n <- ncol(x)
  if (n < 3 || n > 5000) {
    stop("sample size must be between 3 and 5000")
  }
  rng <- range(x)
  rng <- rng[2] - rng[1]
  if (rng == 0) {
    stop("all `x[]' are identical")
  }
  Us <- apply(x, 1, mean)
  R <- x - Us
  M.1 <- solve(R %*% t(R), tol = 1e-18)
  Rmax <- diag(t(R) %*% M.1 %*% R)
  C <- M.1 %*% R[, which.max(Rmax)]
  Z <- t(C) %*% x
  result <- shapiro.test(Z)
  result$method <- "Multivariate Shapiro-Wilk normality test"
  result$data.name <- paste("(", paste(rownames(x), collapse = ","),
                            ")", sep = "")

  tidy(result) %>%
    select(-.data$method)
}
