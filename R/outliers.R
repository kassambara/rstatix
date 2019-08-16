#' @include utilities.R
#' @importFrom stats quantile
#' @importFrom stats IQR
NULL
#'Identify Univariate Outliers Using Boxplot Methods
#'
#'
#'@description Detect outliers using boxplot methods. Boxplots are a popular and
#'  an easy method for identifying outliers. There are two categories of
#'  outlier: (1) outliers and (2) extreme points.
#'
#'  Values above \code{Q3 + 1.5xIQR} or below \code{Q1 - 1.5xIQR} are considered
#'  as outliers. Values above \code{Q3 + 3xIQR} or below \code{Q1 - 3xIQR} are
#'  considered as extreme points (or extreme outliers).
#'
#'  Q1 and Q3 are the first and third quartile, respectively. IQR is the
#'  interquartile range (IQR = Q3 - Q1).
#'
#'  Generally speaking, data points that are labelled outliers in boxplots are
#'  not considered as troublesome as those considered extreme points and might
#'  even be ignored. Note that, any \code{NA} and \code{NaN} are automatically removed
#'  before the quantiles are computed.
#'
#'@return \itemize{ \item \code{identify_outliers()}. Returns the input data
#'  frame with two additional columns: "is.outlier" and "is.extreme", which hold
#'  logical values. \item \code{is_outlier() and is_extreme()}. Returns logical
#'  vectors. }
#'
#'@param data a data frame
#'@param ... One unquoted expressions (or variable name). Used to select a
#'  variable of interest. Alternative to the argument \code{variable}.
#'@param variable variable name for detecting outliers
#'@param x a numeric vector
#'@param coef coefficient specifying how far the outlier should be from the edge
#'  of their box. Possible values are 1.5 (for outlier) and 3 (for extreme
#'  points only). Default is 1.5
#'
#'
#' @examples
#' # Generate a demo data
#' set.seed(123)
#' demo.data <- data.frame(
#'   sample = 1:20,
#'   score = c(rnorm(19, mean = 5, sd = 2), 50),
#'   gender = rep(c("Male", "Female"), each = 10)
#' )
#'
#' # Identify outliers according to the variable score
#' demo.data %>%
#'   identify_outliers(score)
#'
#' # Identify outliers by groups
#' demo.data %>%
#'   group_by(gender) %>%
#'   identify_outliers("score")

#'@describeIn outliers takes a data frame and extract rows suspected as outliers
#'  according to a numeric column. The following columns are added "is.outlier"
#'  and "is.extreme".
#'@export
identify_outliers <- function(data, ..., variable = NULL){

  is.outlier <- NULL
  if(is_grouped_df(data)){
    results <- data %>%
      doo(identify_outliers, ..., variable = variable)
    return(results)
  }

  if(!inherits(data, "data.frame"))
    stop("data should be a data frame")
  variable <- data %>% get_selected_vars(..., vars = variable)
  n.vars <- length(variable)
  if(n.vars > 1)
    stop("Specify only one variable")
  values <- data %>% pull(!!variable)
  results <- data %>%
    mutate(
      is.outlier = is_outlier(values),
      is.extreme = is_extreme(values)
      ) %>%
    filter(is.outlier == TRUE)
  results
}


#' @describeIn outliers detect outliers in a numeric vector. Returns logical vector.
#' @export
is_outlier <- function(x, coef = 1.5){
  res <- x
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  .IQR <- IQR(x, na.rm = TRUE)
  upper.limit <- Q3 + (coef*.IQR)
  lower.limit <- Q1 - (coef*.IQR)
  outlier <- ifelse(x < lower.limit | x > upper.limit, TRUE, FALSE )
  outlier
}

#' @describeIn outliers detect extreme points in a numeric vector. An alias of
#'   \code{is_outlier()}, where coef = 3. Returns logical vector.
#' @export
is_extreme <- function(x){
  is_outlier(x, coef = 3)
}

