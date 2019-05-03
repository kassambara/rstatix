#' @include utilities.R
NULL
#'Compute Mahalanobis Distance and Flag Multivariate Outliers
#'
#'@description Pipe-friendly wrapper around to the function
#'  \code{\link[stats]{mahalanobis}()}, which returns the squared
#'  Mahalanobis distance of all rows in x. Compared to the base function, it
#'  automatically flags multivariate outliers.
#'
#'  Mahalanobis distance is a common metric used to identify multivariate
#'  outliers. The larger the value of Mahalanobis distance, the more unusual the
#'  data point (i.e., the more likely it is to be a multivariate outlier).
#'
#'  The distance tells us how far an observation is from the center of the cloud, taking into
#'  account the shape (covariance) of the cloud as well.
#'
#'  To detect outliers, the calculated Mahalanobis distance is compared against
#'  a chi-square (X^2) distribution with degrees of freedom equal to the number
#'  of dependent (outcome) variables and an alpha level of 0.001.
#'
#'  The threshold to declare a multivariate outlier is determined using the
#'  function \code{qchisq(0.999, df) }, where df is the degree of freedom (i.e.,
#'  the number of dependent variable used in the computation).
#'
#'@param data a data frame. Columns are variables.
#'@param ... One unquoted expressions (or variable name). Used to select a
#'  variable of interest. Can be also used to ignore a variable that are not
#'  needed for the computation. For example specify \code{-id} to ignore the id
#'  column.
#'
#'@return  Returns the input data frame with two additional columns: 1)
#'  "mahal.dist": Mahalanobis distance values; and 2) "is.outlier": logical
#'  values specifying whether a given observation is a multivariate outlier
#'
#' @examples
#'
#' # Compute mahalonobis distance and flag outliers if any
#' iris %>%
#'   doo(~mahalanobis_distance(.))
#'
#'# Compute distance by groups and filter outliers
#' iris %>%
#'  group_by(Species) %>%
#'  doo(~mahalanobis_distance(.)) %>%
#'  filter(is.outlier == TRUE)
#'
#'@export
mahalanobis_distance <- function(data, ...){
  if(is_grouped_df(data)){
    results <- data %>%
      doo(~mahalanobis_distance(.))
  }
  data <- data %>% select_numeric_columns()
  vars <- data %>% get_selected_vars(...)
  n.vars <- length(vars)
  threshold <- stats::qchisq(0.999, n.vars)
  .data <- data %>%
    select(!!!syms(vars)) %>%
    as.matrix()
  distance <- stats::mahalanobis(
    .data,
    center = colMeans(.data),
    cov = cov(.data)
  )
  results <- data %>%
    mutate(
      mahal.dist = round(distance, 3),
      is.outlier = .data$mahal.dist > threshold
      )
  results
}
