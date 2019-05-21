#' @include utilities.R
#'
NULL
#'Compute Cramer's V
#'@description Compute Cramer's V, which measures the strength of the
#'  association between categorical variables.
#'@inheritParams stats::chisq.test
#'@param ... other arguments passed to the function
#'  \code{\link[stats]{chisq.test}()}.
#'@examples
#'
#' # Data preparation
#' df <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(df) <- list(
#'   gender = c("F", "M"),
#'   party = c("Democrat","Independent", "Republican")
#' )
#' df
#' # Compute cramer's V
#' cramer_v(df)
#'
#'@export
cramer_v <- function(x, y = NULL, correct = TRUE, ...) {
  test <- stats::chisq.test(x, y, correct = correct, ...)
  chi2 <- test$statistic
  N <- sum(test$observed)
  k <- min(dim(test$observed))
  V <- sqrt(chi2/(N * (k - 1)))
  as.numeric(V)
}
