#' @include utilities.R
#' @importFrom  stats cov
#' @importFrom stats pchisq
NULL
#' Box's M-test for Homogeneity of Covariance Matrices
#' @description Performs the Box's M-test for homogeneity of covariance matrices
#'   obtained from multivariate normal data according to one grouping variable.
#'   The test is based on the chi-square approximation.
#' @param data a numeric data.frame or matrix containing n observations of p
#'   variables; it is expected that n > p.
#' @param group a vector of length n containing the class of each
#'   observation; it is usually a factor.
#' @return A data frame containing the following components:
#' \itemize{
#' \item{statistic }{an approximated value of the chi-square distribution.}
#' \item{parameter }{the degrees of freedom related of the test statistic in this case that it follows a Chi-square distribution.}
#' \item{p.value }{the p-value of the test.}
#' \item{method }{the character string "Box's M-test for Homogeneity of Covariance Matrices".}
#' }
#' @examples
#' data(iris)
#' box_m(iris[, -5], iris[, 5])
#' @export
box_m <-function(data, group)
  {
    if (!inherits(data, c("data.frame", "matrix")))
      stop("'data' must be a numeric data.frame or matrix!")
    if (length(group) != nrow(data))
      stop("incompatible dimensions!")
    dname <- deparse(substitute(data))
    data <- as.matrix(data)
    group <- as.factor(as.character(group))
    p <- ncol(data)
    nlev <- nlevels(group)
    lev <- levels(group)
    dfs <- tapply(group, group, length) - 1
    if (any(dfs < p))
      warning("there are one or more levels with less observations than variables!")
    mats <- aux <- list()
    for(i in 1:nlev) {
      mats[[i]] <- cov(data[group == lev[i], , drop = FALSE])
      aux[[i]] <- mats[[i]] * dfs[i]
    }
    names(mats) <- lev
    pooled <- Reduce("+", aux) / sum(dfs)
    logdet <- log(unlist(lapply(mats, det)))
    minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet * dfs)
    sum1 <- sum(1 / dfs)
    Co <- (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) *
                                          (nlev - 1))) * (sum1 - (1 / sum(dfs)))
    X2 <- minus2logM * (1 - Co)
    dfchi <- (choose(p, 2) + p) * (nlev - 1)
    pval <- pchisq(X2, dfchi, lower.tail = FALSE)
    out <- structure(
      list(statistic = c("Chi-Sq (approx.)" = X2),
           parameter = c(df = dfchi),
           p.value = pval,
           cov = mats, pooled = pooled, logDet = logdet,
           data.name = dname,
           method = "Box's M-test for Homogeneity of Covariance Matrices"
      ),
      class = c("htest", "boxM")
    )
    out <- broom::tidy(out)
    return(out)
  }
