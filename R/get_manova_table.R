#' @include utilities.R
NULL

# Helper function to get MANOVA table
# The codes is from: getAnywhere("print.Anova.mlm")
#
# x a manova test result
get_manova_table <- function (x)
{
  if ((!is.null(x$singular)) && x$singular)
    stop(
      "singular error SSP matrix; multivariate tests unavailable\n",
      "try summary(object, multivariate=FALSE)"
      )
  test <- x$test
  repeated <- x$repeated
  ntests <- length(x$terms)
  tests <- matrix(NA, ntests, 4)
  . <- NULL
  if (!repeated)
    SSPE.qr <- qr(x$SSPE)
  for (term in 1:ntests) {
    eigs <- qr.coef(
      if (repeated) qr(x$SSPE[[term]]) else SSPE.qr,
      x$SSP[[term]]
      ) %>%
      eigen(symmetric = FALSE) %>%
      .$values %>%
      Re()
    tests[term, 1:4] <- switch(
      test,
      Pillai = Pillai(eigs, x$df[term], x$error.df),
      Wilks = Wilks(eigs, x$df[term], x$error.df),
      `Hotelling-Lawley` = HL(eigs, x$df[term], x$error.df),
      Roy = Roy(eigs, x$df[term], x$error.df)
      )
  }
  ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
  ok <- !is.na(ok) & ok
  tests <- cbind(
    x$df, tests,
    stats::pf(tests[ok, 2], tests[ok, 3], tests[ok, 4], lower.tail = FALSE)
    )
  rownames(tests) <- x$terms
  colnames(tests) <- c("Df", "test stat", "approx F", "num Df",
                       "den Df", "Pr(>F)")
  heading <- paste(
    "\nType ", x$type, if (repeated) " Repeated Measures",
    " MANOVA Tests: ", test, " test statistic",sep = ""
    )

  tests <- structure(
    as.data.frame(tests), heading = heading,
    class = c("anova", "data.frame")
    )
  tests
}


Pillai <- function (eig, q, df.res)
{
  test <- sum(eig/(1 + eig))
  p <- length(eig)
  s <- min(p, q)
  n <- 0.5 * (df.res - p - 1)
  m <- 0.5 * (abs(p - q) - 1)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * n + s + 1
  c(test, (tmp2/tmp1 * test)/(s - test), s * tmp1, s * tmp2)
}

Wilks <- function (eig, q, df.res)
{
  test <- prod(1/(1 + eig))
  p <- length(eig)
  tmp1 <- df.res - 0.5 * (p - q + 1)
  tmp2 <- (p * q - 2)/4
  tmp3 <- p^2 + q^2 - 5
  tmp3 <- if (tmp3 > 0)
    sqrt(((p * q)^2 - 4)/tmp3)
  else 1
  c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q,
    p * q, tmp1 * tmp3 - 2 * tmp2)
}

HL <- function (eig, q, df.res)
{
  test <- sum(eig)
  p <- length(eig)
  m <- 0.5 * (abs(p - q) - 1)
  n <- 0.5 * (df.res - p - 1)
  s <- min(p, q)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * (s * n + 1)
  c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
}

Roy <- function (eig, q, df.res)
{
  p <- length(eig)
  test <- max(eig)
  tmp1 <- max(p, q)
  tmp2 <- df.res - tmp1 + q
  c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
}
