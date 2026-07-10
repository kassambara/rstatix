context("test-sign_test")

# Ten measurements, one of which (185) equals the mu used below. The sign test
# drops that tie, so the binomial is taken over 9 observations while `n` still
# reports 10. Kept self-contained: no bundled data sets in rstatix.
paired_data <- function() {
  data.frame(
    id     = 1:10,
    before = c(200, 165, 180, 190, 175, 210, 195, 185, 170, 205),
    after  = c(190, 170, 175, 180, 178, 200, 185, 180, 168, 195)
  )
}

test_that("sign_test returns the documented tidy columns", {
  res <- sign_test(paired_data(), after ~ 1, mu = 185)
  expect_s3_class(res, "rstatix_test")
  expect_s3_class(res, "sign_test")
  expect_equal(nrow(res), 1L)
  expect_true(all(c(".y.", "group1", "group2", "n", "statistic", "df", "p") %in% colnames(res)))
  expect_equal(res$n, 10L)                  # all observations
  expect_equal(as.numeric(res$df), 9)       # minus the one tied with mu
})

test_that("sign_test matches the DescTools sign test", {
  # The documentation of sign_test() states that its results match
  # DescTools::SignTest(). DescTools is not a dependency, and calling it from a
  # test is an unstated-dependency WARNING under --as-cran, so its output is
  # recorded here as fixed numbers. Pinned snapshot: DescTools 0.99.60,
  # 2026-07-10. A recorded number cannot notice DescTools changing its
  # algorithm; re-verify when refreshing the snapshot.
  res <- sign_test(paired_data(), after ~ 1, mu = 185)

  # DescTools::SignTest(x, mu = 185) reports S = 3, p = 0.5078125.
  expect_equal(as.numeric(res$statistic), 3)
  expect_equal(as.numeric(res$p), 0.5078125, tolerance = 1e-9)

  # The statistic is the number of observations above mu, and the p-value is the
  # exact two-sided binomial probability over the non-tied observations -- both
  # computable in base R, which is what makes the agreement checkable without the
  # package. Ties with mu are excluded, which is why the denominator is 9, not 10.
  x <- paired_data()$after
  above <- sum(x > 185)
  untied <- sum(x != 185)
  expect_equal(untied, 9L)
  expect_equal(as.numeric(res$statistic), above)
  expect_equal(as.numeric(res$p),
               stats::binom.test(above, untied, p = 0.5)$p.value,
               tolerance = 1e-12)
})
