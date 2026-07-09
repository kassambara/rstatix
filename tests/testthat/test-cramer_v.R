context("test-cramer_v")

gender_party <- function() {
  x <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
  dimnames(x) <- list(gender = c("F", "M"),
                      party = c("Democrat", "Independent", "Republican"))
  x
}
tab_2x2 <- function() as.table(rbind(c(20, 30), c(35, 15)))
tab_3x4 <- function() as.table(rbind(c(10, 20, 30, 15), c(25, 15, 10, 20), c(5, 25, 20, 10)))

test_that("cramer_v returns the same bare numeric as before (no regression)", {
  # The default output must stay a single, unnamed numeric value.
  v <- cramer_v(gender_party())
  expect_type(v, "double")
  expect_length(v, 1)
  expect_null(names(v))
  expect_equal(v, 0.1044358024, tolerance = 1e-7)

  expect_equal(cramer_v(tab_3x4()), 0.2726207805, tolerance = 1e-7)

  # Yates' continuity correction applies to 2x2 tables and is ON by default
  expect_equal(cramer_v(tab_2x2()), 0.2814105883, tolerance = 1e-7)
  expect_equal(cramer_v(tab_2x2(), correct = FALSE), 0.3015113446, tolerance = 1e-7)
})

test_that("cramer_v still accepts positional and abbreviated arguments (no regression)", {
  # ci/conf.level sit after `...`, so they take no positional slot and cannot be
  # reached by partial matching; `c=`/`corr=` must still resolve to `correct`.
  expect_equal(cramer_v(gender_party(), NULL, FALSE), 0.1044358024, tolerance = 1e-7)
  expect_equal(cramer_v(gender_party(), corr = FALSE), 0.1044358024, tolerance = 1e-7)
  expect_equal(cramer_v(gender_party(), c = FALSE), 0.1044358024, tolerance = 1e-7)
})

test_that("cramer_v(ci = TRUE) returns the effect size with a confidence interval", {
  res <- cramer_v(gender_party(), ci = TRUE)
  expect_type(res, "double")
  expect_length(res, 3)
  expect_named(res, c("effsize", "conf.low", "conf.high"))
  # the interval brackets its own point estimate
  expect_lte(res[["conf.low"]], res[["effsize"]])
  expect_gte(res[["conf.high"]], res[["effsize"]])
})

test_that("the cramer_v confidence interval matches the noncentral chi-square reference", {
  # Reference values from effectsize::cramers_v(adjust = FALSE, ci = 0.95,
  # alternative = "two.sided"), which uses the same noncentral chi-square
  # inversion. Hard-coded on purpose: effectsize is not a dependency.
  res <- cramer_v(gender_party(), correct = FALSE, ci = TRUE)
  expect_equal(res[["conf.low"]], 0.06490742, tolerance = 1e-5)
  expect_equal(res[["conf.high"]], 0.14026390, tolerance = 1e-5)

  res34 <- cramer_v(tab_3x4(), correct = FALSE, ci = TRUE)
  expect_equal(res34[["conf.low"]], 0.14555000, tolerance = 1e-5)
  expect_equal(res34[["conf.high"]], 0.34962200, tolerance = 1e-5)

  res22 <- cramer_v(tab_2x2(), correct = FALSE, ci = TRUE)
  expect_equal(res22[["conf.low"]], 0.10547470, tolerance = 1e-5)
  expect_equal(res22[["conf.high"]], 0.49750770, tolerance = 1e-5)
})

test_that("the cramer_v interval is computed from the same statistic as the estimate", {
  # Yates' correction shrinks the 2x2 chi-square, so it must shift the interval
  # too; otherwise the interval would not bracket the reported effect size.
  corrected <- cramer_v(tab_2x2(), ci = TRUE)
  uncorrected <- cramer_v(tab_2x2(), correct = FALSE, ci = TRUE)
  expect_lt(corrected[["effsize"]], uncorrected[["effsize"]])
  expect_lt(corrected[["conf.low"]], uncorrected[["conf.low"]])
  expect_lte(corrected[["conf.low"]], corrected[["effsize"]])
  expect_gte(corrected[["conf.high"]], corrected[["effsize"]])
})

test_that("the cramer_v interval is clipped to the [0, 1] range of the statistic", {
  # A near-perfect association pushes the inverted upper bound past 1.
  perfect_2x2 <- as.table(rbind(c(50, 0), c(0, 50)))
  res <- suppressWarnings(cramer_v(perfect_2x2, correct = FALSE, ci = TRUE))
  expect_equal(res[["effsize"]], 1)
  expect_equal(res[["conf.high"]], 1)
  expect_lte(res[["conf.high"]], 1)
  expect_gte(res[["conf.low"]], 0)

  perfect_3x3 <- as.table(diag(c(20, 20, 20)))
  res3 <- suppressWarnings(cramer_v(perfect_3x3, correct = FALSE, ci = TRUE))
  expect_equal(res3[["conf.high"]], 1)

  # exact independence: the statistic is 0 and so are both bounds
  independent <- as.table(rbind(c(50, 50), c(50, 50)))
  res0 <- suppressWarnings(cramer_v(independent, correct = FALSE, ci = TRUE))
  expect_equal(unname(res0), c(0, 0, 0))
})

test_that("the cramer_v interval is NA when the statistic is undefined", {
  # An empty row makes the chi-square statistic NaN; the interval must be NA
  # rather than a fabricated bound.
  empty_row <- as.table(rbind(c(0, 0), c(30, 20)))
  res <- suppressWarnings(cramer_v(empty_row, correct = FALSE, ci = TRUE))
  expect_true(is.na(res[["conf.low"]]))
  expect_true(is.na(res[["conf.high"]]))
  expect_type(res, "double")
})

test_that("cramer_v validates conf.level", {
  expect_error(cramer_v(gender_party(), ci = TRUE, conf.level = 1.5), "conf.level")
  expect_error(cramer_v(gender_party(), ci = TRUE, conf.level = 0), "conf.level")
  expect_error(cramer_v(gender_party(), ci = TRUE, conf.level = c(0.9, 0.95)), "conf.level")

  wide <- cramer_v(gender_party(), ci = TRUE, conf.level = 0.99)
  narrow <- cramer_v(gender_party(), ci = TRUE, conf.level = 0.95)
  expect_lt(wide[["conf.low"]], narrow[["conf.low"]])
  expect_gt(wide[["conf.high"]], narrow[["conf.high"]])
})
