context("test-cohens_d-analytic-ci")

# Analytic (noncentral-t) confidence-interval values for cohens_d(ci.method =
# "analytic"). Pinned from the implementation and cross-checked in development to
# match effectsize::cohens_d(ci = ) 1.x (2026-07-12) to <= 1e-5. With
# hedges.correction = TRUE the values are NOT those of effectsize::hedges_g():
# rstatix scales by its documented (N - 3)/(N - 2.25) approximation while
# effectsize applies the exact gamma-function correction; the gap is
# proportional to the effect size and grows as the samples shrink.
# effectsize is a development-time oracle only: the
# values are hard-coded here, not computed, so these tests need no Suggests
# package and run under _R_CHECK_DEPENDS_ONLY_.

test_that("analytic CI matches the noncentral-t interval, two-sample pooled", {
  res <- ToothGrowth %>% cohens_d(len ~ supp, var.equal = TRUE, ci = TRUE, ci.method = "analytic")
  expect_equal(res$conf.low,  -0.0215103983, tolerance = 1e-6)
  expect_equal(res$conf.high,  1.0064212006, tolerance = 1e-6)
})

test_that("analytic CI matches the noncentral-t interval, two-sample Welch (default)", {
  res <- ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, ci.method = "analytic")
  expect_equal(res$conf.low,  -0.0219872018, tolerance = 1e-6)
  expect_equal(res$conf.high,  1.0067037783, tolerance = 1e-6)
})

test_that("analytic CI matches the noncentral-t interval, one-sample", {
  res <- ToothGrowth %>% cohens_d(len ~ 1, mu = 20, ci = TRUE, ci.method = "analytic")
  expect_equal(res$conf.low,  -0.4090556307, tolerance = 1e-6)
  expect_equal(res$conf.high,  0.1000851191, tolerance = 1e-6)
})

test_that("analytic CI matches the noncentral-t interval, paired", {
  dp <- data.frame(id = 1:5, pre = c(110,122,101,120,140), post = c(150,160,110,140,155)) %>%
    tidyr::gather("treatment", "value", -id)
  res <- dp %>% cohens_d(value ~ treatment, paired = TRUE, ci = TRUE, ci.method = "analytic")
  expect_equal(res$conf.low, 0.2645997902, tolerance = 1e-6)
  expect_equal(res$conf.high, 3.1862270644, tolerance = 1e-6)
})

test_that("analytic CI is scaled by the Hedges factor when hedges.correction = TRUE", {
  res <- ToothGrowth %>%
    cohens_d(len ~ supp, var.equal = TRUE, hedges.correction = TRUE, ci = TRUE, ci.method = "analytic")
  expect_equal(res$conf.low, -0.0212310425, tolerance = 1e-6)
  expect_equal(res$conf.high, 0.9933507954, tolerance = 1e-6)
})

test_that("the analytic interval is deterministic (no seed dependence)", {
  a <- ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, ci.method = "analytic")
  b <- ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, ci.method = "analytic")
  expect_identical(a$conf.low, b$conf.low)
  expect_identical(a$conf.high, b$conf.high)
})

test_that("ci.method is stashed in args only when non-default (byte-safe)", {
  expect_null(attr(ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE), "args")$ci.method)
  expect_null(attr(ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, ci.method = "boot"), "args")$ci.method)
  expect_equal(
    attr(ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, ci.method = "analytic"), "args")$ci.method,
    "analytic"
  )
})

test_that("analytic ci.method works for grouped and pairwise designs", {
  tg <- ToothGrowth %>% dplyr::mutate(dose = factor(dose))
  g <- tg %>% dplyr::group_by(supp) %>% cohens_d(len ~ dose, ci = TRUE, ci.method = "analytic")
  expect_true(all(c("conf.low", "conf.high") %in% colnames(g)))
  expect_true(all(is.finite(g$conf.low)))
  p <- tg %>% cohens_d(len ~ dose, ci = TRUE, ci.method = "analytic")
  expect_equal(nrow(p), 3L)
  expect_true(all(is.finite(c(p$conf.low, p$conf.high))))
})

test_that("the default ci.method = 'boot' still returns a bootstrap interval", {
  skip_if_not_installed("boot")
  set.seed(1)
  res <- ToothGrowth %>% cohens_d(len ~ supp, ci = TRUE, nboot = 50)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
  expect_true(res$conf.low <= res$effsize && res$effsize <= res$conf.high)
})
