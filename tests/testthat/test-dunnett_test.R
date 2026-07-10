context("test-dunnett_test")

test_that("dunnett_test compares each group to the control (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose)
  expect_equal(nrow(res), 2L)                       # k - 1 comparisons
  expect_true(all(res$group1 == "0.5"))             # control = first level (group1)
  expect_setequal(res$group2, c("1", "2"))          # treatments
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "df", "p.adj", "p.adj.signif")
  )
})

test_that("dunnett_test matches emmeans trt.vs.ctrl with mvt adjustment (#129)", {
  skip_if_not_installed("emmeans")
  df <- ToothGrowth; df$dose <- factor(df$dose)
  emm <- emmeans::emmeans(stats::lm(len ~ dose, df), ~dose)
  ref <- as.data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl", ref = 1, adjust = "mvt"))
  res <- ToothGrowth %>% dunnett_test(len ~ dose, detailed = TRUE)
  # rstatix orients control as group1 (estimate = control - treatment), so the
  # estimate/statistic are the negation of emmeans' trt.vs.ctrl; p.adj is identical.
  expect_equal(res$estimate, -ref$estimate, tolerance = 1e-6)
  expect_equal(res$statistic, -ref$t.ratio, tolerance = 1e-6)
  expect_equal(res$p.adj, ref$p.value, tolerance = 1e-6)
})

test_that("dunnett_test detailed returns estimate and simultaneous CI (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose, detailed = TRUE)
  expect_true(all(c("estimate", "conf.low", "conf.high", "se", "method") %in% colnames(res)))
  expect_true(all(res$estimate < 0))                # control - treatment (doses raise length)
  expect_equal(unique(res$method), "Dunnett")
})

test_that("dunnett_test respects ref.group (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose, ref.group = "2")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "2"))               # control = "2" (group1)
  expect_setequal(res$group2, c("0.5", "1"))
})

test_that("dunnett_test works on grouped data (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% dunnett_test(len ~ dose)
  expect_true("supp" %in% colnames(res))
  expect_equal(nrow(res), 4L)                        # 2 supp x 2 comparisons
})

test_that("get_description gives a friendly dunnett_test label (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose)
  expect_equal(get_description(res), "Dunnett test")   # not the raw "dunnett_test"
})

test_that("dunnett_test gives an informative error for an invalid ref.group (#129)", {
  skip_if_not_installed("emmeans")
  expect_error(
    ToothGrowth %>% dunnett_test(len ~ dose, ref.group = "9"),
    "not a level of the grouping variable"
  )
})

test_that("dunnett_test works with exactly two groups (k = 2) (#129)", {
  skip_if_not_installed("emmeans")
  set.seed(1)
  df <- data.frame(y = c(rnorm(12), rnorm(12) + 1), g = rep(c("ctrl", "trt"), each = 12))
  res <- df %>% dunnett_test(y ~ g, ref.group = "ctrl", detailed = TRUE)
  expect_equal(nrow(res), 1L)                        # single treatment vs control
  expect_equal(res$group1, "ctrl")                   # control = group1
  expect_equal(res$group2, "trt")
  expect_false(is.na(res$p.adj))
  # with one comparison the Dunnett p equals the plain emmeans p
  emm <- emmeans::emmeans(stats::lm(y ~ g, df), ~g)
  ref <- as.data.frame(emmeans::contrast(emm, method = list(c(-1, 1))))
  expect_equal(res$p.adj, ref$p.value, tolerance = 1e-6)
})

test_that("dunnett_test handles factor levels containing '-' (#129)", {
  skip_if_not_installed("emmeans")
  set.seed(1)
  df <- data.frame(
    y = c(rnorm(10), rnorm(10) + 1, rnorm(10) + 2),
    g = rep(c("ctrl", "low-dose", "high-dose"), each = 10)
  )
  res <- df %>% dunnett_test(y ~ g, ref.group = "ctrl")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "ctrl"))            # control label intact (group1)
  expect_setequal(res$group2, c("low-dose", "high-dose"))  # dashed labels intact
  expect_false(any(is.na(res$n2)))                  # n lookups succeed
  expect_equal(unique(res$n2), 10L)
})

test_that("dunnett_test matches the DescTools and multcomp Dunnett implementations", {
  skip_if_not_installed("emmeans")
  # The documentation of dunnett_test() claims its results match
  # DescTools::DunnettTest() and multcomp::glht(..., mcp(dose = "Dunnett")).
  # Neither package is a dependency, and calling one from a test is an
  # unstated-dependency WARNING under --as-cran, so their output is recorded here
  # as fixed numbers. Pinned snapshot: DescTools 0.99.60, multcomp 1.4.30,
  # 2026-07-10. A recorded number cannot notice either package changing its
  # algorithm; re-verify when refreshing the snapshot.
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  res <- dunnett_test(df, len ~ dose)

  # multcomp::glht() reports the control-minus-treatment contrast with the
  # opposite sign; the t statistics are otherwise identical to machine precision.
  glht_tstat <- c(6.80584650754, 11.5505576817)
  expect_equal(res$statistic, -glht_tstat, tolerance = 1e-10)

  # p-values from the multivariate-t distribution. These are of order 1e-8, and
  # `expect_equal(tolerance = )` compares numbers this small on an ABSOLUTE
  # scale, so a pin written that way would accept anything below roughly 2e-8 --
  # it would read like nine-digit agreement while allowing a 50% error. Assert
  # the relative error instead.
  #
  # The probability is obtained by numerical integration (mvtnorm), so the last
  # few digits depend on the mvtnorm build: this machine reproduces the multcomp
  # value to 1e-12, other platforms only to about 1e-8. 1e-5 keeps two orders of
  # margin over that while still failing a 0.24% error in p.adj.
  # multcomp reports 1.3367834395e-08 and DescTools 1.33678340619e-08; they agree
  # with each other to 2.5e-8 relative, well inside the tolerance, so one
  # assertion covers both.
  rel_error <- function(observed, reference) abs(observed - reference) / reference
  expect_lt(rel_error(res$p.adj[1], 1.336783e-08), 1e-5)

  # The second comparison lies at the floating-point floor: rstatix and multcomp
  # both report 3.33e-16, DescTools 2.22e-16. All three mean "indistinguishable
  # from zero", so only the magnitude is worth asserting.
  expect_lt(res$p.adj[2], 1e-15)
})
