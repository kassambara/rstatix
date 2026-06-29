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

# Note: correctness is validated above against emmeans' own trt.vs.ctrl + mvt
# contrast (a declared Suggests). DescTools / multcomp would be unstated test
# dependencies, so they are intentionally not used here (cf. R CMD check
# "unstated dependencies in tests").
