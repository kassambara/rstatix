context("test-wilcox-test")

test_that("Checking one-sample test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ 1, mu = 0)
  expect_equal(res$group1, "1")
  expect_equal(res$group2, "null model")
  expect_equal(res$n, 60)
  expect_equal(as.numeric(res$statistic), 1830)
  expect_equal(signif(res$p, 3), 1.66e-11)
})


test_that("Checking two-sample unpaired test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ supp)
  expect_equal(res$group1, "OJ")
  expect_equal(res$group2, "VC")
  expect_equal(res$n1, 30)
  expect_equal(res$n2, 30)
  expect_equal(as.numeric(res$statistic), 575.5)
  expect_equal(signif(res$p, 3), 0.0645)
})


test_that("Checking two-sample paired test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ supp, paired = TRUE)
  expect_equal(res$group1, "OJ")
  expect_equal(res$group2, "VC")
  expect_equal(res$n1, 30)
  expect_equal(res$n2, 30)
  expect_equal(as.numeric(res$statistic), 350)
  expect_equal(signif(res$p, 3), 0.00431)
})

test_that("Checking pairwise comparisons", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose)
  expect_equal(res$group1, c("0.5", "0.5", "1"))
  expect_equal(res$group2, c("1", "2", "2"))
  expect_equal(res$n1, c(20, 20, 20))
  expect_equal(res$n2, c(20, 20, 20))
  expect_equal(as.numeric(res$statistic), c(33.5, 1.5, 61.0))
  expect_equal(signif(res$p, 3), c(7.02e-6, 8.41e-08, 1.77e-04))
})

test_that("Checking pairwise comparison against ref group", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose, ref.group = "0.5")
  expect_equal(res$group1, c("0.5", "0.5"))
  expect_equal(res$group2, c("1", "2"))
  expect_equal(res$n1, c(20, 20))
  expect_equal(res$n2, c(20, 20))
  expect_equal(as.numeric(res$statistic), c(33.5, 1.5))
  expect_equal(signif(res$p, 3), c(7.02e-6, 8.41e-08))
})


test_that("Checking pairwise comparisons against all", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose, ref.group = "all")
  expect_equal(res$group1, c("all", "all", "all"))
  expect_equal(res$group2, c("0.5", "1", "2"))
  expect_equal(res$n1, c(60, 60, 60))
  expect_equal(res$n2, c(20, 20, 20))
  expect_equal(as.numeric(res$statistic), c(965.0, 572.5, 262.5))
  expect_equal(signif(res$p, 3), c(0.0000508, 0.764, 0.000179))
})


test_that("Checking grouped tests", {
  data("ToothGrowth")
  res <- ToothGrowth %>%
    group_by(dose) %>%
    wilcox_test(len ~ supp)
  expect_equal(res$group1, c("OJ", "OJ", "OJ"))
  expect_equal(res$group2, c("VC", "VC", "VC"))
  expect_equal(res$n1, c(10, 10, 10))
  expect_equal(res$n2, c(10, 10, 10))
  expect_equal(as.numeric(res$statistic), c(80.5, 88.5, 49.5))
  expect_equal(signif(res$p, 3), c(0.0232, 0.00403, 1))
})


test_that("Empty values are not counting in group n size (104)", {
  # Data without NA
  df <- data.frame(
    g = rep(c("a", "b"), each = 10),
    v = rnorm(20)
  )

  # run Wilcoxon test --> sample sizes are correct
  res <- wilcox_test(df, v ~ g, paired = TRUE)
  expect_equal(c(res$n1, c(res$n1)), c(10, 10))

  # Insert NAs
  df$v[c(1, 12:14)] <- NA

  #repeat Wilcox test --> sample sizes are still the same
  res <- wilcox_test(data = df, v ~ g, paired = TRUE)
  expect_equal(c(res$n1, res$n2), c(9, 7))
})
