context("test-dunnett_test")

test_that("dunnett_test compares each group to the control (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose)
  expect_equal(nrow(res), 2L)                       # k - 1 comparisons
  expect_true(all(res$group2 == "0.5"))             # control = first level (group2)
  expect_setequal(res$group1, c("1", "2"))          # treatments
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
  # estimates and adjusted p-values match emmeans (order: 1-0.5, 2-0.5)
  expect_equal(res$estimate, ref$estimate, tolerance = 1e-6)
  expect_equal(res$p.adj, ref$p.value, tolerance = 1e-6)
  expect_equal(res$statistic, ref$t.ratio, tolerance = 1e-6)
})

test_that("dunnett_test detailed returns estimate and simultaneous CI (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose, detailed = TRUE)
  expect_true(all(c("estimate", "conf.low", "conf.high", "se", "method") %in% colnames(res)))
  expect_true(all(res$estimate > 0))                # both doses increase length vs 0.5
  expect_equal(unique(res$method), "Dunnett")
})

test_that("dunnett_test respects ref.group (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dunnett_test(len ~ dose, ref.group = "2")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group2 == "2"))               # control = "2"
  expect_setequal(res$group1, c("0.5", "1"))
})

test_that("dunnett_test works on grouped data (#129)", {
  skip_if_not_installed("emmeans")
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% dunnett_test(len ~ dose)
  expect_true("supp" %in% colnames(res))
  expect_equal(nrow(res), 4L)                        # 2 supp x 2 comparisons
})

test_that("dunnett_test gives an informative error for an invalid ref.group (#129)", {
  skip_if_not_installed("emmeans")
  expect_error(
    ToothGrowth %>% dunnett_test(len ~ dose, ref.group = "9"),
    "not a level of the grouping variable"
  )
})

test_that("dunnett_test matches DescTools::DunnettTest when available (#129)", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("DescTools")
  df <- ToothGrowth; df$dose <- factor(df$dose)
  dt <- DescTools::DunnettTest(df$len, df$dose)[["0.5"]]
  res <- ToothGrowth %>% dunnett_test(len ~ dose, detailed = TRUE)
  expect_equal(res$estimate, unname(dt[, "diff"]), tolerance = 1e-5)
  expect_equal(res$p.adj, unname(dt[, "pval"]), tolerance = 1e-5)
})
