context("test-wilcox_effsize")

test_that("wilcox_effsize detailed exposes the Z statistic (#122)", {
  skip_if_not_installed("coin")
  res <- ToothGrowth %>% wilcox_effsize(len ~ supp, paired = TRUE, detailed = TRUE)
  expect_true(all(c("effsize", "statistic", "p") %in% colnames(res)))
  # statistic is the Z used to compute r = |Z| / sqrt(N), N = number of pairs (30)
  expect_equal(unname(abs(res$statistic) / sqrt(30)), unname(res$effsize), tolerance = 1e-7)
  # no spurious 'df' column (Wilcoxon has no degrees of freedom) (#122 review)
  expect_false("df" %in% colnames(res))
  # method correctly labels the paired case as signed-rank (#122 review)
  expect_equal(unique(res$method), "Wilcoxon signed rank test")
})

test_that("wilcox_effsize detailed labels the method correctly per case (#122)", {
  skip_if_not_installed("coin")
  expect_equal(unique((ToothGrowth %>% wilcox_effsize(len ~ supp, detailed = TRUE))$method),
               "Wilcoxon rank sum test")                                   # independent
  expect_equal(unique((ToothGrowth %>% wilcox_effsize(len ~ 1, mu = 20, detailed = TRUE))$method),
               "Wilcoxon signed rank test")                               # one-sample
})

test_that("wilcox_effsize detailed works for the independent and pairwise cases (#122)", {
  skip_if_not_installed("coin")
  ind <- ToothGrowth %>% wilcox_effsize(len ~ supp, detailed = TRUE)
  expect_true("statistic" %in% colnames(ind))
  pw <- ToothGrowth %>% wilcox_effsize(len ~ dose, detailed = TRUE)
  expect_equal(nrow(pw), 3L)
  expect_true(all(c("statistic", "p", "p.adj") %in% colnames(pw)))
})

test_that("wilcox_effsize default output is unchanged (#122 no-regression)", {
  skip_if_not_installed("coin")
  res <- ToothGrowth %>% wilcox_effsize(len ~ supp, paired = TRUE)
  # default (detailed = FALSE) must not gain the Z/p columns
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "effsize", "n1", "n2", "magnitude")
  )
  expect_false("statistic" %in% colnames(res))
})
