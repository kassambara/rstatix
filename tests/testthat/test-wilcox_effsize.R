context("test-wilcox_effsize")

test_that("wilcox_effsize detailed exposes the Z statistic (#122)", {
  skip_if_not_installed("coin")
  res <- ToothGrowth %>% wilcox_effsize(len ~ supp, paired = TRUE, detailed = TRUE)
  expect_true(all(c("effsize", "statistic", "p") %in% colnames(res)))
  # statistic is the Z used to compute r = |Z| / sqrt(N), N = number of pairs (30)
  expect_equal(unname(abs(res$statistic) / sqrt(30)), unname(res$effsize), tolerance = 1e-7)
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
