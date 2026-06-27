context("test-get_summary_stats")

test_that("Checking that get_summary_stats keeps the order of columns specified by the user", {
  res <- data.frame(
    c = rnorm(50,10,5),
    b = rnorm(50,100,20),
    a = rnorm(50,0,1)
    ) %>%
    get_summary_stats(a, c, b, type = "mean_sd")
  obtained_var_order <- as.character(res$variable)
  expected_var_order <- c("a", "c", "b")
  expect_equal(obtained_var_order, expected_var_order)
})

test_that("get_summary_stats `digits` controls the rounding precision (#145, #186, #218)", {
  small <- data.frame(x = c(0.0001234, 0.0002, 0.00033, 0.00041))
  # default (digits = 3) rounds very small values to 0
  expect_equal(get_summary_stats(small, x, type = "mean")$mean, 0)
  # a higher `digits` keeps the precision
  expect_gt(get_summary_stats(small, x, type = "mean", digits = 8)$mean, 0)
})

test_that("get_summary_stats default (digits = 3) output is unchanged (no regression, #145)", {
  for (ty in c("full", "common", "mean_sd", "quantile", "robust")) {
    expect_equal(
      ToothGrowth %>% get_summary_stats(len, type = ty),
      ToothGrowth %>% get_summary_stats(len, type = ty, digits = 3)
    )
  }
  # grouped: default unchanged, and `digits` is honoured per group
  expect_equal(
    ToothGrowth %>% group_by(supp) %>% get_summary_stats(len),
    ToothGrowth %>% group_by(supp) %>% get_summary_stats(len, digits = 3)
  )
  g8 <- ToothGrowth %>% group_by(supp) %>% get_summary_stats(len, type = "mean", digits = 8)
  expect_equal(nrow(g8), 2L)
})
