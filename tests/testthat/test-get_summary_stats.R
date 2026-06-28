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

test_that("get_summary_stats: skewness/kurtosis values are correct (#99)", {
  x <- ToothGrowth$len
  n <- length(x); m <- mean(x)
  m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m4 <- mean((x - m)^4)
  exp.skew <- (m3 / m2^1.5) * sqrt(n * (n - 1)) / (n - 2)
  exp.kurt <- ((n + 1) * (m4 / m2^2 - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
  res <- ToothGrowth %>% get_summary_stats(len, show = c("skewness", "kurtosis"), digits = 8)
  # values match the type-2 (bias-corrected) estimator (validated against e1071 type 2)
  expect_equal(res$skewness, round(exp.skew, 8))
  expect_equal(res$kurtosis, round(exp.kurt, 8))
})

test_that("get_summary_stats: skewness/kurtosis are opt-in via show, default unchanged (#99)", {
  default.cols <- colnames(ToothGrowth %>% get_summary_stats(len))
  expect_false(any(c("skewness", "kurtosis") %in% default.cols))
  # show controls inclusion + order
  res <- ToothGrowth %>% get_summary_stats(len, show = c("mean", "sd", "skewness", "kurtosis"))
  expect_equal(colnames(res), c("variable", "n", "mean", "sd", "skewness", "kurtosis"))
  # works on a non-full type too
  res2 <- ToothGrowth %>% get_summary_stats(len, type = "mean_sd", show = c("mean", "skewness"))
  expect_equal(colnames(res2), c("variable", "n", "mean", "skewness"))
})

test_that("get_summary_stats: skewness/kurtosis per group + NA edge cases (#99)", {
  g <- ToothGrowth %>% group_by(supp) %>%
    get_summary_stats(len, show = c("skewness", "kurtosis"), digits = 8)
  expect_equal(nrow(g), 2L)
  oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
  no <- length(oj); mo <- mean(oj); mo2 <- mean((oj - mo)^2); mo3 <- mean((oj - mo)^3)
  exp.oj <- (mo3 / mo2^1.5) * sqrt(no * (no - 1)) / (no - 2)
  expect_equal(g$skewness[g$supp == "OJ"], round(exp.oj, 8))
  # too-few-observations -> NA, no error
  small <- data.frame(x = c(1, 2, 3))  # n=3: skewness defined, kurtosis NA
  res <- small %>% get_summary_stats(x, show = c("skewness", "kurtosis"))
  expect_false(is.na(res$skewness))
  expect_true(is.na(res$kurtosis))
})
