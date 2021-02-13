context("test-levene_test")

test_that("Levene test output is correctly formatted", {
  # Prepare the data
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  # Compute Levene's Test
  results <- df %>% levene_test(len ~ dose)
  expect_equal(results$df1, 2)
  expect_equal(results$df2, 57)
  expect_equal(round(results$statistic, 3), 0.646)
  expect_equal(round(results$p, 3), 0.528)
})
