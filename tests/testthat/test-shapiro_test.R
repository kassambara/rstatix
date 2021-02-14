context("test-shapiro_test")

test_that("Shapiro test works when input data contains column names 'value' or 'variable'", {
  df <- iris
  # Column names contain value
  colnames(df)[2] <- "value"
  res <- df %>% shapiro_test(value)
  res_value <- round(res$p, 3)
  # Column names contain variable
  colnames(df)[2] <- "variable"
  res <- df %>% shapiro_test(variable)
  res_variable <- round(res$p, 3)
  expect_equal(res_value, 0.101)
  expect_equal(res_variable, 0.101)
})
