context("Testing kruskal_test and kruskal_effsize missing value handling")

test_that("kruskal_test reports correct sample size 'n' when handling missing values", {  
  set.seed(123)
  test_df <- data.frame(
    group = factor(rep(c(NA, "A", "B", "C"), each = 10)),
    value = c(rnorm(35), rep(NA, 5))
  )
  res_default <- kruskal_test(test_df, value ~ group)
  expect_equal(res_default$n, 25)
  res_explicit <- kruskal_test(test_df, value ~ group, na.action = stats::na.omit)
  expect_equal(res_explicit$n, 25)
  pristine_df <- na.omit(test_df)
  res_pristine <- kruskal_test(pristine_df, value ~ group)
  expect_equal(res_pristine$n, 25)
})