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
