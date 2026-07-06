context("test-bootstrap_ci")

test_that("get_boot_ci forwards parallel arguments to boot", {
  skip_if_not_installed("boot")
  stat_func <- function(data, i) mean(data$x[i])

  expect_error(
    get_boot_ci(
      data.frame(x = 1:5), stat_func,
      nboot = 10, parallel = "bad", ncpus = 1
    ),
    "should be one of"
  )
})
