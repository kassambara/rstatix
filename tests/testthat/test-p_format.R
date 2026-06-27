context("test-p_format")

test_that("p_format preserves scientific-notation exponents (#112)", {
  # the trailing 0 of "e-10" was wrongly stripped, giving "5.1e-1"
  expect_equal(p_format(5.1e-10, accuracy = 1e-15), "5.1e-10")
  expect_equal(p_format(5.1e-11, accuracy = 1e-15), "5.1e-11")
  expect_equal(p_format(1e-20, accuracy = 1e-30), "1e-20")
})

test_that("p_format is unchanged for ordinary decimal p-values (#112)", {
  expect_equal(p_format(c(0.1, 0.01)), c("0.1", "0.01"))
  # trailing.zero padding (its intended use) still works
  expect_equal(p_format(c(0.1, 0.01), trailing.zero = TRUE), c("0.10", "0.01"))
  expect_equal(p_format(c(0.5, 0.001, 0.049)), c("0.5", "0.001", "0.049"))
  # very small values below the accuracy threshold still collapse to the sentinel
  expect_equal(p_format(1e-8), "<0.0001")
})

test_that("extract_number parses scientific notation without dropping the exponent (#148)", {
  expect_equal(extract_number("1e-04"), 1e-4)
  expect_equal(extract_number("<0.0001"), 1e-4)
  # ordinary strings are unchanged
  expect_equal(extract_number("0.001"), 0.001)
  expect_equal(extract_number(c("0.05", "1e-04")), c(0.05, 1e-4))
})
