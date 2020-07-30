context("test-p_mark_significance")

test_that("p_mark_significance works when NA only", {
  na_signif <- p_mark_significant(NA)
  expect_equal(na_signif, "NA")
})
