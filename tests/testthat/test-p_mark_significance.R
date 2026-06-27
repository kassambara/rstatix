context("test-p_mark_significance")

test_that("p_mark_significance works when NA only", {
  na_signif <- p_mark_significant(NA)
  expect_equal(na_signif, "NA")
})

test_that("p_mark_significant handles scientific-notation strings (#148)", {
  # The reported edge case: a formatted scientific p-value used to yield "e-NA"
  # with a coercion warning
  res <- expect_warning(
    data.frame(p = 1e-4) %>% p_format() %>% p_mark_significant(),
    regexp = NA
  )
  expect_equal(res$p, "1e-04****")
  expect_false(grepl("NA", res$p))
})

test_that("p_mark_significant is robust to mixed-magnitude scientific vectors (#148)", {
  # a tiny + a large p makes format.pval render the whole vector in scientific
  # notation; this used to yield "e-NA" for every element
  res <- expect_warning(
    data.frame(p = c(1e-4, 0.5)) %>% p_format() %>% p_mark_significant(),
    regexp = NA
  )
  expect_false(any(grepl("NA", res$p)))     # no coercion artefact
  expect_false(any(grepl("e-$|e$", res$p))) # no dangling exponent
  expect_true(grepl("\\*\\*\\*\\*$", res$p[1]))  # 1e-4 is highly significant
  expect_false(grepl("\\*", res$p[2]))            # 0.5 is not significant
})

test_that("p_mark_significant is unchanged for ordinary decimal strings (#148)", {
  expect_equal(p_mark_significant("0.001"), "0.001***")
  expect_equal(p_mark_significant("0.5"), "0.5")
  df <- data.frame(p = c(0.5, 0.001, 0.049)) %>% p_format() %>% p_mark_significant()
  expect_equal(df$p, c("0.5", "0.001***", "0.049*"))
})
