context("test-kruskal_effsize")

test_that("kruskal_effsize does not mislabel a negative eta-squared as 'large' (#217)", {
  # identical, interleaved group profiles -> tiny H -> (degenerate) negative eta-squared
  d <- data.frame(y = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                  g = factor(rep(c("a", "b", "c"), each = 3)))
  res <- suppressWarnings(kruskal_effsize(d, y ~ g))
  expect_true(res$effsize < 0)                        # negative (negligible) effect size
  expect_equal(as.character(res$magnitude), "small")  # must NOT be reported as 'large'
})

test_that("kruskal_effsize magnitude is unchanged for valid effect sizes (no regression, #217)", {
  # ToothGrowth dose has a strong effect -> magnitude 'large' (unchanged behaviour)
  res <- ToothGrowth %>% kruskal_effsize(len ~ dose)
  expect_true(res$effsize > 0.14)
  expect_equal(as.character(res$magnitude), "large")
})
