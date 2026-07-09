context("test-kruskal_effsize")

test_that("kruskal_effsize clamps a degenerate eta-squared to [0,1] and labels it 'small' (#217)", {
  # identical, interleaved group profiles -> tiny H -> formula yields a negative eta-squared
  d <- data.frame(y = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                  g = factor(rep(c("a", "b", "c"), each = 3)))
  res <- suppressWarnings(kruskal_effsize(d, y ~ g))
  expect_gte(res$effsize, 0)                          # never negative (clamped to [0,1])
  expect_equal(res$effsize, 0)                        # this degenerate case clamps to 0
  expect_equal(as.character(res$magnitude), "small")  # and is NOT reported as 'large'
})

test_that("kruskal_effsize magnitude is unchanged for valid effect sizes (no regression, #217)", {
  # ToothGrowth dose has a strong effect -> magnitude 'large' (unchanged behaviour)
  res <- ToothGrowth %>% kruskal_effsize(len ~ dose)
  expect_true(res$effsize > 0.14)
  expect_equal(as.character(res$magnitude), "large")
})

test_that("kruskal_effsize accepts bootstrap parallel options", {
  skip_if_not_installed("boot")
  set.seed(123)

  res <- ToothGrowth %>%
    kruskal_effsize(len ~ dose, ci = TRUE, nboot = 50,
                    boot.parallel = "no", boot.ncpus = 1)

  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
  expect_equal(nrow(res), 1L)
})

test_that("kruskal_effsize still accepts abbreviated argument names (no regression)", {
  # The bootstrap arguments are named boot.ncpus (not ncpus) so that `n` keeps
  # partial-matching unambiguously to `nboot`.
  skip_if_not_installed("boot")
  set.seed(123)
  res <- ToothGrowth %>% kruskal_effsize(len ~ dose, ci = TRUE, n = 200)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
  expect_equal(round(res$effsize, 4), 0.6784)
})
