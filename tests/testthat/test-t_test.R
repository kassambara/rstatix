context("test-t_test")

test_that("pairwise_t_test drops unused factor levels (#133)", {
  set.seed(1)
  d <- data.frame(
    y = rnorm(30),
    g = factor(rep(c("a", "b", "c"), each = 10), levels = c("a", "b", "c", "d"))  # 'd' unused
  )
  res <- d %>% pairwise_t_test(y ~ g)
  expect_equal(nrow(res), 3L)                          # 3 comparisons over a/b/c, not the empty 'd'
  expect_false("d" %in% c(res$group1, res$group2))     # no impossible comparison
  # paired variant (the reported scenario)
  dp <- data.frame(
    id = factor(rep(1:8, 3)),
    g  = factor(rep(c("a", "b", "c"), each = 8), levels = c("a", "b", "c", "d")),
    y  = rnorm(24)
  )
  expect_equal(nrow(dp %>% pairwise_t_test(y ~ g, paired = TRUE)), 3L)
})

test_that("t_test detailed method reports the variant (#124)", {
  expect_equal((ToothGrowth %>% t_test(len ~ supp, detailed = TRUE))$method, "Welch t-test")
  expect_equal((ToothGrowth %>% t_test(len ~ supp, var.equal = TRUE, detailed = TRUE))$method, "T-test")
  expect_equal((ToothGrowth %>% t_test(len ~ supp, paired = TRUE, detailed = TRUE))$method, "Paired t-test")
  expect_equal((ToothGrowth %>% t_test(len ~ 1, mu = 20, detailed = TRUE))$method, "One-sample t-test")
})

test_that("t_test / wilcox_test handle a filtered factor with an unused level (#133)", {
  # 2 species kept, but the 'virginica' factor level is retained (empty) - used to error
  d <- iris %>% filter(Species %in% c("setosa", "versicolor"))
  expect_true(is.data.frame(t_test(d, Sepal.Length ~ Species)))
  expect_true(is.data.frame(wilcox_test(d, Sepal.Length ~ Species)))
})
