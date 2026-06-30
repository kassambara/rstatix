context("test-cor_test-weights")

# #47: cor_test() wraps stats::cor.test(), which has no `weights` argument. A
# `weights =` used to be silently mis-handled (treated as an extra variable, or
# dropped) and returned the unweighted result. It must now error clearly.

test_that("cor_test() errors when weights are supplied (#47)", {
  set.seed(1)
  df <- data.frame(x = rnorm(20), y = rnorm(20), wt = runif(20))
  expect_error(df %>% cor_test(x, y, weights = wt), "weighted correlation")
  # also with the vars/vars2 interface
  expect_error(df %>% cor_test(vars = "x", vars2 = "y", weights = wt),
               "weighted correlation")
})

test_that("a bare column literally named 'weights' is unaffected (#47)", {
  set.seed(1)
  df <- data.frame(weights = rnorm(20), x = rnorm(20))
  res <- df %>% cor_test(weights, x)
  expect_equal(nrow(res), 1L)
  expect_true(all(c("weights", "x") %in% c(res$var1, res$var2)))
})

test_that("ordinary cor_test() calls are unchanged (no-regression) (#47)", {
  set.seed(1)
  df <- data.frame(x = rnorm(20), y = rnorm(20))
  res <- df %>% cor_test(x, y)
  expect_equal(nrow(res), 1L)
  expect_equal(res$var1, "x"); expect_equal(res$var2, "y")
  # multi-variable mode still returns the full matrix of pairs
  expect_equal(nrow(mtcars %>% cor_test(wt, mpg, disp)), 9L)
})
