context("test-error-as-na")

test_that("error.as.na = FALSE (default) still errors on a constant comparison (#158)", {
  # both groups constant -> t.test: 'data are essentially constant'
  dd <- data.frame(g = rep(c("x", "y"), each = 4), v = c(rep(7, 4), rep(3, 4)))
  expect_error(dd %>% t_test(v ~ g))
  expect_error(dd %>% t_test(v ~ g, error.as.na = FALSE))
})

test_that("error.as.na = FALSE (default) still errors on too few observations (#208)", {
  d1 <- data.frame(g = c("a", "a", "a", "b"), v = c(1, 2, 3, 5))   # b has 1 obs
  expect_error(d1 %>% t_test(v ~ g))
})

test_that("error.as.na = TRUE returns an NA row for a constant comparison and warns (#158)", {
  dd <- data.frame(g = rep(c("x", "y"), each = 4), v = c(rep(7, 4), rep(3, 4)))
  expect_warning(res <- dd %>% t_test(v ~ g, error.as.na = TRUE), "Could not compute")
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$statistic) && is.na(res$p))
  expect_equal(res$n1, 4); expect_equal(res$n2, 4)   # n still reported
})

test_that("error.as.na = TRUE returns an NA row for too-few-observations (#208)", {
  d1 <- data.frame(g = c("a", "a", "a", "b"), v = c(1, 2, 3, 5))
  res <- suppressWarnings(d1 %>% t_test(v ~ g, error.as.na = TRUE))
  expect_true(is.na(res$p))
  expect_equal(res$n1, 3); expect_equal(res$n2, 1)
})

test_that("error.as.na = TRUE computes the good comparisons and NA-only the bad pair (#208)", {
  set.seed(1)
  # a,b both constant (a-b is degenerate); c varies -> a-c, b-c computable
  df <- data.frame(g = rep(c("a", "b", "c"), each = 4),
                   v = c(rep(7, 4), rep(3, 4), rnorm(4) + 10))
  res <- suppressWarnings(df %>% t_test(v ~ g, error.as.na = TRUE))
  expect_equal(nrow(res), 3L)
  ab <- res[res$group1 == "a" & res$group2 == "b", ]
  expect_true(is.na(ab$p))                         # degenerate pair -> NA
  others <- res[!(res$group1 == "a" & res$group2 == "b"), ]
  expect_false(any(is.na(others$p)))               # the rest are computed
  # p.adjust is computed over the non-NA comparisons (NA stays NA)
  expect_true(is.na(ab$p.adj))
})

test_that("error.as.na = TRUE keeps good groups in a grouped analysis (#208)", {
  df <- data.frame(
    supp = rep(c("S1", "S2"), each = 8),
    g    = rep(rep(c("x", "y"), each = 4), 2),
    v    = c(rep(2, 8), rnorm(4), rnorm(4) + 3)     # S1 is constant in both groups
  )
  res <- suppressWarnings(df %>% dplyr::group_by(supp) %>% t_test(v ~ g, error.as.na = TRUE))
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$p[res$supp == "S1"]))       # degenerate group -> NA
  expect_false(is.na(res$p[res$supp == "S2"]))      # good group -> computed
})

test_that("error.as.na = TRUE also works for wilcox_test (#208)", {
  # a group with no finite observations makes wilcox.test() error
  d1 <- data.frame(g = rep(c("a", "b"), each = 2), v = c(NA, NA, 1, 2))
  res <- suppressWarnings(d1 %>% wilcox_test(v ~ g, error.as.na = TRUE))
  expect_true(is.na(res$p))
  expect_equal(res$n1, 0); expect_equal(res$n2, 2)
})

test_that("error.as.na = TRUE keeps the wilcox_test schema (no phantom df column) (#208)", {
  df <- data.frame(g = rep(c("a", "b", "c"), each = 4),
                   v = c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, 7, 8))   # a -> NA row
  res <- suppressWarnings(df %>% wilcox_test(v ~ g, error.as.na = TRUE))
  expect_false("df" %in% colnames(res))           # wilcox_test never has df
  # a t_test NA row, by contrast, keeps df
  dt <- data.frame(g = rep(c("a", "b"), each = 4), v = c(rep(7, 4), rep(3, 4)))
  expect_true("df" %in% colnames(suppressWarnings(dt %>% t_test(v ~ g, error.as.na = TRUE))))
})

test_that("error.as.na = TRUE does not alter results on valid data (no-regression)", {
  with.flag <- ToothGrowth %>% t_test(len ~ dose, error.as.na = TRUE)
  without   <- ToothGrowth %>% t_test(len ~ dose)
  expect_equal(with.flag$statistic, without$statistic)
  expect_equal(with.flag$p, without$p)
  expect_equal(colnames(with.flag), colnames(without))
})
