context("test-fligner_test")

test_that("fligner_test matches stats::fligner.test (#179)", {
  res <- ToothGrowth %>% fligner_test(len ~ dose)
  ref <- stats::fligner.test(len ~ dose, data = ToothGrowth)
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(unname(res$df), unname(ref$parameter))
  expect_equal(res$p, ref$p.value)
})

test_that("fligner_test returns the expected columns (#179)", {
  res <- ToothGrowth %>% fligner_test(len ~ dose)
  expect_equal(colnames(res), c(".y.", "n", "statistic", "df", "p", "method"))
  expect_equal(nrow(res), 1L)
  expect_match(unique(res$method), "Fligner")
})

test_that("fligner_test works on grouped data (#179)", {
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% fligner_test(len ~ dose)
  expect_true("supp" %in% colnames(res))
  expect_equal(nrow(res), 2L)
})

test_that("fligner_test reports the complete-case n with NAs (#179)", {
  df <- ToothGrowth
  df$len[1:5] <- NA
  res <- df %>% fligner_test(len ~ dose)
  expect_equal(res$n, 55)                 # complete cases, not nrow = 60
  # statistic still matches base fligner.test on the same data
  ref <- stats::fligner.test(len ~ dose, data = df)
  expect_equal(unname(res$statistic), unname(ref$statistic))
})

test_that("fligner_test n is unchanged for data without NAs (no-regression) (#179)", {
  expect_equal((ToothGrowth %>% fligner_test(len ~ dose))$n, nrow(ToothGrowth))
})

test_that("fligner_test surfaces the base-R error for a single group (#179)", {
  df <- data.frame(y = rnorm(10), g = factor(rep("a", 10)))
  # base fligner.test() errors (message is locale-translated, so don't match text)
  expect_error(df %>% fligner_test(y ~ g))
})

test_that("fligner_test grouped reports per-group n (#179)", {
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% fligner_test(len ~ dose)
  expect_equal(res$n, c(30, 30))
})

test_that("get_description/get_test_label give a friendly fligner_test label (#179)", {
  res <- ToothGrowth %>% fligner_test(len ~ dose)
  expect_equal(get_description(res), "Fligner-Killeen")
  lab <- get_test_label(res, type = "text")
  expect_true(is.character(lab) && grepl("Fligner-Killeen", lab))
})
