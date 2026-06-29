context("test-kruskal_test")

test_that("kruskal_test reports the complete-case n when the data contain NAs (#224)", {
  set.seed(123)
  df <- data.frame(
    group = factor(rep(c(NA, "A", "B", "C"), each = 10)),
    value = c(rnorm(35), rep(NA, 5))
  )
  res <- kruskal_test(df, value ~ group)
  expect_equal(res$n, 25)                       # complete cases, not nrow = 40
  # an explicit na.action passed through ... is respected
  expect_equal(kruskal_test(df, value ~ group, na.action = stats::na.omit)$n, 25)
  # pre-cleaned data gives the same n
  expect_equal(kruskal_test(na.omit(df), value ~ group)$n, 25)
  # n stays the complete-case count even under na.pass (kruskal.test always uses
  # complete cases regardless of na.action), passed explicitly or set globally
  expect_equal(kruskal_test(df, value ~ group, na.action = stats::na.pass)$n, 25)
  old <- getOption("na.action"); on.exit(options(na.action = old))
  options(na.action = "na.pass")
  expect_equal(kruskal_test(df, value ~ group)$n, 25)
})

test_that("kruskal_test statistic/p are unchanged and match stats::kruskal.test (#224)", {
  set.seed(123)
  df <- data.frame(
    group = factor(rep(c(NA, "A", "B", "C"), each = 10)),
    value = c(rnorm(35), rep(NA, 5))
  )
  res <- kruskal_test(df, value ~ group)
  ref <- suppressWarnings(stats::kruskal.test(value ~ group, data = df))
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(res$p, ref$p.value)
})

test_that("kruskal_test n is unchanged for data without NAs (no-regression) (#224)", {
  res <- ToothGrowth %>% kruskal_test(len ~ dose)
  expect_equal(res$n, nrow(ToothGrowth))        # 60
  expect_equal(unname(round(res$statistic, 4)), 40.6689)
})

test_that("kruskal_test works on grouped data (#224)", {
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% kruskal_test(len ~ dose)
  expect_equal(nrow(res), 2L)
  expect_equal(res$n, c(30, 30))
})

test_that("kruskal_effsize uses the complete-case n with NAs (#224)", {
  set.seed(123)
  df <- data.frame(
    group = factor(rep(c(NA, "A", "B", "C"), each = 10)),
    value = c(rnorm(35), rep(NA, 5))
  )
  eff <- kruskal_effsize(df, value ~ group)
  expect_equal(eff$n, 25)
  # eta2[H] uses n in its denominator, so the corrected n also corrects the
  # effect size: recompute it from the (correct) H and complete-case n.
  kt <- kruskal_test(df, value ~ group)
  k <- kt$df + 1
  expected <- max(0, min(1, (kt$statistic - k + 1) / (eff$n - k)))
  expect_equal(eff$effsize, unname(expected), tolerance = 1e-7)
})
