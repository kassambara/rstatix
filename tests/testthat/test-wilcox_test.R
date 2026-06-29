context("test-wilcox-test")

test_that("Checking one-sample test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ 1, mu = 0)
  expect_equal(res$group1, "1")
  expect_equal(res$group2, "null model")
  expect_equal(res$n, 60)
  expect_equal(as.numeric(res$statistic), 1830)
  expect_equal(signif(res$p, 3), 1.66e-11)
})


test_that("Checking two-sample unpaired test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ supp)
  expect_equal(res$group1, "OJ")
  expect_equal(res$group2, "VC")
  expect_equal(res$n1, 30)
  expect_equal(res$n2, 30)
  expect_equal(as.numeric(res$statistic), 575.5)
  # Accept either 0.0645 (legacy) or 0.0637 (R-devel with exact conditional inference)
  expect_true(signif(res$p, 3) %in% c(0.0645, 0.0637),
              info = paste("Observed p =", signif(res$p, 3)))
})


test_that("Checking two-sample paired test", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ supp, paired = TRUE)
  expect_equal(res$group1, "OJ")
  expect_equal(res$group2, "VC")
  expect_equal(res$n1, 30)
  expect_equal(res$n2, 30)
  # Compare against base R computed the same way so the test tracks the active
  # R version instead of chasing hard-coded values: wilcox.test's paired V
  # statistic and p-value shifted on R-devel (350/0.00431 on R <= 4.5,
  # 369/0.00381 on devel).
  ref <- suppressWarnings(stats::wilcox.test(
    ToothGrowth$len[ToothGrowth$supp == "OJ"],
    ToothGrowth$len[ToothGrowth$supp == "VC"],
    paired = TRUE
  ))
  expect_equal(as.numeric(res$statistic), as.numeric(ref$statistic))
  expect_equal(res$p, ref$p.value, tolerance = 1e-3)
})

test_that("Checking pairwise comparisons", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose)
  expect_equal(res$group1, c("0.5", "0.5", "1"))
  expect_equal(res$group2, c("1", "2", "2"))
  expect_equal(res$n1, c(20, 20, 20))
  expect_equal(res$n2, c(20, 20, 20))
  expect_equal(as.numeric(res$statistic), c(33.5, 1.5, 61.0))
  # Accept either legacy or R-devel p-values
  legacy_p <- c(7.02e-6, 8.41e-08, 1.77e-04)
  rdevel_p <- c(7.74e-07, 4.35e-11, 7.57e-05)
  observed_p <- signif(res$p, 3)
  expect_true(all(observed_p %in% c(legacy_p, rdevel_p)) || all(abs(observed_p - legacy_p) < 1e-6) || all(abs(observed_p - rdevel_p) < 1e-6),
              info = paste("Observed p =", paste(observed_p, collapse=", ")))
})

test_that("Checking pairwise comparison against ref group", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose, ref.group = "0.5")
  expect_equal(res$group1, c("0.5", "0.5"))
  expect_equal(res$group2, c("1", "2"))
  expect_equal(res$n1, c(20, 20))
  expect_equal(res$n2, c(20, 20))
  expect_equal(as.numeric(res$statistic), c(33.5, 1.5))
  # Accept either legacy or R-devel p-values
  legacy_p <- c(7.02e-6, 8.41e-08)
  rdevel_p <- c(7.74e-07, 4.35e-11)
  observed_p <- signif(res$p, 3)
  expect_true(all(observed_p %in% c(legacy_p, rdevel_p)) || all(abs(observed_p - legacy_p) < 1e-6) || all(abs(observed_p - rdevel_p) < 1e-6),
              info = paste("Observed p =", paste(observed_p, collapse=", ")))
})


test_that("Checking pairwise comparisons against all", {
  data("ToothGrowth")
  res <- ToothGrowth %>% wilcox_test(len ~ dose, ref.group = "all")
  expect_equal(res$group1, c("all", "all", "all"))
  expect_equal(res$group2, c("0.5", "1", "2"))
  expect_equal(res$n1, c(60, 60, 60))
  expect_equal(res$n2, c(20, 20, 20))
  expect_equal(as.numeric(res$statistic), c(965.0, 572.5, 262.5))
  expect_equal(signif(res$p, 3), c(0.0000508, 0.764, 0.000179))
})


test_that("Checking grouped tests", {
  data("ToothGrowth")
  res <- ToothGrowth %>%
    group_by(dose) %>%
    wilcox_test(len ~ supp)
  expect_equal(res$group1, c("OJ", "OJ", "OJ"))
  expect_equal(res$group2, c("VC", "VC", "VC"))
  expect_equal(res$n1, c(10, 10, 10))
  expect_equal(res$n2, c(10, 10, 10))
  expect_equal(as.numeric(res$statistic), c(80.5, 88.5, 49.5))
  # Accept either legacy or R-devel p-values
  legacy_p <- c(0.0232, 0.00403, 1)
  rdevel_p <- c(0.0198, 0.00223, 0.986)
  observed_p <- signif(res$p, 3)
  expect_true(all(abs(observed_p - legacy_p) < 0.005) || all(abs(observed_p - rdevel_p) < 0.005),
              info = paste("Observed p =", paste(observed_p, collapse=", ")))
})


test_that("Empty values are not counting in group n size (104)", {
  # Data without NA
  df <- data.frame(
    g = rep(c("a", "b"), each = 10),
    v = rnorm(20)
  )

  # run Wilcoxon test --> sample sizes are correct
  res <- wilcox_test(df, v ~ g, paired = TRUE)
  expect_equal(c(res$n1, c(res$n1)), c(10, 10))

  # Insert NAs
  df$v[c(1, 12:14)] <- NA

  #repeat Wilcox test --> sample sizes are still the same
  res <- wilcox_test(data = df, v ~ g, paired = TRUE)
  expect_equal(c(res$n1, res$n2), c(9, 7))
})

test_that("wilcox_test detailed method reports the variant (#124)", {
  expect_equal((ToothGrowth %>% wilcox_test(len ~ supp, detailed = TRUE))$method,
               "Wilcoxon rank sum test")
  expect_equal((ToothGrowth %>% wilcox_test(len ~ supp, paired = TRUE, detailed = TRUE))$method,
               "Wilcoxon signed rank test")
})

test_that("wilcox_test does not crash on degenerate/all-tied data (#79, #167)", {
  set.seed(1)
  deg <- data.frame(value = c(0,0,0,0, 0,0,0,0,0), g = rep(c("a","b"), c(4, 5)))
  # default (detailed = FALSE) no longer runs the crash-prone confidence-interval step
  expect_true(is.data.frame(wilcox_test(deg, value ~ g)))
  expect_true(is.data.frame(pairwise_wilcox_test(deg, value ~ g)))
  # grouped data with a fully-constant subgroup no longer errors
  gdf <- data.frame(value = c(rep(0, 6), rnorm(6, 2)),
                    sex = rep(c("M", "F"), 6), grp = rep(c("g1", "g2"), each = 6))
  expect_true(is.data.frame(gdf %>% group_by(grp) %>% wilcox_test(value ~ sex)))
})

test_that("wilcox_test detailed = TRUE still returns the estimate and CI (#79)", {
  res <- ToothGrowth %>% wilcox_test(len ~ supp, detailed = TRUE)
  expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(res)))
  expect_false(is.na(res$conf.low))   # normal data -> a real confidence interval
})


# #127: surface a warning when wilcox.test silently reduced the CI confidence level.
# Whether a given data set forces a reduced CI is R-version dependent (newer
# wilcox.test can achieve the exact level where older versions could not), so the
# tests assert the warning fires *iff* the running R's wilcox.test actually
# reduced the level - tracking the active R version rather than a hard-coded value.
ci127_data <- function(){
  data.frame(
    Result = c(0,9,6,8,0,0,0,0,0,0,0,0,1,2,3,3,1,2,1,1,3,3,7,7),
    Timepoint = rep(c("Baseline", "Month 3"), 12)
  )
}

test_that("wilcox_test surfaces a reduced CI confidence level when one occurs (#127)", {
  d <- ci127_data()
  x <- d$Result[d$Timepoint == "Baseline"]
  y <- d$Result[d$Timepoint == "Month 3"]
  raw <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, conf.int = TRUE))
  reduced <- isTRUE(attr(raw$conf.int, "conf.level") < 0.95)

  w <- testthat::capture_warnings(
    res <- wilcox_test(d, Result ~ Timepoint, paired = TRUE, detailed = TRUE)
  )
  # the CI warning is present exactly when this R version reduced the CI level
  expect_equal(any(grepl("confidence interval", w)), reduced)
  # returned values are unchanged either way (only a warning is ever added)
  expect_equal(res$p, raw$p.value)
})

test_that("wilcox_test default (non-detailed) call never warns about the CI (#127)", {
  d <- ci127_data()
  w <- testthat::capture_warnings(wilcox_test(d, Result ~ Timepoint, paired = TRUE))
  expect_false(any(grepl("confidence interval", w)))   # no CI requested -> no warning
})

test_that("clean-data wilcox and t_test never warn about the CI (#127)", {
  set.seed(1)
  cd <- data.frame(v = c(rnorm(15), rnorm(15) + 1), g = rep(c("a", "b"), each = 15))
  w1 <- testthat::capture_warnings(wilcox_test(cd, v ~ g, detailed = TRUE))
  expect_false(any(grepl("confidence interval", w1)))   # full conf.level achievable
  d <- ci127_data()
  w2 <- testthat::capture_warnings(t_test(d, Result ~ Timepoint, paired = TRUE, detailed = TRUE))
  expect_false(any(grepl("confidence interval", w2)))   # t.test never reduces
})
