context("test-cliff_delta")

# Independent re-derivation of Cliff's delta straight from its definition,
# delta = (#{x > y} - #{x < y}) / (n1 * n2), without the package helpers.
cliff_ref <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
}

test_that("two-sample cliff_delta returns the right shape and value", {
  res <- ToothGrowth %>% cliff_delta(len ~ supp)
  expect_s3_class(res, "cliff_delta")
  expect_s3_class(res, "rstatix_test")
  expect_equal(colnames(res), c(".y.", "group1", "group2", "effsize", "n1", "n2", "magnitude"))
  expect_equal(nrow(res), 1L)
  # no test-statistic / p columns: Cliff's delta is a pure effect size
  expect_false(any(c("statistic", "p", "method") %in% colnames(res)))
  x <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
  y <- ToothGrowth$len[ToothGrowth$supp == "VC"]
  expect_equal(unname(res$effsize), cliff_ref(x, y))
  expect_equal(res$n1, 30L)
  expect_equal(res$n2, 30L)
})

test_that("cliff_delta matches effectsize::rank_biserial()", {
  # Cliff's delta is algebraically identical to the (matched) rank-biserial
  # correlation, so it equals effectsize::rank_biserial(). Values hard-coded
  # from effectsize 1.0.1 (2026-07-11); effectsize is not a dependency.
  expect_equal(
    unname((ToothGrowth %>% cliff_delta(len ~ supp))$effsize),
    0.2788888889, tolerance = 1e-7
  )
  # iris versicolor vs virginica: a non-round value
  iv <- subset(iris, Species %in% c("versicolor", "virginica"))
  iv$Species <- factor(iv$Species)
  expect_equal(
    unname((iv %>% cliff_delta(Sepal.Length ~ Species))$effsize),
    -0.5792, tolerance = 1e-7
  )
})

test_that("pairwise cliff_delta computes one row per pair, each matching the definition", {
  res <- ToothGrowth %>% cliff_delta(len ~ dose)
  expect_equal(nrow(res), 3L)
  expect_equal(unname(res$effsize), c(-0.8325, -0.9925, -0.6950), tolerance = 1e-7)
  # re-derive every pair from the raw data
  lv <- c("0.5", "1", "2")
  prs <- utils::combn(lv, 2)
  expected <- vapply(seq_len(ncol(prs)), function(i) {
    a <- ToothGrowth$len[ToothGrowth$dose == prs[1, i]]
    b <- ToothGrowth$len[ToothGrowth$dose == prs[2, i]]
    cliff_ref(a, b)
  }, numeric(1))
  expect_equal(unname(res$effsize), expected, tolerance = 1e-12)
})

test_that("ref.group and ref.group = 'all' dispatch correctly", {
  # every comparison is against the reference level
  ref <- ToothGrowth %>% cliff_delta(len ~ dose, ref.group = "0.5")
  expect_equal(nrow(ref), 2L)
  expect_true(all(ref$group1 == "0.5"))
  # against the pooled 'all' pseudo-group
  all <- ToothGrowth %>% cliff_delta(len ~ dose, ref.group = "all")
  expect_equal(nrow(all), 3L)
  expect_true(all(all$group1 == "all"))
  expect_equal(all$n1, rep(60L, 3))   # pooled reference size
})

test_that("grouped cliff_delta computes within each group", {
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% cliff_delta(len ~ dose)
  expect_equal(nrow(res), 6L)              # 2 groups x 3 pairs
  expect_true("supp" %in% colnames(res))
  # spot-check one group against the definition
  oj <- subset(ToothGrowth, supp == "OJ")
  a <- oj$len[oj$dose == "0.5"]; b <- oj$len[oj$dose == "1"]
  got <- res$effsize[res$supp == "OJ" & res$group1 == "0.5" & res$group2 == "1"]
  expect_equal(unname(got), cliff_ref(a, b), tolerance = 1e-12)
})

test_that("ci = TRUE adds a bootstrap confidence interval", {
  skip_if_not_installed("boot")
  set.seed(1)
  res <- ToothGrowth %>% cliff_delta(len ~ supp, ci = TRUE, nboot = 200)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
  expect_true(res$conf.low <= res$effsize)
  expect_true(res$conf.high >= res$effsize)
  expect_true(res$conf.low >= -1 && res$conf.high <= 1)
})

test_that("the magnitude follows Romano et al. (2006) thresholds", {
  # boundaries are inclusive on the upper bin (strict < in Romano's definition)
  m <- get_cliff_delta_magnitude(c(0, 0.146, 0.147, 0.329, 0.33, 0.473, 0.474, 1))
  expect_equal(
    as.character(m),
    c("negligible", "negligible", "small", "small", "medium", "medium", "large", "large")
  )
  # symmetric in sign
  expect_equal(as.character(get_cliff_delta_magnitude(-0.9)), "large")
  expect_s3_class(m, "ordered")
})

test_that("cliff_delta errors cleanly when a group cannot be formed", {
  # one-sample form has no second group: Cliff's delta is undefined
  expect_error(ToothGrowth %>% cliff_delta(len ~ 1))
})

test_that("missing values are dropped before computing delta", {
  d <- data.frame(
    val = c(1, 2, 3, NA, 5, 4, 6, 7, 8, NA),
    grp = factor(rep(c("a", "b"), each = 5))
  )
  res <- d %>% cliff_delta(val ~ grp)
  a <- d$val[d$grp == "a"]; b <- d$val[d$grp == "b"]
  expect_equal(unname(res$effsize), cliff_ref(a, b))
  # n1/n2 count only the non-missing observations, matching what delta used
  expect_equal(res$n1, sum(!is.na(a)))
  expect_equal(res$n2, sum(!is.na(b)))
})

test_that("paired = TRUE is rejected rather than silently ignored", {
  # Cliff's delta is a two-independent-samples statistic; before this guard the
  # argument was absorbed by `...` and the independent-samples value returned.
  expect_error(
    cliff_delta(ToothGrowth, len ~ supp, paired = TRUE),
    "two independent samples"
  )
})

test_that("Inf observations are compared, not dropped", {
  # Inf > finite counts as a win and Inf vs Inf as a tie, so the documented
  # formula keeps the full n1 * n2 denominator. Reference:
  # effectsize::rank_biserial() = 0.2122222222 on this data (effectsize 1.0.1,
  # 2026-07-23), equal to the hand count (191 - 0 wins asymmetry) / 900.
  tinf <- ToothGrowth
  tinf$len[1] <- Inf
  res <- cliff_delta(tinf, len ~ supp)
  x <- tinf$len[tinf$supp == "OJ"]; y <- tinf$len[tinf$supp == "VC"]
  hand <- (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
  expect_equal(unname(res$effsize), hand, tolerance = 1e-12)
  expect_equal(unname(res$effsize), 0.2122222222, tolerance = 1e-9)
  expect_equal(c(res$n1, res$n2), c(30L, 30L))
})

test_that("the bootstrap interval resamples within groups and survives small groups", {
  # An unstratified resample of the pooled rows can lose a whole small group,
  # which aborted the call mid-bootstrap for most seeds.
  ps <- data.frame(v = c(1, 3, 5, 7, 9, 2, 4, 6, 18, 20),
                   g = factor(rep(c("lo", "hi"), each = 5)))
  for (s in 1:10) {
    set.seed(s)
    res <- suppressWarnings(cliff_delta(ps, v ~ g, ci = TRUE, nboot = 200))
    expect_false(anyNA(res$effsize))
    expect_true(all(stats::na.omit(c(res$conf.low, res$conf.high)) >= -1))
    expect_true(all(stats::na.omit(c(res$conf.low, res$conf.high)) <= 1))
  }
})
