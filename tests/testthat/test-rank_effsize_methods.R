context("test-rank_effsize_methods")

tg_f <- function() { d <- ToothGrowth; d$dose <- factor(d$dose); d }

# ---- kruskal_effsize(method = "epsilon2") ---------------------------------
test_that("kruskal_effsize default (eta2) is unchanged", {
  d <- tg_f()
  res <- d %>% kruskal_effsize(len ~ dose)
  expect_equal(res$method, "eta2[H]")
  expect_equal(unname(res$effsize), 0.6784024, tolerance = 1e-6)
  # default output carries no non-default method key in the stashed args
  expect_null(attr(res, "args")$effsize.method)
})

test_that("kruskal_effsize(method = 'epsilon2') matches effectsize::rank_epsilon_squared", {
  # epsilon2 = H / (N - 1); pinned from effectsize 1.x (2026-07-11). Not a
  # dependency: value hard-coded.
  d <- tg_f()
  res <- d %>% kruskal_effsize(len ~ dose, method = "epsilon2")
  expect_equal(res$method, "epsilon2")
  expect_equal(unname(res$effsize), 0.689303987542, tolerance = 1e-9)
  # non-default metric is recorded in the stashed args (reproducibility)
  expect_equal(attr(res, "args")$effsize.method, "epsilon2")
  # independent recompute: H / (N - 1)
  kt <- d %>% kruskal_test(len ~ dose)
  expect_equal(unname(res$effsize), unname(kt$statistic / (nrow(d) - 1)))
  # genuinely differs from eta2 (not bias-corrected)
  expect_false(isTRUE(all.equal(unname(res$effsize), unname((d %>% kruskal_effsize(len ~ dose))$effsize))))
})

test_that("kruskal_effsize(method = 'epsilon2') works grouped and with a CI", {
  d <- tg_f()
  expect_equal(nrow(d %>% dplyr::group_by(supp) %>% kruskal_effsize(len ~ dose, method = "epsilon2")), 2L)
  skip_if_not_installed("boot")
  set.seed(1)
  ci <- d %>% kruskal_effsize(len ~ dose, method = "epsilon2", ci = TRUE, nboot = 100)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(ci)))
})

# ---- wilcox_effsize(method = "rank_biserial") -----------------------------
test_that("wilcox_effsize default (r) is unchanged", {
  skip_if_not_installed("coin")   # default metric r goes through coin::wilcox_test
  d <- tg_f()
  res <- d %>% wilcox_effsize(len ~ supp)
  expect_equal(unname(res$effsize), 0.2396302, tolerance = 1e-6)
  expect_null(attr(res, "args")$effsize.method)
})

test_that("wilcox_effsize(method = 'rank_biserial') equals cliff_delta for an independent test", {
  d <- tg_f()
  res <- d %>% wilcox_effsize(len ~ supp, method = "rank_biserial")
  expect_equal(unname(res$effsize), (d %>% cliff_delta(len ~ supp))$effsize %>% as.numeric())
  expect_equal(unname(res$effsize), 0.2788889, tolerance = 1e-6)
  expect_s3_class(res$magnitude, "ordered")   # Romano thresholds
})

test_that("wilcox_effsize(method = 'rank_biserial') handles pairwise and paired designs", {
  d <- tg_f()
  expect_equal(nrow(d %>% wilcox_effsize(len ~ dose, method = "rank_biserial")), 3L)
  # paired: matches the matched-pairs rank-biserial (same as the effect.size column)
  set.seed(1)
  dp <- data.frame(id = factor(rep(1:12, 2)), g = factor(rep(c("a", "b"), each = 12)),
                   y = c(rnorm(12, 10, 3), rnorm(12, 13, 3)))
  paired <- dp %>% wilcox_effsize(y ~ g, paired = TRUE, method = "rank_biserial")
  from.column <- dp %>% wilcox_test(y ~ g, paired = TRUE, id = "id", effect.size = TRUE)
  expect_equal(unname(paired$effsize), unname(from.column$rank.biserial))
  # no calibrated magnitude for the matched-pairs rank-biserial (as wilcox_test);
  # the independent case keeps the Romano thresholds
  expect_true(is.na(paired$magnitude))
  indep <- dp %>% wilcox_effsize(y ~ g, method = "rank_biserial")
  expect_false(is.na(indep$magnitude))
  # both share one factor type, so bind_rows() does not coerce the levels
  expect_identical(levels(paired$magnitude), levels(indep$magnitude))
})

test_that("wilcox_effsize(method = 'rank_biserial', ci = TRUE) adds a bootstrap CI", {
  skip_if_not_installed("boot")
  d <- tg_f()
  set.seed(1)
  res <- d %>% wilcox_effsize(len ~ supp, method = "rank_biserial", ci = TRUE, nboot = 100)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
  expect_true(res$conf.low <= unname(res$effsize) && unname(res$effsize) <= res$conf.high)
})
