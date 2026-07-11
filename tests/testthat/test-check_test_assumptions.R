context("test-check_test_assumptions")

test_that("check_test_assumptions returns a tidy one-row recommendation", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  res <- d %>% check_test_assumptions(len ~ dose)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(
    colnames(res),
    c(".y.", "normality.p", "homogeneity.p", "normal", "equal.variance",
      "significance", "omnibus", "posthoc")
  )
  # normal + equal variances -> parametric pair
  expect_true(res$normal)
  expect_true(res$equal.variance)
  expect_equal(res$omnibus, "anova_test")
  expect_equal(res$posthoc, "tukey_hsd")
})

test_that("the recommendation follows the assumption verdicts", {
  # unequal variances, normal per group -> Welch + Games-Howell
  set.seed(2)
  d.gh <- data.frame(y = c(rnorm(30, 0, 1), rnorm(30, 5, 4), rnorm(30, 10, 8)),
                     g = factor(rep(1:3, each = 30)))
  rec.gh <- d.gh %>% check_test_assumptions(y ~ g)
  expect_equal(rec.gh$omnibus, "welch_anova_test")
  expect_equal(rec.gh$posthoc, "games_howell_test")
  # not normal -> Kruskal + Dunn
  set.seed(1)
  d.k <- data.frame(y = c(rexp(20), rexp(20) + 2, rexp(20) + 4),
                    g = factor(rep(1:3, each = 20)))
  rec.k <- d.k %>% check_test_assumptions(y ~ g)
  expect_false(rec.k$normal)
  expect_equal(rec.k$omnibus, "kruskal_test")
  expect_equal(rec.k$posthoc, "dunn_test")
})

test_that("the recommender and posthoc_test agree on the chosen post-hoc", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  rec <- d %>% check_test_assumptions(len ~ dose)
  ph <- d %>% posthoc_test(len ~ dose)
  expect_equal(rec$posthoc, attr(ph, "posthoc.method"))
})

test_that("posthoc_test(.assumptions=) reuses the verdicts and matches the direct call", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  rec <- d %>% check_test_assumptions(len ~ dose)
  direct <- d %>% posthoc_test(len ~ dose)
  reused <- d %>% posthoc_test(len ~ dose, .assumptions = rec)
  expect_equal(attr(reused, "posthoc.method"), attr(direct, "posthoc.method"))
  expect_equal(as.data.frame(reused), as.data.frame(direct))
  expect_equal(attr(reused, "assumptions"), attr(direct, "assumptions"))
})

test_that("posthoc_test(omnibus=) warns only on a parametric/non-parametric mismatch", {
  set.seed(1)
  d <- data.frame(y = c(rexp(20), rexp(20) + 2, rexp(20) + 4),
                  g = factor(rep(1:3, each = 20)))   # non-normal -> dunn route
  # parametric omnibus + non-parametric route -> warn
  expect_warning(d %>% posthoc_test(y ~ g, omnibus = d %>% anova_test(y ~ g)),
                 "different family")
  # coherent: kruskal omnibus + dunn route -> no warning
  expect_silent(d %>% posthoc_test(y ~ g, omnibus = d %>% kruskal_test(y ~ g)))
  # parametric equal-var (ANOVA) omnibus followed by a Games-Howell route (unequal
  # variances) is also a family mismatch -> warns
  set.seed(2)
  d.gh <- data.frame(y = c(rnorm(30, 0, 1), rnorm(30, 5, 4), rnorm(30, 10, 8)),
                     g = factor(rep(1:3, each = 30)))
  expect_warning(d.gh %>% posthoc_test(y ~ g, omnibus = d.gh %>% anova_test(y ~ g)),
                 "different family")
  # an unrecognized omnibus object degrades to no warning, not an error --
  # including a malformed object whose `args` attribute is not a list
  expect_silent(d %>% posthoc_test(y ~ g, omnibus = list(not = "an omnibus")))
  expect_silent(d %>% posthoc_test(y ~ g, omnibus = d %>% t_test(y ~ g)))
  expect_silent(d %>% posthoc_test(y ~ g, omnibus = structure(1, args = "not-a-list")))
})

test_that("check_test_assumptions rejects invalid designs", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  expect_error(d %>% dplyr::group_by(supp) %>% check_test_assumptions(len ~ dose),
               "[Gg]rouped")
  expect_error(d %>% check_test_assumptions(len ~ 1), "grouping variable")
})
