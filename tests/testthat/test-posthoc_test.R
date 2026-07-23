context("test-posthoc_test")

test_that("normal residuals + equal variances route to Tukey HSD", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  res <- d %>% posthoc_test(len ~ dose)
  expect_equal(attr(res, "posthoc.method"), "tukey_hsd")
  expect_s3_class(res, "posthoc_test")
  expect_s3_class(res, "tukey_hsd")
  # returns the Tukey table (estimate + CI columns)
  expect_true(all(c("estimate", "conf.low", "conf.high", "p.adj") %in% colnames(res)))
  a <- attr(res, "assumptions")
  expect_true(a$normal)
  expect_true(a$equal.variance)
  expect_true(a$normality.p > 0.05 && a$homogeneity.p > 0.05)
})

test_that("normal per group but unequal variances route to Games-Howell", {
  set.seed(2)
  d <- data.frame(
    y = c(rnorm(30, 0, 1), rnorm(30, 5, 4), rnorm(30, 10, 8)),
    g = factor(rep(1:3, each = 30))
  )
  res <- d %>% posthoc_test(y ~ g)
  expect_equal(attr(res, "posthoc.method"), "games_howell_test")
  expect_s3_class(res, "games_howell_test")
  a <- attr(res, "assumptions")
  expect_true(a$normal)             # each group is normal
  expect_false(a$equal.variance)    # variances differ
})

test_that("non-normal data route to Dunn's test", {
  set.seed(1)
  d <- data.frame(
    y = c(rexp(20), rexp(20) + 2, rexp(20) + 4),
    g = factor(rep(1:3, each = 20))
  )
  res <- d %>% posthoc_test(y ~ g)
  expect_equal(attr(res, "posthoc.method"), "dunn_test")
  expect_s3_class(res, "dunn_test")
  expect_false(attr(res, "assumptions")$normal)
})

test_that("the chosen test and assumption verdicts are printed", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  out <- paste(utils::capture.output(print(d %>% posthoc_test(len ~ dose))), collapse = "\n")
  expect_true(grepl("Post-hoc test chosen: Tukey HSD", out))
  expect_true(grepl("Normality", out) && grepl("Homogeneity", out))
  expect_true(grepl("equal variances", out))
})

test_that("the significance level changes the routing at the boundary", {
  # Data whose worst-group normality p falls between two thresholds: strict
  # alpha keeps it 'normal' (parametric), a lax alpha flips it to non-parametric.
  set.seed(3)
  d <- data.frame(
    y = c(rnorm(30), rnorm(30, 2), rnorm(30, 4)),
    g = factor(rep(1:3, each = 30))
  )
  p.min <- min(tapply(d$y, d$g, function(v) shapiro.test(v)$p.value))
  # choose an alpha just above p.min so normality is judged to FAIL -> Dunn,
  # and the default 0.05 (below p.min here) keeps it parametric
  lax <- (d %>% posthoc_test(y ~ g, significance = p.min + 0.01))
  strict <- (d %>% posthoc_test(y ~ g, significance = p.min - 0.01))
  expect_equal(attr(lax, "posthoc.method"), "dunn_test")
  expect_true(attr(strict, "posthoc.method") %in% c("tukey_hsd", "games_howell_test"))
})

test_that("extra arguments are forwarded only to a route that accepts them", {
  # Dunn route: p.adjust.method is honoured (changes p.adj)
  set.seed(1)
  d.dunn <- data.frame(
    y = c(rexp(20), rexp(20) + 2, rexp(20) + 4),
    g = factor(rep(1:3, each = 20))
  )
  holm <- d.dunn %>% posthoc_test(y ~ g)
  bonf <- d.dunn %>% posthoc_test(y ~ g, p.adjust.method = "bonferroni")
  expect_equal(attr(bonf, "posthoc.method"), "dunn_test")
  expect_false(isTRUE(all.equal(holm$p.adj, bonf$p.adj)))
  # Games-Howell route: games_howell_test() takes no p.adjust.method / no `...`,
  # so the same argument must be dropped, NOT crash (data-dependent routing).
  set.seed(2)
  d.gh <- data.frame(
    y = c(rnorm(30, 0, 1), rnorm(30, 5, 4), rnorm(30, 10, 8)),
    g = factor(rep(1:3, each = 30))
  )
  expect_error(d.gh %>% posthoc_test(y ~ g, p.adjust.method = "bonferroni"), NA)
  expect_equal(
    attr(d.gh %>% posthoc_test(y ~ g, p.adjust.method = "bonferroni"), "posthoc.method"),
    "games_howell_test"
  )
})

test_that("invalid designs are rejected with a clear error", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  expect_error(d %>% dplyr::group_by(supp) %>% posthoc_test(len ~ dose), "[Gg]rouped")
  expect_error(d %>% posthoc_test(len ~ 1), "grouping variable")
})

test_that("a multi-factor formula is rejected with a one-way-design message", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  expect_error(posthoc_test(df, len ~ dose + supp), "one-way design")
  expect_error(posthoc_test(df, len ~ dose * supp), "one-way design")
  expect_error(check_test_assumptions(df, len ~ dose + supp), "one-way design")
})
