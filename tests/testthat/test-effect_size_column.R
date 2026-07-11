context("test-effect_size_column")

# Unbalanced groups with effects in BOTH directions (b above a, c below a): equal
# n hides the pooled-vs-per-pair SD gap, and a one-directional effect hides sign
# errors. Deterministic values, no RNG.
both_dir_data <- function() {
  data.frame(
    y = c(10, 11, 12, 13, 9, 10, 11, 8,        # a: mean ~ 10.5 (n = 8)
          18, 20, 19, 21, 22, 20, 19, 23, 20, 18, 21, 19,  # b: mean ~ 20   (n = 12)
          4, 5, 3, 6, 5, 4),                   # c: mean ~ 4.5  (n = 6)
    g = factor(rep(c("a", "b", "c"), c(8, 12, 6)))
  )
}

tg_f <- function() {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  d
}

# ---- t_test: per-pair Cohen's d -------------------------------------------
test_that("t_test(effect.size = TRUE) adds cohens.d matching cohens_d(), with magnitude", {
  d <- tg_f()
  res <- d %>% t_test(len ~ dose, effect.size = TRUE)
  expect_true(all(c("cohens.d", "magnitude") %in% colnames(res)))
  expect_equal(res$cohens.d, (d %>% cohens_d(len ~ dose))$effsize %>% as.numeric())
  expect_s3_class(res$magnitude, "ordered")
  # default off adds no effect-size columns
  expect_false(any(c("cohens.d", "cliff.delta", "r") %in%
                     colnames(d %>% t_test(len ~ dose))))
})

test_that("t_test cohens.d sign follows the group1 - group2 mean difference (both directions)", {
  d <- both_dir_data()
  res <- d %>% t_test(y ~ g, effect.size = TRUE)
  mns <- tapply(d$y, d$g, mean)
  expected.sign <- sign(mns[res$group1] - mns[res$group2])
  expect_equal(sign(res$cohens.d), as.numeric(expected.sign))
})

# ---- pairwise_t_test: pooled vs per-pair SD -------------------------------
test_that("pairwise_t_test(pool.sd = TRUE) reports the common-SD d (emmeans convention)", {
  # Pinned from emmeans::eff_size(emmeans(lm(len ~ dose)), sigma = sigma(m),
  # edf = df.residual(m)) on ToothGrowth (emmeans 1.x, 2026-07-11). This is the
  # pooled-model d, NOT the per-pair cohens_d, because pool.sd = TRUE builds its
  # p-values from one common within-group SD.
  res <- tg_f() %>% pairwise_t_test(len ~ dose, effect.size = TRUE)
  expect_equal(res$cohens.d, c(-2.152198, -3.652607, -1.500409), tolerance = 1e-5)
})

test_that("pairwise_t_test(pool.sd = FALSE) reports the per-pair cohens_d", {
  d <- tg_f()
  res <- d %>% pairwise_t_test(len ~ dose, pool.sd = FALSE, effect.size = TRUE)
  expect_equal(res$cohens.d, (d %>% cohens_d(len ~ dose))$effsize %>% as.numeric())
})

test_that("the pooled-SD d uses complete-case counts, matching emmeans under missing data", {
  # A missing outcome value must not inflate the pooled-SD weights: the common SD
  # is weighted by each group's COMPLETE-CASE count, so cohens.d stays equal to
  # emmeans::eff_size(sigma = sigma(m), edf = df.residual(m)) even with NAs.
  # Pinned from emmeans on ToothGrowth with a fixed NA pattern (emmeans 1.x,
  # 2026-07-11); a get_group_size()-weighted SD would give different values.
  d <- tg_f()
  d$len[c(1, 8, 15, 22, 29, 36, 43, 50)] <- NA
  res <- d %>% pairwise_t_test(len ~ dose, effect.size = TRUE)
  expect_equal(res$cohens.d, c(-1.937440610, -3.840655342, -1.903214732),
               tolerance = 1e-6)
})

test_that("a group collapsed to one complete observation does not blank the pooled d", {
  # 0 df + an undefined variance for one group must not poison the pooled SD to
  # NA (0 * NA): every pair still gets a finite d, matching emmeans, which also
  # gives that group 0 residual df. Pinned from emmeans (2026-07-11).
  d <- tg_f()
  d$len[d$dose == "0.5"][-1] <- NA          # dose 0.5 keeps a single value
  res <- d %>% pairwise_t_test(len ~ dose, effect.size = TRUE)
  expect_false(anyNA(res$cohens.d))
  expect_equal(res$cohens.d, c(-3.782264, -5.331933, -1.549669), tolerance = 1e-5)
})

test_that("the pooled and per-pair d genuinely differ on unbalanced data", {
  d <- both_dir_data()
  pooled  <- (d %>% pairwise_t_test(y ~ g, effect.size = TRUE))$cohens.d
  perpair <- (d %>% pairwise_t_test(y ~ g, pool.sd = FALSE, effect.size = TRUE))$cohens.d
  expect_false(isTRUE(all.equal(pooled, perpair)))
})

# ---- wilcox: Cliff's delta ------------------------------------------------
test_that("wilcox_test(effect.size = TRUE) adds cliff.delta matching cliff_delta()", {
  d <- tg_f()
  res <- d %>% wilcox_test(len ~ dose, effect.size = TRUE)
  expect_true(all(c("cliff.delta", "magnitude") %in% colnames(res)))
  expect_equal(res$cliff.delta, (d %>% cliff_delta(len ~ dose))$effsize %>% as.numeric())
})

# ---- dunn: r = Z / sqrt(N_total) ------------------------------------------
test_that("dunn_test(effect.size = TRUE) adds r = Z/sqrt(N_total), no magnitude, |r| <= 1", {
  d <- tg_f()
  res <- d %>% dunn_test(len ~ dose, effect.size = TRUE)
  expect_true("r" %in% colnames(res))
  expect_false("magnitude" %in% colnames(res))     # no calibrated threshold for Dunn r
  N <- nrow(d)                                      # total, not pairwise n1 + n2
  expect_equal(res$r, res$statistic / sqrt(N))
  expect_true(all(abs(res$r) <= 1))
})

test_that("dunn_test r inherits the sign of Z, both with and without ref.group", {
  d <- tg_f()
  res <- d %>% dunn_test(len ~ dose, ref.group = "2", effect.size = TRUE)
  expect_equal(sign(res$r), sign(res$statistic))
})

# ---- games-howell: Welch d oriented like its own estimate -----------------
test_that("games_howell_test(effect.size = TRUE) adds a Welch cohens.d oriented like estimate", {
  d <- tg_f()
  res <- d %>% games_howell_test(len ~ dose, effect.size = TRUE)
  expect_true(all(c("cohens.d", "magnitude") %in% colnames(res)))
  # The d is oriented like Games-Howell's own estimate (group2 - group1), which
  # is the OPPOSITE of t_test's group1 - group2 cohens.d for the same pair. Pin
  # the signed 0.5-vs-1 value (dose 1 > dose 0.5, so POSITIVE here) so the check
  # is a real oracle, not the tautology d = estimate / sqrt(.) makes it.
  row01 <- res$group1 == "0.5" & res$group2 == "1"
  expect_equal(res$cohens.d[row01], 2.048095842, tolerance = 1e-7)
  expect_equal((d %>% t_test(len ~ dose, effect.size = TRUE))$cohens.d[row01],
               -2.048095842, tolerance = 1e-7)   # opposite orientation
  # magnitude equals cohens_d(var.equal = FALSE) up to orientation
  cd <- d %>% cohens_d(len ~ dose, var.equal = FALSE)
  expect_equal(abs(res$cohens.d), abs(as.numeric(cd$effsize)), tolerance = 1e-9)
})

# ---- guards on unsupported combinations -----------------------------------
# NOTE: paired t (matched by id) and paired Wilcoxon are now SUPPORTED via the
# id-aware Cohen's d and the matched-pairs rank-biserial; see
# test-paired_effect_sizes.R. Only the one-sample Wilcoxon has no rank effect
# size and still rejects effect.size = TRUE.
test_that("effect.size = TRUE is still rejected for a one-sample Wilcoxon", {
  expect_error(tg_f() %>% wilcox_test(len ~ 1, mu = 20, effect.size = TRUE),
               "one-sample|two or more")
})

# ---- grouped ---------------------------------------------------------------
test_that("effect.size works with grouped data across all four functions", {
  d <- tg_f()
  expect_equal(nrow(d %>% dplyr::group_by(supp) %>% t_test(len ~ dose, effect.size = TRUE)), 6L)
  expect_equal(nrow(d %>% dplyr::group_by(supp) %>% wilcox_test(len ~ dose, effect.size = TRUE)), 6L)
  expect_equal(nrow(d %>% dplyr::group_by(supp) %>% dunn_test(len ~ dose, effect.size = TRUE)), 6L)
  expect_equal(nrow(d %>% dplyr::group_by(supp) %>% games_howell_test(len ~ dose, effect.size = TRUE)), 6L)
})
