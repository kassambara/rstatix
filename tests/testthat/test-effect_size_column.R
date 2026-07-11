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
  # sign of d equals sign of the reported mean difference (never contradicts it)
  expect_equal(sign(res$cohens.d), sign(res$estimate))
  # magnitude equals cohens_d(var.equal = FALSE) up to orientation
  cd <- d %>% cohens_d(len ~ dose, var.equal = FALSE)
  expect_equal(abs(res$cohens.d), abs(as.numeric(cd$effsize)), tolerance = 1e-9)
})

# ---- guards on unsupported combinations -----------------------------------
test_that("effect.size = TRUE is forbidden where the metric would be wrong", {
  d <- data.frame(
    id = factor(rep(1:10, 2)),
    g  = factor(rep(c("a", "b"), each = 10)),
    y  = c(1, 3, 2, 5, 4, 7, 6, 9, 8, 11,  8, 6, 9, 7, 12, 10, 13, 11, 15, 14)
  )
  # paired t-test with id has no id-aware cohens_d
  expect_error(d %>% t_test(y ~ g, paired = TRUE, id = "id", effect.size = TRUE),
               "id")
  # paired Wilcoxon: Cliff's delta is independent-samples only
  expect_error(d %>% wilcox_test(y ~ g, paired = TRUE, effect.size = TRUE),
               "paired")
  # one-sample Wilcoxon: Cliff's delta undefined
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
