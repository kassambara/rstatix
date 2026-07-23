context("test-paired_effect_sizes")

# Paired data whose second-group rows are in REVERSED id order, so pairing by row
# order (the naive default) matches the WRONG subjects and pairing by id matches
# the right ones. Deterministic, no RNG.
paired_shuffled <- function() {
  data.frame(
    id = factor(c(1, 2, 3, 4, 5, 6,  6, 5, 4, 3, 2, 1)),
    g  = factor(rep(c("a", "b"), each = 6)),
    y  = c(10, 12, 11, 13, 9, 14,   20, 12, 11, 18, 14, 8)
  )
}
# subject-matched values: a = 10,12,11,13,9,14 ; b = 8,14,18,11,12,20

test_that("cohens_d() gains an id argument that matches pairs by subject", {
  d <- paired_shuffled()
  a <- c(10, 12, 11, 13, 9, 14); b <- c(8, 14, 18, 11, 12, 20)
  diffs <- a - b
  id.aligned <- d %>% cohens_d(y ~ g, paired = TRUE, id = "id")
  expect_equal(as.numeric(id.aligned$effsize), mean(diffs) / sd(diffs), tolerance = 1e-7)
  # the naive row-order paired d matches different subjects and differs
  row.order <- d %>% cohens_d(y ~ g, paired = TRUE)
  expect_false(isTRUE(all.equal(as.numeric(id.aligned$effsize),
                                as.numeric(row.order$effsize))))
  # id is stashed only when supplied (default output unchanged)
  expect_false("id" %in% names(attr(d %>% cohens_d(y ~ g), "args")))
})

test_that("t_test(paired, id, effect.size = TRUE) reports the id-aligned cohens.d", {
  d <- paired_shuffled()
  res <- d %>% t_test(y ~ g, paired = TRUE, id = "id", effect.size = TRUE)
  expect_true(all(c("cohens.d", "magnitude") %in% colnames(res)))
  expect_equal(res$cohens.d, -0.609271796, tolerance = 1e-7)
  expect_equal(res$cohens.d,
               as.numeric((d %>% cohens_d(y ~ g, paired = TRUE, id = "id"))$effsize),
               tolerance = 1e-9)
})

test_that("wilcox_test(paired, id, effect.size = TRUE) reports the matched-pairs rank-biserial", {
  # rank.biserial = (R+ - R-)/(R+ + R-) on the signed ranks of the id-aligned
  # paired differences, == effectsize::rank_biserial(paired = TRUE) (2026-07-11).
  d <- paired_shuffled()
  res <- d %>% wilcox_test(y ~ g, paired = TRUE, id = "id", effect.size = TRUE)
  expect_true("rank.biserial" %in% colnames(res))
  expect_false("magnitude" %in% colnames(res))       # no calibrated threshold
  expect_false("cliff.delta" %in% colnames(res))
  # group1 = a, group2 = b, so the sign is that of the a - b differences
  expect_equal(res$rank.biserial, -0.619047619, tolerance = 1e-7)
})

test_that("paired rank-biserial without id uses the row-order pairs, matching effectsize", {
  # ToothGrowth len ~ supp as a paired design (row order = subject order here).
  # Pinned from effectsize::rank_biserial(OJ, VC, paired = TRUE) = 0.6091954.
  res <- ToothGrowth %>% wilcox_test(len ~ supp, paired = TRUE, effect.size = TRUE)
  expect_equal(res$rank.biserial, 0.6091954, tolerance = 1e-6)
})

test_that("the rank-biserial drops zero differences, as the Wilcoxon test does", {
  d <- data.frame(
    id = factor(rep(1:5, 2)),
    g  = factor(rep(c("a", "b"), each = 5)),
    y  = c(1, 2, 3, 4, 5,   1, 4, 3, 2, 9)   # subj1 & subj3 have zero difference
  )
  res <- d %>% wilcox_test(y ~ g, paired = TRUE, id = "id", effect.size = TRUE)
  # only the 3 non-zero-difference pairs are ranked: diffs 0,-2,0,+2,-4
  dd <- c(-2, 2, -4); rr <- rank(abs(dd))
  expected <- (sum(rr[dd > 0]) - sum(rr[dd < 0])) / (sum(rr[dd > 0]) + sum(rr[dd < 0]))
  expect_equal(res$rank.biserial, expected, tolerance = 1e-9)
})

test_that("paired effect sizes work for a pairwise (>2 group) design, matching per pair", {
  set.seed(2)
  d3 <- data.frame(
    id = factor(rep(1:10, 3)),
    t  = factor(rep(c("t1", "t2", "t3"), each = 10)),
    y  = c(1:10, c(3, 1, 5, 2, 7, 4, 9, 6, 11, 8), c(2, 4, 1, 6, 3, 8, 5, 10, 7, 12))
  )
  rt <- d3 %>% t_test(y ~ t, paired = TRUE, id = "id", effect.size = TRUE)
  rw <- d3 %>% wilcox_test(y ~ t, paired = TRUE, id = "id", effect.size = TRUE)
  expect_equal(nrow(rt), 3L)
  expect_equal(nrow(rw), 3L)
  # cohens.d equals the standalone id-aware cohens_d, row for row
  expect_equal(rt$cohens.d,
               as.numeric((d3 %>% cohens_d(y ~ t, paired = TRUE, id = "id"))$effsize),
               tolerance = 1e-9)
})

test_that("the unpaired Wilcoxon effect size is unchanged (cliff.delta + magnitude)", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  res <- d %>% wilcox_test(len ~ dose, effect.size = TRUE)
  expect_true(all(c("cliff.delta", "magnitude") %in% colnames(res)))
  expect_false("rank.biserial" %in% colnames(res))
  expect_equal(res$cliff.delta, (d %>% cliff_delta(len ~ dose))$effsize %>% as.numeric())
})

test_that("a one-sample Wilcoxon still rejects effect.size = TRUE", {
  expect_error(ToothGrowth %>% wilcox_test(len ~ 1, mu = 20, effect.size = TRUE),
               "one-sample|two or more")
})

test_that("an all-zero paired difference set gives NA rank-biserial with a warning", {
  # (R+ - R-)/(R+ + R-) is 0/0 with nothing to rank: NA + warning, same
  # contract as the undefined bootstrap CI (#290), instead of an error.
  z <- data.frame(t = rep(c("a", "b"), each = 4), v = rep(1:4, 2))
  expect_warning(
    res <- wilcox_test(z, v ~ t, paired = TRUE, effect.size = TRUE),
    "undefined"
  )
  expect_true(is.na(res$rank.biserial))
  expect_equal(nrow(res), 1L)
  # a pairwise call still computes the other comparisons
  z3 <- data.frame(t = rep(c("a", "b", "c"), each = 4), v = c(1:4, 1:4, 5:8))
  expect_warning(
    res3 <- wilcox_test(z3, v ~ t, paired = TRUE, effect.size = TRUE,
                        error.as.na = TRUE),
    "undefined"
  )
  expect_equal(res3$rank.biserial, c(NA, -1, -1))
  # wilcox_effsize's rank_biserial path follows the same contract
  expect_warning(
    rese <- wilcox_effsize(z, v ~ t, paired = TRUE, method = "rank_biserial"),
    "undefined"
  )
  expect_true(is.na(rese$effsize))
})
