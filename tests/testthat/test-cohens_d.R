context("test-cohens_d")

oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]

test_that("cohens_d applies mu for an independent two-sample test (#200)", {
  d0 <- ToothGrowth %>% cohens_d(len ~ supp)
  d3 <- ToothGrowth %>% cohens_d(len ~ supp, mu = 3)
  expect_false(isTRUE(all.equal(d0$effsize, d3$effsize)))            # mu now changes the result
  expected <- ((mean(oj) - mean(vc)) - 3) / sqrt((var(oj) + var(vc)) / 2)
  expect_equal(as.numeric(d3$effsize), expected, tolerance = 1e-4)
})

test_that("cohens_d applies mu for a paired two-sample test (#200)", {
  d0 <- ToothGrowth %>% cohens_d(len ~ supp, paired = TRUE)
  d3 <- ToothGrowth %>% cohens_d(len ~ supp, paired = TRUE, mu = 3)
  expect_false(isTRUE(all.equal(d0$effsize, d3$effsize)))
  diffs <- oj - vc
  expect_equal(as.numeric(d3$effsize), (mean(diffs) - 3) / sd(diffs), tolerance = 1e-4)
})

test_that("cohens_d default (mu = 0) is the standard Cohen's d (no regression, #200)", {
  d_ind <- ToothGrowth %>% cohens_d(len ~ supp)
  expect_equal(as.numeric(d_ind$effsize),
               (mean(oj) - mean(vc)) / sqrt((var(oj) + var(vc)) / 2), tolerance = 1e-4)
  diffs <- oj - vc
  d_pr <- ToothGrowth %>% cohens_d(len ~ supp, paired = TRUE)
  expect_equal(as.numeric(d_pr$effsize), mean(diffs) / sd(diffs), tolerance = 1e-4)
  # one-sample behaviour is unchanged (mu already worked there)
  os <- ToothGrowth %>% cohens_d(len ~ 1, mu = 20)
  expect_equal(as.numeric(os$effsize), (mean(ToothGrowth$len) - 20) / sd(ToothGrowth$len), tolerance = 1e-4)
})

test_that("cohens_d still accepts abbreviated argument names (no regression)", {
  # The bootstrap arguments are named boot.parallel / boot.ncpus (not parallel /
  # ncpus) so that `p`/`pa` keep partial-matching unambiguously to `paired`, and
  # `n` to `nboot`.
  diffs <- oj - vc
  expect_equal(as.numeric(cohens_d(ToothGrowth, len ~ supp, p = TRUE)$effsize),
               mean(diffs) / sd(diffs), tolerance = 1e-4)
  expect_equal(as.numeric(cohens_d(ToothGrowth, len ~ supp, pa = TRUE)$effsize),
               mean(diffs) / sd(diffs), tolerance = 1e-4)
  skip_if_not_installed("boot")
  set.seed(42)
  res <- cohens_d(ToothGrowth, len ~ supp, ci = TRUE, n = 200)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(res)))
})
