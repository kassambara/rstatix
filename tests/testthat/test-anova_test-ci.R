context("test-anova_test-ci")

# #18: opt-in confidence intervals for partial eta-squared, computed in base R
# from the noncentral F distribution, matching effectsize::eta_squared().

tg <- function(){ d <- ToothGrowth; d$dose <- factor(d$dose); d }

test_that("ci = NULL (default) leaves the output unchanged (no-regression) (#18)", {
  d <- tg()
  res <- d %>% anova_test(len ~ supp * dose)
  expect_false(any(c("conf.low", "conf.high") %in% colnames(res)))
  res_pes <- d %>% anova_test(len ~ supp * dose, effect.size = "pes")
  expect_false(any(c("conf.low", "conf.high") %in% colnames(res_pes)))
})

test_that("ci adds conf.low/conf.high bracketing pes (#18)", {
  d <- tg()
  res <- d %>% anova_test(len ~ supp * dose, effect.size = "pes", ci = 0.95)
  expect_true(all(c("pes", "conf.low", "conf.high") %in% colnames(res)))
  expect_true(all(res$conf.low <= res$pes + 1e-8))
  expect_true(all(res$pes <= res$conf.high + 1e-8))
  expect_true(all(res$conf.low >= 0 & res$conf.high <= 1))
})

test_that("ci reproduces the known effectsize partial-eta-squared interval (#18)", {
  # Expected bounds validated during development against
  # effectsize::eta_squared(model, partial = TRUE, ci = 0.95,
  # alternative = "two.sided") on aov(len ~ supp * dose). Hard-coded here so the
  # test needs no dependency on effectsize (which is not imported/suggested).
  d <- tg()
  res <- d %>% anova_test(len ~ supp * dose, effect.size = "pes", ci = 0.95)
  res <- res[order(res$Effect), ]
  expect_equal(res$conf.low[res$Effect == "supp"],      0.059, tolerance = 0.002)
  expect_equal(res$conf.high[res$Effect == "supp"],     0.402, tolerance = 0.002)
  expect_equal(res$conf.low[res$Effect == "dose"],      0.662, tolerance = 0.002)
  expect_equal(res$conf.high[res$Effect == "dose"],     0.838, tolerance = 0.002)
  expect_equal(res$conf.low[res$Effect == "supp:dose"], 0.001, tolerance = 0.002)
  expect_equal(res$conf.high[res$Effect == "supp:dose"],0.295, tolerance = 0.002)
})

test_that("ci works for one-way and repeated-measures designs (#18)", {
  d <- tg()
  ow <- d %>% anova_test(len ~ dose, effect.size = "pes", ci = 0.95)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(ow)))
  set.seed(1)
  dr <- data.frame(
    id = factor(rep(1:12, 3)),
    time = factor(rep(c("t1", "t2", "t3"), each = 12)),
    score = c(rnorm(12, 5), rnorm(12, 6), rnorm(12, 8))
  )
  rm <- dr %>% anova_test(dv = score, wid = id, within = time, effect.size = "pes", ci = 0.95)
  rm <- get_anova_table(rm)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(rm)))
  expect_true(all(rm$conf.low <= rm$pes & rm$pes <= rm$conf.high))
})

test_that("ci requires effect.size to include 'pes' and a valid level (#18)", {
  d <- tg()
  expect_error(d %>% anova_test(len ~ supp * dose, ci = 0.95), "partial eta-squared")
  expect_error(d %>% anova_test(len ~ supp * dose, effect.size = "pes", ci = 95),
               "between 0 and 1")
  expect_error(d %>% anova_test(len ~ supp * dose, effect.size = "pes", ci = -1),
               "between 0 and 1")
})

test_that("a narrower confidence level gives a narrower interval (#18)", {
  d <- tg()
  r90 <- d %>% anova_test(len ~ dose, effect.size = "pes", ci = 0.90)
  r99 <- d %>% anova_test(len ~ dose, effect.size = "pes", ci = 0.99)
  expect_true((r90$conf.high - r90$conf.low) < (r99$conf.high - r99$conf.low))
})
