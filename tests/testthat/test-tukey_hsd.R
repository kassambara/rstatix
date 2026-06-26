context("test-tukey_hsd")

test_that("tukey_hsd forwards conf.level for ungrouped data (#188)", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  a95 <- d %>% tukey_hsd(len ~ dose, conf.level = 0.95)
  a99 <- d %>% tukey_hsd(len ~ dose, conf.level = 0.99)
  # a 99% CI must be wider than the 95% CI (lower bound smaller, upper bound larger)
  expect_true(all(a99$conf.low  < a95$conf.low))
  expect_true(all(a99$conf.high > a95$conf.high))
  # the (Tukey-adjusted) p-values do not depend on conf.level
  expect_equal(a95$p.adj, a99$p.adj)
})

test_that("tukey_hsd default conf.level (0.95) is unchanged (no regression, #188)", {
  d <- ToothGrowth; d$dose <- factor(d$dose)
  res   <- d %>% tukey_hsd(len ~ dose)                 # default
  res95 <- d %>% tukey_hsd(len ~ dose, conf.level = 0.95)
  expect_equal(res$conf.low,  res95$conf.low)
  expect_equal(res$conf.high, res95$conf.high)
  expect_equal(res$p.adj,     res95$p.adj)
})
