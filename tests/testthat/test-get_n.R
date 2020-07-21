context("test-get_n")

test_that("Checking that get_n works for T-test", {
  data("ToothGrowth")
  stat.test <- ToothGrowth %>% t_test(len ~ dose)
  expect_equal(get_n(stat.test), c(40, 40, 40))
})

test_that("Checking that get_n works for grouped T-test", {
  data("ToothGrowth")
  stat.test <- ToothGrowth %>%
    group_by(dose) %>%
    t_test(len ~ supp)
  expect_equal(get_n(stat.test), c(20, 20, 20))
})

test_that("Checking that get_n works for grouped ANOVA", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>%
    group_by(supp) %>%
    anova_test(len ~ dose)
  expect_equal(get_n(res.aov), c(30, 30))
})
