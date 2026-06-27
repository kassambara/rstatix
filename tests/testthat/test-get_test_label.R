context("test-get_test_label")

test_that("get_test_label includes the sample size n for ANOVA (#150)", {
  one_way <- get_test_label(ToothGrowth %>% anova_test(len ~ dose),
                            detailed = TRUE, type = "text")
  expect_match(one_way, "n = 60")
  # two-way: total sample size
  two_way <- get_test_label(ToothGrowth %>% anova_test(len ~ supp * dose),
                            detailed = TRUE, type = "text")
  expect_match(two_way, "n = 60")
  # repeated measures: n = number of subjects (wid)
  set.seed(1)
  rm <- data.frame(id = factor(rep(1:10, 3)),
                   time = factor(rep(c("t1", "t2", "t3"), each = 10)),
                   score = rnorm(30))
  rm_label <- get_test_label(anova_test(rm, dv = score, wid = id, within = time),
                             detailed = TRUE, type = "text")
  expect_match(rm_label, "n = 10")
  # expression type still builds without error
  expect_silent(get_test_label(ToothGrowth %>% anova_test(len ~ dose), detailed = TRUE))
})

test_that("get_test_label is unchanged for non-ANOVA tests and non-detailed labels (#150)", {
  expect_match(get_test_label(ToothGrowth %>% t_test(len ~ supp), detailed = TRUE, type = "text"), "n = 60")
  expect_match(get_test_label(ToothGrowth %>% kruskal_test(len ~ dose), detailed = TRUE, type = "text"), "n = 60")
  # the non-detailed ANOVA label has no n (only the description + p)
  expect_false(grepl("n =", get_test_label(ToothGrowth %>% anova_test(len ~ dose), type = "text")))
})
