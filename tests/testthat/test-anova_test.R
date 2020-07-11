context("test-anova_test")

test_that("Checking one-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>% anova_test(len ~ dose)
  expect_equal(res.aov$Effect, "dose")
  expect_equal(res.aov$DFn, 1)
  expect_equal(res.aov$DFd, 58)
  expect_equal(res.aov$F, 105.065)
  expect_equal(res.aov$ges, 0.644)
})

test_that("Checking grouped one-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>%
    group_by(supp) %>%
    anova_test(len ~ dose)
  expect_equal(res.aov$Effect, c("dose", "dose"))
  expect_equal(res.aov$DFn, c(1, 1))
  expect_equal(res.aov$DFd, c(28, 28))
  expect_equal(res.aov$F, c(36.013, 117.948))
  expect_equal(res.aov$ges, c(0.563, 0.808))
})


test_that("Checking two-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>% anova_test(len ~ supp*dose)
  expect_equal(res.aov$Effect, c("supp", "dose", "supp:dose"))
  expect_equal(res.aov$DFn, c(1, 1, 1))
  expect_equal(res.aov$DFd, c(56, 56, 56))
  expect_equal(res.aov$F, c(12.317, 133.415, 5.333))
  expect_equal(res.aov$p, c(8.94e-04, 1.91e-16, 2.50e-02))
  expect_equal(res.aov$ges, c(0.180, 0.704, 0.087))
})


test_that("Checking repeated measures ANOVA test", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
  anova.table <- res.aov$ANOVA
  sphericity <- res.aov$`Mauchly's Test for Sphericity`
  corrections <- res.aov$`Sphericity Corrections`
  expect_equal(anova.table$Effect, c("supp", "dose", "supp:dose"))
  expect_equal(anova.table$DFn, c(1, 2, 2))
  expect_equal(anova.table$DFd, c(9, 18, 18))
  expect_equal(anova.table$F, c(34.866, 106.470, 2.534))
  expect_equal(anova.table$p, c(2.28e-04, 1.06e-10, 1.07e-01))
  expect_equal(anova.table$ges, c(0.224, 0.773, 0.132))
  expect_equal(sphericity$W, c(0.807, 0.934))
  expect_equal(corrections$GGe, c(0.838, 0.938))
  expect_equal(corrections$HFe, c(1.008, 1.176))
})


test_that("Checking that get_anova_table works with any data frame", {
  data("ToothGrowth")
  expect_is(get_anova_table(ToothGrowth), "data.frame")
})

test_that("Checking that get_anova_table works for grouped repeated measures ANOVA", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>%
    group_by(supp) %>%
    anova_test(dv = len, wid = id, within = dose)
  aov.table <- get_anova_table(res.aov)
  expect_equal(aov.table$F, c(23.936, 57.783))
})


test_that("Checking that get_anova_table performs auto sphericity correction", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
  res.aov2 <- res.aov
  res.aov2$`Mauchly's Test for Sphericity`$p[1] <- 0.05 # significant
  # Correction not applied, because there is not significant sphericity test
  auto <- get_anova_table(res.aov, correction = "auto")
  expect_equal(auto$DFn, c(1, 2, 2))
  expect_equal(auto$DFd, c(9, 18, 18))
  expect_equal(auto$F, c(34.866, 106.470, 2.534))
  # Correction automatically applied to the DF of the effect where sphericity is signiica,t
  auto2 <- get_anova_table(res.aov2, correction = "auto")
  expect_equal(auto2$DFn, c(1, 1.68, 2))
  expect_equal(auto2$DFd, c(9, 15.09, 18))
  expect_equal(auto2$F, c(34.866, 106.470, 2.534))
  # Check that GG correction works for all within-subject variables
  gg <- get_anova_table(res.aov2, correction = "GG")
  expect_equal(gg$DFn, c(1, 1.68, 1.88))
  expect_equal(gg$DFd, c(9, 15.09, 16.88))
  expect_equal(gg$p, c(2.28e-04, 2.79e-09, 1.12e-01))
})
