context("test-anova_summary")


test_that("Checking anova summary table has pes and ges columns",{
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  res.aov <- Anova(lm(len ~ dose*supp, data = df))
  aov_summary <- anova_summary(
    res.aov,
    detailed = TRUE,
    effect.size = c("pes", "ges")
  )

  expect_equal(aov_summary$pes, c(0.773, 0.224, 0.132))
  expect_equal(aov_summary$ges, c(0.773, 0.224, 0.132))
})

test_that("Checking anova summary table has pes column",{
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  res.aov <- Anova(lm(len ~ dose*supp, data = df))
  aov_summary <- anova_summary(
    res.aov,
    detailed = TRUE,
    effect.size = "pes"
  )

  expect_equal(aov_summary$pes, c(0.773, 0.224, 0.132))
  expect_equal(aov_summary$ges, NULL)
})

test_that("Checking anova summary table has pes column",{
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  res.aov <- Anova(lm(len ~ dose*supp, data = df))
  aov_summary <- anova_summary(
    res.aov,
    detailed = TRUE,
    effect.size = "ges"
  )

  expect_equal(aov_summary$pes, NULL)
  expect_equal(aov_summary$ges, c(0.773, 0.224, 0.132))
})