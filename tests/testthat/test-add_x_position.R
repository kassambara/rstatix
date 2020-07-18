context("test-add_x_position")

test_that("add_x_position works for any data frame with group1 and group2 cols", {
  stat.test <- data.frame(
    stringsAsFactors = FALSE,
    group1 = c("0.5", "0.5", "1"),
    group2 = c("1", "2", "2"),
    p = c(1.27e-07, 4.4e-14, 1.91e-05)
  ) %>%
    add_x_position()
  expect_equal(stat.test$xmin, c(1, 1, 2))
  expect_equal(stat.test$xmax, c(2, 3, 3))
})


test_that("add_x_position works for rstatix in a basic ggplot setting", {
  data("ToothGrowth")
  stat.test <- ToothGrowth %>%
    t_test(len ~ supp) %>%
    add_x_position()
  expect_equal(stat.test$xmin, 1)
  expect_equal(stat.test$xmax, 2)
})

test_that("add_x_position works for rstatix in a basic ggplot facet setting", {
  data("ToothGrowth")
  stat.test <- ToothGrowth %>%
    group_by(dose) %>%
    t_test(len ~ supp) %>%
    add_x_position(x = "supp")
  expect_equal(stat.test$xmin, c(1, 1, 1))
  expect_equal(stat.test$xmax, c(2, 2, 2))
})

test_that("add_x_position works for comparison against reference groups", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  stat.test <- df %>%
    t_test(len ~ dose, ref.group = "0.5") %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 1))
  expect_equal(stat.test$xmax, c(2, 3))
})

test_that("add_x_position works for comparison against all (basemean)", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  stat.test <- df %>%
    t_test(len ~ dose, ref.group = "all") %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 2, 3))
  expect_equal(stat.test$xmax, c(1,2, 3))
})

test_that("add_x_position works for comparison against null (one-sample test)", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  stat.test <- df %>%
    group_by(dose) %>%
    t_test(len ~ 1) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$x, c(1, 2, 3))
})

test_that("add_x_position works for grouped plots: grouping by x-var and performing test between legend groups", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  stat.test <- df %>%
    group_by(dose) %>%
    t_test(len ~ supp) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$x, c(1, 2, 3))
  expect_equal(stat.test$xmin, c(0.8, 1.8, 2.8))
  expect_equal(stat.test$xmax, c(1.2, 2.2, 3.2))
})


test_that("add_x_position works for grouped plots: grouping by legend-var and performing test between x-group", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  stat.test <- df %>%
    group_by(supp) %>%
    t_test(len ~ dose) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 1, 2, 1, 1, 2))
  expect_equal(stat.test$xmax, c(2, 3, 3, 2, 3, 3))
})
