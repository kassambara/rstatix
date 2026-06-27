context("test-get_pvalue_position")

test_that("add_y_position spaces brackets by exactly step.increase (#201)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  ys <- rstatix:::get_y_scale(df, "len", "dose", "max")
  step <- 0.12 * (ys$max - ys$min)   # fixed scales: step.increase scaled to data units
  st <- df %>% t_test(len ~ dose) %>% add_y_position(step.increase = 0.12)
  expect_equal(nrow(st), 3L)
  # consecutive brackets are step.increase apart (was step*n/(n-1) before the fix)
  expect_equal(diff(st$y.position), rep(step, 2), tolerance = 1e-6)
  # first bracket sits one step above the data max
  expect_equal(st$y.position[1], ys$max + step, tolerance = 1e-6)
})

test_that("add_y_position is unchanged for a single comparison (#201 no-regression)", {
  ys <- rstatix:::get_y_scale(ToothGrowth, "len", "supp", "max")
  step <- 0.12 * (ys$max - ys$min)
  st <- ToothGrowth %>% t_test(len ~ supp) %>% add_y_position(step.increase = 0.12)
  expect_equal(nrow(st), 1L)
  # seq(length.out = 1) returns `from`, so this value is identical old vs new
  expect_equal(st$y.position, ys$max + step, tolerance = 1e-6)
})

test_that("add_y_position ref.group = 'all' positions are unaffected (#201 no-regression)", {
  st <- ToothGrowth %>%
    t_test(len ~ dose, ref.group = "all") %>%
    add_y_position(step.increase = 0.12)
  expect_equal(nrow(st), 3L)
  expect_false(any(is.na(st$y.position)))
})
