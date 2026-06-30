context("test-ref-group-missing")

# #153: a missing reference group used to surface as a cryptic stats::relevel()
# error ("'<ref>' must be an existing level"). It should now give a clear,
# actionable message that points to the group_by()+filter() idiom.

test_that("missing ref.group gives a clear, actionable error (ungrouped) (#153)", {
  d <- data.frame(g = rep(c("x", "y"), each = 4), y = c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_error(d %>% t_test(y ~ g, ref.group = "ref"), "reference group")
  expect_error(d %>% t_test(y ~ g, ref.group = "ref"), "not present in the data")
  # the available levels are listed to guide the user
  expect_error(d %>% t_test(y ~ g, ref.group = "ref"), "x, y")
})

test_that("missing ref.group in a grouped subset errors clearly (#153)", {
  d <- data.frame(
    blk = c(rep("A", 6), rep("B", 4)),
    g   = c("ref", "ref", "x", "x", "y", "y", "x", "x", "y", "y"),  # B has no 'ref'
    y   = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  expect_error(
    d %>% dplyr::group_by(blk) %>% t_test(y ~ g, ref.group = "ref"),
    "reference group"
  )
})

test_that("the group_by()+filter() idiom works after dropping ref-less groups (#153)", {
  d <- data.frame(
    blk = c(rep("A", 6), rep("B", 4)),
    g   = c("ref", "ref", "x", "x", "y", "y", "x", "x", "y", "y"),
    y   = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  res <- d %>%
    dplyr::group_by(blk) %>%
    dplyr::filter(any(g == "ref")) %>%
    t_test(y ~ g, ref.group = "ref")
  expect_true(all(res$blk == "A"))          # only the block that has 'ref'
  expect_true(all(res$group1 == "ref"))
})

test_that("a valid ref.group is unaffected (no-regression) (#153)", {
  tg <- ToothGrowth; tg$dose <- factor(tg$dose)
  res <- tg %>% t_test(len ~ dose, ref.group = "0.5")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "0.5"))
  # ref.group = "all" still works too
  res_all <- tg %>% t_test(len ~ dose, ref.group = "all")
  expect_equal(nrow(res_all), 3L)
  expect_true(all(res_all$group1 == "all"))
})

test_that("missing ref.group is also clear for wilcox_test (#153)", {
  d <- data.frame(g = rep(c("x", "y"), each = 4), y = c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_error(d %>% wilcox_test(y ~ g, ref.group = "ref"), "reference group")
})

test_that("missing ref.group raises a classed condition for reliable detection (#153)", {
  # The condition carries class 'rstatix_missing_ref_group' so downstream
  # callers (e.g. ggpubr::geom_pwc) can detect it by class, not by matching the
  # translatable message text.
  d <- data.frame(g = rep(c("x", "y"), each = 4), y = c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_error(d %>% t_test(y ~ g, ref.group = "ref"),
               class = "rstatix_missing_ref_group")
  # the class survives the grouped (doo/purrr) path too
  d2 <- data.frame(
    blk = c(rep("A", 6), rep("B", 4)),
    g   = c("ref", "ref", "x", "x", "y", "y", "x", "x", "y", "y"),
    y   = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  err <- tryCatch(
    d2 %>% dplyr::group_by(blk) %>% t_test(y ~ g, ref.group = "ref"),
    error = function(e) e
  )
  # in the grouped path the condition is wrapped by dplyr/purrr, so the class
  # lives in the parent chain; rlang::cnd_inherits() walks it.
  expect_true(rlang::cnd_inherits(err, "rstatix_missing_ref_group"))
})
