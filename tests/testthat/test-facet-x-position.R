context("test-facet-x-position")

# Faceted test: facet X has x-levels a,b; facet Y has c,d,e.
facet_df <- function(){
  data.frame(
    y = 1:10,
    x = c("a", "a", "b", "b", "c", "c", "d", "d", "e", "e"),
    group = c("X", "X", "X", "X", "Y", "Y", "Y", "Y", "Y", "Y")
  )
}

test_that("scales = 'free' gives per-facet x positions (#203)", {
  st <- t_test(dplyr::group_by(facet_df(), group), y ~ x) %>%
    add_xy_position(x = "x", scales = "free")
  # facet X: a-b -> 1,2
  x <- st[st$group == "X", ]
  expect_equal(x$xmin, 1); expect_equal(x$xmax, 2)
  # facet Y: c,d,e -> positions 1,2,3 within the facet (not the global 3,4,5)
  y <- st[st$group == "Y", ]
  expect_equal(y$xmin[y$group1 == "c" & y$group2 == "d"], 1)
  expect_equal(y$xmax[y$group1 == "c" & y$group2 == "d"], 2)
  expect_equal(y$xmin[y$group1 == "c" & y$group2 == "e"], 1)
  expect_equal(y$xmax[y$group1 == "c" & y$group2 == "e"], 3)
  expect_equal(y$xmin[y$group1 == "d" & y$group2 == "e"], 2)
  expect_equal(y$xmax[y$group1 == "d" & y$group2 == "e"], 3)
})

test_that("default scales = 'fixed' keeps global x positions (no-regression) (#203)", {
  st <- t_test(dplyr::group_by(facet_df(), group), y ~ x) %>%
    add_xy_position(x = "x")                       # default fixed
  y <- st[st$group == "Y", ]
  # global positions: c,d,e -> 3,4,5
  expect_equal(y$xmin[y$group1 == "c" & y$group2 == "d"], 3)
  expect_equal(y$xmax[y$group1 == "d" & y$group2 == "e"], 5)
})

test_that("scales = 'free_y' does NOT remap x (only y is freed) (#203)", {
  st_fixed <- t_test(dplyr::group_by(facet_df(), group), y ~ x) %>%
    add_xy_position(x = "x")
  st_freey <- t_test(dplyr::group_by(facet_df(), group), y ~ x) %>%
    add_xy_position(x = "x", scales = "free_y")
  expect_equal(st_freey$xmin, st_fixed$xmin)        # x positions unchanged
  expect_equal(st_freey$xmax, st_fixed$xmax)
})

test_that("scales = 'free' does not affect a non-faceted (ungrouped) test (#203)", {
  tg <- ToothGrowth; tg$dose <- factor(tg$dose)
  st_fixed <- tg %>% t_test(len ~ dose) %>% add_xy_position(x = "dose")
  st_free  <- tg %>% t_test(len ~ dose) %>% add_xy_position(x = "dose", scales = "free")
  expect_equal(st_free$xmin, st_fixed$xmin)         # no facet vars -> no remap
  expect_equal(st_free$xmax, st_fixed$xmax)
})

test_that("free-scale remap counts plotted-but-uncompared x levels (#203)", {
  # All three levels a,b,c are plotted in BOTH facets, but only a-vs-c is
  # compared, so b is a plotted box that no comparison touches. The panel x
  # positions must come from the DATA (a,b,c -> 1,2,3), so a-vs-c is 1 -> 3,
  # NOT 1 -> 2 (which a comparison-endpoint remap would wrongly produce).
  df <- data.frame(
    y = c(1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 15),
    x = rep(c("a", "a", "b", "b", "c", "c"), 2),
    group = rep(c("X", "Y"), each = 6)
  )
  st <- t_test(dplyr::group_by(df, group), y ~ x, comparisons = list(c("a", "c"))) %>%
    add_xy_position(x = "x", scales = "free")
  expect_true(all(st$xmin == 1))
  expect_true(all(st$xmax == 3))
})

test_that("free-scale remap shifts a shared comparison by each facet's own levels (#203)", {
  # facet X has only b,c; facet Y has a,b,c. The SAME comparison b-vs-c must land
  # at 1->2 in X but 2->3 in Y (because a occupies panel position 1 in Y). This
  # only holds when the remap is driven by each facet's actual data levels.
  df <- data.frame(
    y = c(1, 2, 3, 4, 10, 11, 12, 13, 14, 15),
    x = c("b", "b", "c", "c", "a", "a", "b", "b", "c", "c"),
    group = c(rep("X", 4), rep("Y", 6))
  )
  st <- t_test(dplyr::group_by(df, group), y ~ x, comparisons = list(c("b", "c"))) %>%
    add_xy_position(x = "x", scales = "free")
  X <- st[st$group == "X", ]
  Y <- st[st$group == "Y", ]
  expect_equal(X$xmin, 1); expect_equal(X$xmax, 2)
  expect_equal(Y$xmin, 2); expect_equal(Y$xmax, 3)
})

test_that("free-scale remap handles non-consecutive global x levels per facet (#203)", {
  # facet Y is missing level 'b' so its present levels are a,c,d (global 1,3,4)
  df <- data.frame(
    y = 1:12,
    x = c("a","a","b","b","c","c", "a","a","c","c","d","d"),
    group = rep(c("X","Y"), each = 6)
  )
  st <- t_test(dplyr::group_by(df, group), y ~ x) %>%
    add_xy_position(x = "x", scales = "free")
  y <- st[st$group == "Y", ]
  # present levels a,c,d -> compressed to 1,2,3 (not 1,3,4)
  expect_setequal(unique(c(y$xmin, y$xmax)), c(1, 2, 3))
})
