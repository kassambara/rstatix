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
