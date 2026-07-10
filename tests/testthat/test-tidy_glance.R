context("test-tidy_glance")

test_that("tidy() turns an rstatix_test into a plain tibble", {
  res <- ToothGrowth %>% t_test(len ~ dose)
  td <- generics::tidy(res)
  # a plain tibble: the internal classes are gone
  expect_s3_class(td, "tbl_df")
  expect_false(inherits(td, "rstatix_test"))
  expect_false(inherits(td, "t_test"))
  # the stashed test arguments are dropped
  expect_null(attr(td, "args"))
  # the data is unchanged: same columns, same rows, same values
  expect_equal(colnames(td), colnames(res))
  expect_equal(nrow(td), nrow(res))
  expect_equal(td$statistic, res$statistic)
  expect_equal(td$p, res$p)
})

test_that("tidy() leaves the original object untouched", {
  res <- ToothGrowth %>% t_test(len ~ supp)
  before <- class(res)
  invisible(generics::tidy(res))
  expect_identical(class(res), before)
  expect_false(is.null(attr(res, "args")))
})

test_that("tidy() works across the test functions and keeps every row", {
  cases <- list(
    ToothGrowth %>% t_test(len ~ dose),          # 3 pairwise rows
    ToothGrowth %>% wilcox_test(len ~ dose),      # 3 pairwise rows
    ToothGrowth %>% anova_test(len ~ dose),       # 1 term
    ToothGrowth %>% kruskal_test(len ~ dose),     # 1 row
    ToothGrowth %>% dplyr::group_by(supp) %>% t_test(len ~ dose)  # grouped, 6 rows
  )
  for (res in cases) {
    td <- generics::tidy(res)
    expect_s3_class(td, "tbl_df")
    expect_false(inherits(td, "rstatix_test"))
    expect_equal(nrow(td), nrow(res))
    expect_equal(colnames(td), colnames(res))
  }
})

test_that("glance() returns a one-row summary of the test", {
  res <- ToothGrowth %>% t_test(len ~ dose)
  gl <- generics::glance(res)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1L)
  expect_equal(colnames(gl), c("method", "n"))
  expect_equal(gl$method, "t_test")
  expect_equal(gl$n, nrow(res))   # number of comparisons
})

test_that("glance() reports the right method and row count per test", {
  expect_equal(generics::glance(ToothGrowth %>% wilcox_test(len ~ dose))$method, "wilcox_test")
  expect_equal(generics::glance(ToothGrowth %>% anova_test(len ~ dose))$method, "anova_test")
  expect_equal(generics::glance(ToothGrowth %>% kruskal_test(len ~ dose))$method, "kruskal_test")
  # grouped: n counts every row across groups
  grouped <- ToothGrowth %>% dplyr::group_by(supp) %>% t_test(len ~ dose)
  expect_equal(generics::glance(grouped)$n, nrow(grouped))
})

test_that("glance() falls back to the class when no method is stashed", {
  # An rstatix_test whose args carry no method still yields a method name from
  # the object's own class, rather than NA or an error.
  res <- ToothGrowth %>% t_test(len ~ supp)
  attr(res, "args")$method <- NULL
  gl <- generics::glance(res)
  expect_equal(gl$method, "t_test")
  expect_equal(gl$n, nrow(res))
})

test_that("tidy()/glance() handle repeated-measures and mixed anova_test (list objects)", {
  # anova_test() with within=/between= returns a list, not a rectangular tibble:
  # the ANOVA table plus Mauchly's test and the sphericity corrections. tidy()
  # must return the corrected ANOVA table (one row per term), not a tibble whose
  # columns are packed data frames; glance()'s n must be the number of terms, not
  # dropped (nrow() of the list is NULL).
  set.seed(1)
  d <- data.frame(
    id = factor(rep(1:12, 3)),
    t  = factor(rep(c("t1", "t2", "t3"), each = 12)),
    g  = factor(rep(rep(c("a", "b"), each = 6), 3)),
    score = rnorm(36)
  )
  rm <- anova_test(d, dv = score, wid = id, within = t)
  mx <- anova_test(d, dv = score, wid = id, within = t, between = g)

  for (obj in list(rm, mx)) {
    td <- generics::tidy(obj)
    expect_s3_class(td, "tbl_df")
    expect_false(inherits(td, "rstatix_test"))
    # one row per term, matching the corrected ANOVA table
    tab <- get_anova_table(obj)
    expect_equal(nrow(td), nrow(tab))
    expect_true("Effect" %in% colnames(td))
    # every column is an atomic vector, never a packed data frame
    expect_true(all(vapply(td, is.atomic, logical(1))))

    gl <- generics::glance(obj)
    expect_equal(colnames(gl), c("method", "n"))   # n not dropped
    expect_equal(gl$method, "anova_test")
    expect_equal(gl$n, nrow(tab))
  }
  # the mixed design has three terms
  expect_equal(generics::glance(mx)$n, 3L)
})

test_that("tidy()/glance() handle GROUPED anova_test in every design", {
  # A grouped anova_test -- data %>% group_by(g) %>% anova_test(...) -- is a data
  # frame classed grouped_anova_test. For a repeated-measures or mixed grouped
  # design it carries a packed `anova` list-column, so a plain data-frame path
  # would return that list-column instead of the flat table; get_anova_table()
  # unpacks it to one row per group x term.
  set.seed(1)
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  d <- data.frame(
    id = factor(rep(1:12, 3)),
    t  = factor(rep(c("t1", "t2", "t3"), each = 12)),
    g  = factor(rep(rep(c("a", "b"), each = 6), 3)),
    site = factor(rep(c("x", "y"), 18)),
    y = rnorm(36)
  )
  cases <- list(
    tg %>% dplyr::group_by(supp) %>% anova_test(len ~ dose),                     # grouped between
    d  %>% dplyr::group_by(site) %>% anova_test(dv = y, wid = id, within = t),   # grouped RM
    d  %>% dplyr::group_by(site) %>% anova_test(dv = y, wid = id, within = t, between = g)  # grouped mixed
  )
  for (obj in cases) {
    td <- generics::tidy(obj)
    expect_s3_class(td, "tbl_df")
    expect_false(inherits(td, "rstatix_test"))
    # never a packed list-column: every column is atomic
    expect_true(all(vapply(td, is.atomic, logical(1))))
    tab <- get_anova_table(obj)
    expect_equal(nrow(td), nrow(tab))

    gl <- generics::glance(obj)
    expect_equal(colnames(gl), c("method", "n"))
    expect_equal(gl$method, "anova_test")   # not "grouped_anova_test"
    expect_equal(gl$n, nrow(tab))           # terms across groups, not the group count
  }
})

test_that("tidy() and glance() are dispatched through the generics", {
  # tidy()/glance() called as the bare generic (as broom / gtsummary would)
  # reach the rstatix_test methods, not broom's deprecated data-frame tidier.
  res <- ToothGrowth %>% t_test(len ~ dose)
  expect_identical(tidy(res), tidy.rstatix_test(res))
  expect_identical(glance(res), glance.rstatix_test(res))
})
