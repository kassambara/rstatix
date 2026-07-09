context("test-bootstrap_ci")

# A stand-in for boot::boot() that records how it was called and then aborts with
# a distinctively classed condition, so the assertions never depend on a
# translated error message (base R's match.arg() text is localized).
mock_boot <- function(record) {
  function(data, statistic, R, ..., parallel, ncpus) {
    record$parallel <- parallel
    record$ncpus <- ncpus
    stop(structure(
      class = c("mock_boot_called", "error", "condition"),
      list(message = "mock boot::boot() called", call = NULL)
    ))
  }
}

stat_func <- function(data, i) mean(data$x[i])
boot_data <- data.frame(x = 1:10)

test_that("get_boot_ci forwards parallel and ncpus to boot::boot()", {
  skip_if_not_installed("boot")
  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")

  expect_error(
    get_boot_ci(boot_data, stat_func, nboot = 10, parallel = "multicore", ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "multicore")
  expect_equal(record$ncpus, 3)
})

test_that("get_boot_ci honours options(boot.parallel=) and options(boot.ncpus=)", {
  # Regression test: boot::boot() resolves getOption("boot.parallel") only when
  # `parallel` is missing. Passing a hard-coded "no" silently disabled the option.
  skip_if_not_installed("boot")
  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")

  old <- options(boot.parallel = "multicore", boot.ncpus = 2L)
  on.exit(options(old), add = TRUE)

  expect_error(
    get_boot_ci(boot_data, stat_func, nboot = 10),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "multicore")
  expect_equal(record$ncpus, 2L)
})

test_that("an explicit parallel argument overrides options(boot.parallel=)", {
  skip_if_not_installed("boot")
  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")

  old <- options(boot.parallel = "multicore", boot.ncpus = 2L)
  on.exit(options(old), add = TRUE)

  expect_error(
    get_boot_ci(boot_data, stat_func, nboot = 10, parallel = "no", ncpus = 1L),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "no")
  expect_equal(record$ncpus, 1L)
})

test_that("get_boot_ci defaults to a serial bootstrap", {
  skip_if_not_installed("boot")
  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")

  old <- options(boot.parallel = NULL, boot.ncpus = NULL)
  on.exit(options(old), add = TRUE)

  expect_error(
    get_boot_ci(boot_data, stat_func, nboot = 10),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "no")
  expect_equal(record$ncpus, 1L)
})

test_that("the effect size functions forward boot.parallel and boot.ncpus", {
  skip_if_not_installed("boot")
  df <- ToothGrowth
  df$dose <- factor(df$dose)

  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")
  expect_error(
    cohens_d(df, len ~ supp, ci = TRUE, nboot = 10,
             boot.parallel = "multicore", boot.ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "multicore")
  expect_equal(record$ncpus, 3)

  record2 <- new.env()
  local_mocked_bindings(boot = mock_boot(record2), .package = "boot")
  expect_error(
    kruskal_effsize(df, len ~ dose, ci = TRUE, nboot = 10,
                    boot.parallel = "multicore", boot.ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record2$parallel, "multicore")
  expect_equal(record2$ncpus, 3)
})

test_that("the arguments placed after `...` also reach boot::boot()", {
  # friedman_effsize() and wilcox_effsize() take boot.parallel/boot.ncpus after
  # `...`, so they must be matched exactly rather than swallowed by the dots.
  skip_if_not_installed("boot")

  df <- data.frame(
    id = factor(rep(1:8, each = 3)),
    time = factor(rep(c("t1", "t2", "t3"), times = 8)),
    score = c(12, 15, 18, 11, 14, 20, 13, 17, 19, 10, 16, 21,
              14, 13, 22, 15, 12, 17, 11, 18, 20, 16, 14, 23)
  )
  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")
  expect_error(
    friedman_effsize(df, score ~ time | id, ci = TRUE, nboot = 10,
                     boot.parallel = "multicore", boot.ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "multicore")
  expect_equal(record$ncpus, 3)

  skip_if_not_installed("coin")
  record2 <- new.env()
  local_mocked_bindings(boot = mock_boot(record2), .package = "boot")
  expect_error(
    wilcox_effsize(ToothGrowth, len ~ supp, ci = TRUE, nboot = 10,
                   boot.parallel = "multicore", boot.ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record2$parallel, "multicore")
  expect_equal(record2$ncpus, 3)
})

test_that("grouped data forward the bootstrap arguments for every group", {
  skip_if_not_installed("boot")
  df <- ToothGrowth
  df$dose <- factor(df$dose)

  record <- new.env()
  local_mocked_bindings(boot = mock_boot(record), .package = "boot")
  expect_error(
    kruskal_effsize(dplyr::group_by(df, supp), len ~ dose, ci = TRUE, nboot = 10,
                    boot.parallel = "multicore", boot.ncpus = 3),
    class = "mock_boot_called"
  )
  expect_equal(record$parallel, "multicore")
  expect_equal(record$ncpus, 3)
})

test_that("boot.parallel and boot.ncpus are not stored in the test arguments", {
  # attr(x, "args") records the statistical call (ggpubr reads it back via
  # get_test_arguments()). The bootstrap-execution arguments cannot change any
  # returned value, and their defaults depend on the user's options(), so
  # including them would make the attribute vary between sessions.
  df <- ToothGrowth
  df$dose <- factor(df$dose)

  expect_false(any(c("boot.parallel", "boot.ncpus") %in%
                     names(attr(cohens_d(df, len ~ supp), "args"))))
  expect_false(any(c("boot.parallel", "boot.ncpus") %in%
                     names(attr(kruskal_effsize(df, len ~ dose), "args"))))
})

test_that("the default bootstrap confidence interval is unchanged (no regression)", {
  # Values pinned from the behaviour before the parallel arguments were added, so
  # that a change to the bootstrap plumbing cannot move them silently.
  skip_if_not_installed("boot")
  df <- ToothGrowth

  set.seed(42)
  res <- cohens_d(df, len ~ supp, ci = TRUE, nboot = 200)
  expect_equal(res$conf.low, -0.06)
  expect_equal(res$conf.high, 1.2)

  set.seed(42)
  res2 <- cohens_d(df, len ~ supp, ci = TRUE, nboot = 200, ci.type = "basic")
  expect_equal(res2$conf.low, -0.21)
  expect_equal(res2$conf.high, 1.05)
})

# Degenerate bootstrap replicates (#290) -------------------------------------

# Perfectly concordant rankings give Kendall's W == 1, so every bootstrap
# replicate equals 1 and no interval exists.
concordant_df <- function() {
  data.frame(
    id = factor(rep(1:4, each = 3)),
    time = factor(rep(c("t1", "t2", "t3"), times = 4)),
    score = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  )
}

# Constant data: the statistic is NaN, so every replicate is NA.
constant_df <- function() {
  data.frame(g = factor(rep(c("a", "b"), each = 6)), v = rep(1, 12))
}

test_that("get_boot_ci returns NA bounds and warns on degenerate replicates (#290)", {
  skip_if_not_installed("boot")
  d <- data.frame(x = 1:10)

  set.seed(1)
  expect_warning(
    ci_identical <- get_boot_ci(d, function(data, i) 1, nboot = 50),
    "could not be computed"
  )
  expect_equal(ci_identical, c(NA_real_, NA_real_))
  expect_type(ci_identical, "double")

  set.seed(1)
  expect_warning(
    ci_missing <- get_boot_ci(d, function(data, i) NA_real_, nboot = 50),
    "could not be computed"
  )
  expect_equal(ci_missing, c(NA_real_, NA_real_))

  # a well-behaved bootstrap is untouched
  set.seed(1)
  ci_ok <- get_boot_ci(d, function(data, i) mean(data$x[i]), nboot = 100)
  expect_type(ci_ok, "double")
  expect_length(ci_ok, 2)
  expect_false(anyNA(ci_ok))
})

test_that("friedman_effsize returns NA bounds instead of list-columns (#290)", {
  skip_if_not_installed("boot")
  set.seed(1)
  expect_warning(
    res <- friedman_effsize(concordant_df(), score ~ time | id, ci = TRUE, nboot = 50),
    "could not be computed"
  )
  # the bug returned a <list> column of NULLs
  expect_type(res$conf.low, "double")
  expect_type(res$conf.high, "double")
  expect_true(is.na(res$conf.low))
  expect_true(is.na(res$conf.high))
  expect_equal(as.numeric(res$effsize), 1)
})

test_that("grouped friedman_effsize no longer errors when one group is degenerate (#290)", {
  skip_if_not_installed("boot")
  df <- data.frame(
    id = factor(rep(1:8, each = 3)),
    time = factor(rep(c("t1", "t2", "t3"), times = 8)),
    grp = factor(rep(c("A", "B"), each = 12)),
    score = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
              1, 3, 2, 2, 1, 3, 1, 2, 3, 3, 1, 2)
  )
  set.seed(1)
  expect_warning(
    res <- friedman_effsize(dplyr::group_by(df, grp), score ~ time | id,
                            ci = TRUE, nboot = 50),
    "could not be computed"
  )
  expect_equal(nrow(res), 2L)
  expect_type(res$conf.low, "double")
  expect_true(is.na(res$conf.low[1]))   # group A: W == 1, undefined
  expect_false(is.na(res$conf.low[2]))  # group B: computed as usual
})

test_that("the effect size functions return NA bounds on constant data (#290)", {
  skip_if_not_installed("boot")
  const <- constant_df()

  set.seed(1)
  expect_warning(d <- cohens_d(const, v ~ g, ci = TRUE, nboot = 50), "could not be computed")
  expect_type(d$conf.low, "double")
  expect_true(is.na(d$conf.low))

  set.seed(1)
  expect_warning(k <- kruskal_effsize(const, v ~ g, ci = TRUE, nboot = 50), "could not be computed")
  expect_type(k$conf.low, "double")
  expect_true(is.na(k$conf.low))

  skip_if_not_installed("coin")
  set.seed(1)
  expect_warning(w <- wilcox_effsize(const, v ~ g, ci = TRUE, nboot = 50), "could not be computed")
  expect_type(w$conf.low, "double")
  expect_true(is.na(w$conf.low))
})

test_that("an interval type boot.ci cannot build yields NA bounds, not a list-column (#290)", {
  skip_if_not_installed("boot")
  # "stud" needs bootstrap variances, which are not supplied
  set.seed(1)
  expect_warning(
    res <- cohens_d(ToothGrowth, len ~ supp, ci = TRUE, nboot = 50, ci.type = "stud"),
    "could not be computed"
  )
  expect_type(res$conf.low, "double")
  expect_true(is.na(res$conf.low))
})

test_that("a well-behaved bootstrap CI still warns about nothing (#290 no-regression)", {
  skip_if_not_installed("boot")
  set.seed(42)
  expect_silent(res <- cohens_d(ToothGrowth, len ~ supp, ci = TRUE, nboot = 200))
  expect_equal(res$conf.low, -0.06)
  expect_equal(res$conf.high, 1.2)
})

test_that("an unknown ci.type errors rather than returning NA bounds (#290)", {
  skip_if_not_installed("boot")
  # boot.ci() ignores a type it does not know, so a typo would otherwise slip
  # through the "interval could not be computed" guard and return NA silently.
  expect_error(
    cohens_d(ToothGrowth, len ~ supp, ci = TRUE, nboot = 50, ci.type = "bogus"),
    "ci.type"
  )
  expect_error(
    get_boot_ci(data.frame(x = 1:10), function(d, i) mean(d$x[i]),
                nboot = 10, type = "percentile"),
    "ci.type"
  )
  # the documented types keep working
  set.seed(42)
  for (ty in c("norm", "basic", "perc", "bca")) {
    res <- cohens_d(ToothGrowth, len ~ supp, ci = TRUE, nboot = 200, ci.type = ty)
    expect_type(res$conf.low, "double")
    expect_false(is.na(res$conf.low))
  }
})
