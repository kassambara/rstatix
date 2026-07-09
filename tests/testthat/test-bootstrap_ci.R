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
