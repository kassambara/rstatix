context("test-freq_table")

tg <- function(){ d <- ToothGrowth; d$dose <- factor(d$dose); d }

test_that("freq_table() works on ungrouped data (no-regression) (#191)", {
  res <- tg() %>% freq_table(supp, dose)
  expect_equal(colnames(res), c("supp", "dose", "n", "prop"))
  expect_equal(nrow(res), 6L)
  expect_true(all(res$n == 10))
  # proportions sum to 100% within each supp (all-but-last grouping)
  sums <- tapply(res$prop, res$supp, sum)
  expect_true(all(abs(sums - 100) < 0.5))
})

test_that("freq_table() handles grouped data, returning a tidy data frame (#191)", {
  res <- tg() %>% group_by(supp) %>% freq_table(dose)
  # tidy data frame (NOT a list), grouping column kept
  expect_s3_class(res, "data.frame")
  expect_false(is.list(res) && !is.data.frame(res))
  expect_true(all(c("supp", "dose", "n", "prop") %in% colnames(res)))
  expect_equal(nrow(res), 6L)
  # equals the equivalent ungrouped call freq_table(supp, dose)
  ref <- tg() %>% freq_table(supp, dose)
  expect_equal(
    as.data.frame(res)[order(res$supp, res$dose), c("supp", "dose", "n", "prop")],
    as.data.frame(ref)[order(ref$supp, ref$dose), c("supp", "dose", "n", "prop")],
    ignore_attr = TRUE
  )
  # proportions sum to ~100% within each group
  sums <- tapply(res$prop, res$supp, sum)
  expect_true(all(abs(sums - 100) < 0.5))
})

test_that("freq_table() grouped by two variables works (#191)", {
  d <- tg(); d$cap <- factor(rep(c("a", "b"), 30))
  res <- d %>% group_by(supp, cap) %>% freq_table(dose)
  expect_true(all(c("supp", "cap", "dose", "n", "prop") %in% colnames(res)))
  # proportions sum to ~100% within each (supp, cap) cell
  sums <- tapply(res$prop, interaction(res$supp, res$cap), sum)
  expect_true(all(abs(sums - 100) < 1))
})
