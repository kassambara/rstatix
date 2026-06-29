context("test-dunn_test")

test_that("dunn_test ref.group keeps only comparisons against the reference (#101)", {
  res <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")
  expect_equal(nrow(res), 2L)                       # k - 1 = 2 comparisons
  expect_true(all(res$group1 == "0.5"))            # reference is group1
  expect_setequal(res$group2, c("1", "2"))
})

test_that("dunn_test ref.group leaves raw p and statistic unchanged (#101)", {
  full <- ToothGrowth %>% dunn_test(len ~ dose)
  ref  <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")
  # in the full result the 0.5 pairs are already oriented as group1 = "0.5"
  full_ref <- full[full$group1 == "0.5", ]
  full_ref <- full_ref[order(full_ref$group2), ]
  ref <- ref[order(ref$group2), ]
  expect_equal(ref$p, full_ref$p)                   # raw p identical
  expect_equal(ref$statistic, full_ref$statistic)  # z identical (same orientation here)
})

test_that("dunn_test ref.group adjusts over k-1, not all pairs (#101)", {
  ref <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")
  # adjustment must be computed over the 2 retained comparisons only
  expect_equal(ref$p.adj, p.adjust(ref$p, method = "holm"))
  # and that differs from adjusting over all 3 pairwise comparisons (over-correction)
  full <- ToothGrowth %>% dunn_test(len ~ dose)
  posthoc <- full[full$group1 == "0.5", ]
  expect_false(isTRUE(all.equal(sort(ref$p.adj), sort(posthoc$p.adj))))
})

test_that("dunn_test ref.group re-orients reference as group1 and flips sign (#101)", {
  full <- ToothGrowth %>% dunn_test(len ~ dose, detailed = TRUE)
  ref2 <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "2", detailed = TRUE)
  expect_true(all(ref2$group1 == "2"))             # reference oriented as group1
  f <- full[full$group1 == "0.5" & full$group2 == "2", ]
  r <- ref2[ref2$group1 == "2" & ref2$group2 == "0.5", ]
  expect_equal(r$statistic, -f$statistic)          # z is direction-dependent -> negated
  expect_equal(r$estimate,  -f$estimate)           # mean-rank diff -> negated
  expect_equal(r$estimate1, f$estimate2)           # mean ranks swap
  expect_equal(r$estimate2, f$estimate1)
  expect_equal(r$p, f$p)                            # two-sided raw p unchanged
})

test_that("dunn_test invalid ref.group gives an informative error (#101)", {
  expect_error(
    ToothGrowth %>% dunn_test(len ~ dose, ref.group = "99"),
    "not a level of the grouping variable"
  )
})

test_that("dunn_test without ref.group is unchanged (#101 no-regression)", {
  res <- ToothGrowth %>% dunn_test(len ~ dose)
  expect_equal(nrow(res), 3L)                       # all k(k-1)/2 = 3 comparisons
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "p", "p.adj", "p.adj.signif")
  )
  # p.adj computed over all 3 comparisons
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))
})
