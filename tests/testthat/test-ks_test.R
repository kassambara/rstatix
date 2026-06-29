context("test-ks_test")

test_that("ks_test two-sample matches stats::ks.test (#92, #168)", {
  res <- ToothGrowth %>% ks_test(len ~ supp, detailed = TRUE)
  oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
  vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]
  ref <- suppressWarnings(stats::ks.test(oj, vc))
  expect_equal(nrow(res), 1L)
  expect_equal(res$group1, "OJ")
  expect_equal(res$group2, "VC")
  expect_equal(res$n1, 30); expect_equal(res$n2, 30)
  expect_equal(unname(res$statistic), unname(ref$statistic))   # D
  expect_equal(res$p, ref$p.value)
})

test_that("ks_test default (non-detailed) keeps n1, n2 and statistic, like wilcox_test (#92)", {
  res <- ToothGrowth %>% ks_test(len ~ supp)
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "p")
  )
  expect_equal(res$n1, 30); expect_equal(res$n2, 30)
  expect_false(is.na(res$statistic))
})

test_that("get_test_label/get_description give a friendly ks_test label (#92)", {
  res <- ToothGrowth %>% ks_test(len ~ supp)
  expect_equal(get_description(res), "Kolmogorov-Smirnov test")   # not the raw "ks_test"
  lab <- get_test_label(res, type = "text")
  expect_true(is.character(lab) && grepl("Kolmogorov-Smirnov test", lab))
})

test_that("ks_test performs pairwise comparisons for >2 groups (#92)", {
  res <- ToothGrowth %>% ks_test(len ~ dose)
  expect_equal(nrow(res), 3L)
  expect_true(all(c("group1", "group2", "p", "p.adj", "p.adj.signif") %in% colnames(res)))
  # p.adj is computed over the 3 comparisons (holm)
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))
})

test_that("ks_test ref.group compares each group to the control (#92)", {
  res <- ToothGrowth %>% ks_test(len ~ dose, ref.group = "0.5")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "0.5"))
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))   # adjusted over k-1
})

test_that("ks_test detailed returns the statistic and method (#92)", {
  res <- ToothGrowth %>% ks_test(len ~ supp, detailed = TRUE)
  expect_true(all(c("statistic", "method", "alternative") %in% colnames(res)))
  expect_match(unique(res$method), "Kolmogorov-Smirnov")
})

test_that("ks_test works on grouped data (#92)", {
  res <- ToothGrowth %>% dplyr::group_by(dose) %>% ks_test(len ~ supp)
  expect_true("dose" %in% colnames(res))
  expect_equal(nrow(res), 3L)               # one OJ-vs-VC test per dose
})

test_that("ks_test errors clearly without a grouping variable (#92)", {
  expect_error(ToothGrowth %>% ks_test(len ~ 1), "at least two levels")
})
