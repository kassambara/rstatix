context("test-mcnemar_test")

# Demo data: 3 related treatments, binary outcome (from ?mcnemar_test)
make_mcnemar_data <- function(){
  mydata <- data.frame(
    outcome = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
    treatment = gl(3, 1, 30, labels = LETTERS[1:3]),
    participant = gl(10, 3, labels = letters[1:10])
  )
  mydata$outcome <- factor(
    mydata$outcome, levels = c(1, 0), labels = c("success", "failure")
  )
  mydata
}

test_that("pairwise_mcnemar_test returns statistic and df for type='mcnemar' (#122)", {
  res <- pairwise_mcnemar_test(make_mcnemar_data(), outcome ~ treatment | participant)
  expect_true(all(c("statistic", "df") %in% colnames(res)))
  expect_equal(
    colnames(res),
    c("group1", "group2", "statistic", "df", "p", "p.adj", "p.adj.signif", "method")
  )
  expect_equal(nrow(res), 3L)                 # 3 pairwise comparisons of 3 groups
  expect_true(all(res$df == 1))               # McNemar chi-squared on a 2x2 table
})

test_that("pairwise_mcnemar_test statistic/df match per-pair mcnemar_test (#122)", {
  mydata <- make_mcnemar_data()
  res <- pairwise_mcnemar_test(mydata, outcome ~ treatment | participant)
  wide <- tidyr::spread(
    dplyr::mutate(mydata, outcome = as.factor(outcome)),
    key = treatment, value = outcome
  )
  manual_pair <- function(g1, g2){
    xt <- stats::xtabs(stats::as.formula(paste0("~", g1, "+", g2)), wide)
    mcnemar_test(xt)
  }
  ab <- manual_pair("A", "B")
  row <- res[res$group1 == "A" & res$group2 == "B", ]
  expect_equal(row$statistic, ab$statistic)
  expect_equal(row$df, ab$df)
  expect_equal(row$p, ab$p)
})

test_that("pairwise_mcnemar_test type='exact' has no statistic/df (#122 no-regression)", {
  res <- pairwise_mcnemar_test(
    make_mcnemar_data(), outcome ~ treatment | participant, type = "exact"
  )
  expect_false(any(c("statistic", "df") %in% colnames(res)))
  expect_equal(
    colnames(res),
    c("group1", "group2", "p", "p.adj", "p.adj.signif", "method")
  )
  expect_equal(unique(res$method), "Exact binomial test")
})

test_that("mcnemar_test (single, 2x2) is unchanged (#122 no-regression)", {
  xtab <- as.table(rbind(c(25, 6), c(21, 10)))
  res <- mcnemar_test(xtab)
  expect_equal(
    colnames(res),
    c("n", "statistic", "df", "p", "p.signif", "method")
  )
  expect_equal(res$n, 62)
  expect_equal(unname(res$df), 1)
  expect_equal(unname(res$p), stats::mcnemar.test(xtab)$p.value)
})
