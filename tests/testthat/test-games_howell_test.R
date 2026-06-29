context("test-games_howell_test")

# Exact data from issue #183 (6 genotypes; Geno2/4/6 have zero variance)
make_183_data <- function(){
  structure(list(
    Genotype = structure(
      c(1L,1L,1L,1L,2L,2L,2L,2L,3L,3L,3L,3L,4L,4L,4L,4L,5L,5L,5L,5L,6L,6L,6L,6L),
      levels = c("Geno1","Geno2","Geno3","Geno4","Geno5","Geno6"), class = "factor"),
    Tillering = c(22L,23L,23L,23L,22L,22L,22L,22L,23L,22L,23L,23L,
                  23L,23L,23L,23L,21L,21L,20L,21L,22L,22L,22L,22L)),
    row.names = c(NA,-24L), class = "data.frame")
}

test_that("games_howell_test does not crash on zero-variance groups (#183)", {
  df <- make_183_data()
  expect_warning(
    res <- games_howell_test(df, Tillering ~ Genotype, detailed = TRUE),
    "zero or undefined variance"
  )
  expect_equal(nrow(res), 15L)                  # all 6*5/2 comparisons present
})

test_that("games_howell_test returns NA only for both-zero-variance pairs (#183)", {
  df <- make_183_data()
  res <- suppressWarnings(games_howell_test(df, Tillering ~ Genotype, detailed = TRUE))
  zero.var <- c("Geno2", "Geno4", "Geno6")
  both_zero <- res$group1 %in% zero.var & res$group2 %in% zero.var
  # undefined pairs -> NA statistic/df/se/p.adj/conf
  expect_true(all(is.na(res$statistic[both_zero])))
  expect_true(all(is.na(res$df[both_zero])))
  expect_true(all(is.na(res$se[both_zero])))
  expect_true(all(is.na(res$p.adj[both_zero])))
  expect_true(all(is.na(res$conf.low[both_zero])))
  # every other comparison is computable (finite)
  expect_true(all(is.finite(res$statistic[!both_zero])))
  expect_true(all(is.finite(res$p.adj[!both_zero])))
})

test_that("games_howell_test handles a single-observation (n=1) group without crashing (#183)", {
  # groups A and B each have n = 1 (var = NA). A df-only alignment still crashed
  # here (`se must be size 10 or 1, ...`) for mixed group sizes; aligning the
  # Welch sd too fixes it.
  df <- data.frame(
    val = c(5,  9,  1,2,3,  6,7,8,  4,5,6),
    grp = rep(c("A","B","C","D","E"), times = c(1,1,3,3,3))
  )
  expect_warning(
    res <- games_howell_test(df, val ~ grp, detailed = TRUE),
    "undefined variance"
  )
  expect_equal(nrow(res), 10L)                       # 5*4/2 comparisons, none dropped
  involves_n1 <- res$group1 %in% c("A","B") | res$group2 %in% c("A","B")
  expect_true(all(is.na(res$statistic[involves_n1])))
  expect_true(all(is.na(res$se[involves_n1])))
  expect_true(all(is.finite(res$statistic[!involves_n1])))   # C/D/E pairs compute
})

test_that("games_howell_test with a single zero-variance group works (#183)", {
  # only group A is constant -> no pair has TWO zero-variance groups
  df <- data.frame(
    val = c(5,5,5,5,  1,2,3,4,  6,7,8,10),
    grp = rep(c("A","B","C"), each = 4)
  )
  res <- games_howell_test(df, val ~ grp, detailed = TRUE)
  expect_equal(nrow(res), 3L)
  expect_true(all(is.finite(res$p.adj)))       # no undefined pair
})

test_that("games_howell_test on valid data is unchanged (#183 no-regression)", {
  res <- ToothGrowth %>% games_howell_test(len ~ dose, detailed = TRUE)
  expect_equal(nrow(res), 3L)
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "estimate",
      "conf.low", "conf.high", "se", "statistic", "df", "p.adj", "p.adj.signif", "method")
  )
  expect_true(all(is.finite(res$p.adj)))
})
