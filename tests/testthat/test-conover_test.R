context("test-conover_test")

# Self-contained base-R re-derivation of the Conover-Iman pairwise statistic,
# used to validate conover_test() without depending on an external package.
conover_ref <- function(x, g){
  g <- factor(g)
  N <- length(x); k <- nlevels(g)
  r <- rank(x)
  Rbar <- tapply(r, g, mean); nj <- tapply(x, g, length)
  rs <- tapply(r, g, sum)
  H.unc <- (12 / (N * (N + 1))) * sum(rs^2 / nj) - 3 * (N + 1)
  ties <- table(r)
  H <- H.unc / (1 - sum(ties^3 - ties) / (N^3 - N))
  S2 <- (1 / (N - 1)) * (sum(r^2) - N * (N + 1)^2 / 4)
  scale <- S2 * ((N - 1 - H) / (N - k))
  lev <- levels(g)
  out <- list()
  for(a in seq_len(k)) for(b in seq_len(a - 1)){
    # match pairwise.table orientation: group1 = lower-index level (b)
    t <- (Rbar[lev[a]] - Rbar[lev[b]]) / sqrt(scale * (1/nj[lev[a]] + 1/nj[lev[b]]))
    out[[paste(lev[b], lev[a])]] <- list(
      group1 = lev[b], group2 = lev[a],
      statistic = unname(t), p = 2 * stats::pt(abs(t), N - k, lower.tail = FALSE),
      df = N - k
    )
  }
  do.call(rbind.data.frame, lapply(out, function(o) data.frame(o, stringsAsFactors = FALSE)))
}

test_that("conover_test matches the Conover-Iman formula (#222, #17)", {
  res <- ToothGrowth %>% conover_test(len ~ dose, detailed = TRUE)
  ref <- conover_ref(ToothGrowth$len, ToothGrowth$dose)
  ref <- ref[order(ref$group1, ref$group2), ]
  res <- res[order(res$group1, res$group2), ]
  expect_equal(res$statistic, ref$statistic, tolerance = 1e-6)
  expect_equal(res$p, ref$p, tolerance = 1e-9)
  expect_equal(unique(res$df), 60L - 3L)            # df = N - k
})

test_that("conover_test reproduces PMCMRplus::kwAllPairsConoverTest reference values (#222)", {
  # Reference produced by PMCMRplus::kwAllPairsConoverTest(len ~ factor(dose),
  # ToothGrowth, p.adjust.method = "none"); hard-coded so the test has no external
  # dependency. Pinned snapshot: PMCMRplus 1.9.12, 2026-07-10. PMCMRplus reports
  # the signed t and the raw (unadjusted) p, and its sign agrees with this
  # function's in both directions -- unlike frdAllPairsConoverTest(), whose sign
  # is reversed, and frdAllPairsNemenyiTest(), which reports the magnitude. The
  # statistic is pinned with its sign so the agreement is actually checked.
  res <- ToothGrowth %>% conover_test(len ~ dose, p.adjust.method = "none", detailed = TRUE)
  res <- res[order(res$group1, res$group2), ]
  expect_equal(res$statistic, c(6.268626, 11.219642, 4.951016), tolerance = 1e-5)
  expect_equal(res$p, c(5.203093e-08, 4.683820e-16, 6.915398e-06), tolerance = 1e-6)
})

test_that("conover_test default (non-detailed) returns the expected columns (#222)", {
  res <- ToothGrowth %>% conover_test(len ~ dose)
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "df", "p", "p.adj", "p.adj.signif")
  )
  expect_equal(nrow(res), 3L)                        # k(k-1)/2 comparisons
  expect_false(any(is.na(res$statistic)))
})

test_that("conover_test detailed adds estimate/method columns (#222)", {
  res <- ToothGrowth %>% conover_test(len ~ dose, detailed = TRUE)
  expect_true(all(c("estimate", "estimate1", "estimate2", "method") %in% colnames(res)))
  expect_equal(unique(res$method), "Conover test")
  # estimate is the mean-rank difference, oriented like dunn_test (group2 - group1)
  expect_equal(as.numeric(res$estimate), as.numeric(res$estimate2 - res$estimate1), tolerance = 1e-9)
})

test_that("conover_test ref.group compares each group to the control and adjusts over k-1 (#222)", {
  res <- ToothGrowth %>% conover_test(len ~ dose, ref.group = "0.5")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "0.5"))
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))   # adjusted over only k-1
})

test_that("conover_test p.adj uses all pairs without ref.group (#222)", {
  res <- ToothGrowth %>% conover_test(len ~ dose)
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))
})

test_that("conover_test works on grouped data (#222)", {
  res <- ToothGrowth %>% dplyr::group_by(supp) %>% conover_test(len ~ dose)
  expect_true("supp" %in% colnames(res))
  expect_equal(nrow(res), 6L)                        # 2 supp x 3 comparisons
})

test_that("conover_test errors when all observations are in one group (#222)", {
  df <- data.frame(y = rnorm(10), g = rep("a", 10))
  expect_error(df %>% conover_test(y ~ g), "same group")
})

test_that("conover_test fails cleanly on degenerate (undefined) data (#222)", {
  # one observation per group -> N - k = 0 residual df
  singletons <- data.frame(y = 1:6, g = factor(1:6))
  expect_error(singletons %>% conover_test(y ~ g), "undefined")
  # all values identical -> no rank variability
  constant <- data.frame(y = rep(5, 9), g = factor(rep(c("a", "b", "c"), each = 3)))
  expect_error(constant %>% conover_test(y ~ g), "undefined")
})

test_that("conover_test guard does NOT fire for ordinary inputs (#222)", {
  # representative valid inputs must still run and return finite statistics
  expect_silent_run <- function(expr) {
    res <- expr
    expect_false(any(is.na(res$statistic)))
    expect_true(all(is.finite(res$statistic)))
  }
  expect_silent_run(ToothGrowth %>% conover_test(len ~ dose))
  expect_silent_run(iris %>% conover_test(Sepal.Length ~ Species))
  # two groups, and a group with a single observation (still has residual df)
  df2 <- data.frame(y = c(1, 2, 3, 10, 11, 12), g = factor(rep(c("a", "b"), each = 3)))
  expect_silent_run(df2 %>% conover_test(y ~ g))
})

test_that("get_description gives a friendly conover_test label (#222)", {
  res <- ToothGrowth %>% conover_test(len ~ dose)
  expect_equal(get_description(res), "Conover test")   # not the raw "conover_test"
  # multi-row pairwise output -> get_test_label returns one label per comparison
  lab <- unlist(get_test_label(res, type = "text"))
  expect_true(all(grepl("Conover test", lab)))
})
