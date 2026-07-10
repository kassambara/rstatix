context("test-friedman_nemenyi_test")

# A fixed, non-degenerate balanced complete block design (3 treatments, 6 blocks).
demo_df <- data.frame(
  id        = factor(rep(1:6, 3)),
  treatment = factor(rep(c("A", "B", "C"), each = 6)),
  score     = c(4, 6, 3, 5, 4, 5,   7, 8, 6, 7, 9, 6,   6, 9, 7, 8, 8, 9)
)

# Self-contained base-R re-derivation of the Nemenyi pairwise statistic and p.
nemenyi_ref <- function(data, dv, within, wid){
  data[[within]] <- factor(data[[within]])
  trt <- levels(data[[within]])
  m <- tapply(data[[dv]], list(data[[wid]], factor(data[[within]], levels = trt)),
              function(z) z[1])
  m <- m[, trt, drop = FALSE]
  b <- nrow(m); k <- ncol(m)
  R <- t(apply(m, 1, rank)); Rj <- colSums(R); names(Rj) <- trt
  denom <- sqrt(b * k * (k + 1) / 12)
  out <- list()
  for(a in seq_len(k)) for(bb in seq_len(a - 1)){
    q <- (Rj[trt[a]] - Rj[trt[bb]]) / denom
    out[[paste(trt[bb], trt[a])]] <- data.frame(
      group1 = trt[bb], group2 = trt[a], statistic = unname(q),
      p.adj = stats::ptukey(abs(q), nmeans = k, df = Inf, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

test_that("friedman_nemenyi_test matches the Nemenyi formula (#141)", {
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id, detailed = TRUE)
  ref <- nemenyi_ref(demo_df, "score", "treatment", "id")
  res <- res[order(res$group1, res$group2), ]
  ref <- ref[order(ref$group1, ref$group2), ]
  expect_equal(res$statistic, ref$statistic, tolerance = 1e-6)
  expect_equal(res$p.adj, ref$p.adj, tolerance = 1e-9)
})

test_that("friedman_nemenyi_test reproduces PMCMRplus::frdAllPairsNemenyiTest values (#141)", {
  # Reference produced by PMCMRplus::frdAllPairsNemenyiTest(...); hard-coded so
  # the test has no external dependency. Pinned snapshot: PMCMRplus 1.9.12,
  # 2026-07-10. PMCMRplus reports the magnitude of the statistic. This function
  # reports it with the sign of the rank-sum difference, which is positive for
  # every pair of demo_df (A < B < C), so the two coincide here. Reverse the
  # groups and the signs flip; the block below checks that.
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id, detailed = TRUE)
  res <- res[order(res$group1, res$group2), ]
  expect_equal(res$statistic, c(3.2659863, 4.0824829, 0.8164966), tolerance = 1e-5)
  expect_equal(res$p.adj, c(0.05450063, 0.01086516, 0.83222922), tolerance = 1e-6)
})

test_that("friedman_nemenyi_test reports the statistic with the sign of the rank-sum difference", {
  # Same scores with A and C exchanged: every rank-sum difference reverses, so
  # every statistic reverses while its magnitude and p-value are unchanged. This
  # is where the returned value parts company with PMCMRplus, which reports only
  # the magnitude.
  reversed <- demo_df
  reversed$score <- c(6, 9, 7, 8, 8, 9,   7, 8, 6, 7, 9, 6,   4, 6, 3, 5, 4, 5)
  res <- reversed %>% friedman_nemenyi_test(score ~ treatment | id, detailed = TRUE)
  res <- res[order(res$group1, res$group2), ]
  expect_equal(res$statistic, c(-0.8164966, -4.0824829, -3.2659863), tolerance = 1e-5)
  expect_equal(res$p.adj, c(0.83222922, 0.01086516, 0.05450063), tolerance = 1e-6)
})

test_that("friedman_nemenyi_test default returns the expected columns (tukey-style p.adj) (#141)", {
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id)
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "p.adj", "p.adj.signif")
  )
  expect_false("p" %in% colnames(res))                 # studentized range -> only p.adj
  expect_equal(nrow(res), 3L)
  expect_equal(unique(c(res$n1, res$n2)), 6L)
})

test_that("friedman_nemenyi_test detailed adds estimate/method columns (#141)", {
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id, detailed = TRUE)
  expect_true(all(c("estimate", "estimate1", "estimate2", "method") %in% colnames(res)))
  expect_equal(unique(res$method), "Nemenyi test")
  expect_equal(as.numeric(res$estimate), as.numeric(res$estimate2 - res$estimate1), tolerance = 1e-9)
})

test_that("friedman_nemenyi_test works on grouped data (#141)", {
  df2 <- rbind(
    cbind(demo_df, cohort = "g1"),
    cbind(transform(demo_df, score = score + c(0, 1, -1, 2, 0, 1)), cohort = "g2")
  )
  res <- df2 %>% dplyr::group_by(cohort) %>% friedman_nemenyi_test(score ~ treatment | id)
  expect_true("cohort" %in% colnames(res))
  expect_equal(nrow(res), 6L)                          # 2 cohorts x 3 comparisons
})

test_that("friedman_nemenyi_test rejects missing outcome values (#141)", {
  na_df <- demo_df
  na_df$score[1] <- NA
  expect_error(na_df %>% friedman_nemenyi_test(score ~ treatment | id), "missing values")
})

test_that("friedman_nemenyi_test errors on an incomplete/unbalanced design (#141)", {
  expect_error(demo_df[-1, ] %>% friedman_nemenyi_test(score ~ treatment | id), "complete block")
})

test_that("friedman_nemenyi_test requires at least two blocks (#141)", {
  one_block <- data.frame(
    id = factor(c(1, 1, 1)), treatment = factor(c("A", "B", "C")), score = c(3, 5, 8)
  )
  expect_error(one_block %>% friedman_nemenyi_test(score ~ treatment | id), "two blocks")
  # the valid design (6 blocks) is unaffected
  expect_silent(suppressWarnings(demo_df %>% friedman_nemenyi_test(score ~ treatment | id)))
})

test_that("friedman_nemenyi_test output works with add_xy_position (ggpubr brackets) (#141)", {
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id)
  pos <- res %>% add_xy_position(x = "treatment")
  expect_true(all(c("xmin", "xmax", "y.position") %in% colnames(pos)))
  expect_false(any(is.na(pos$y.position)))
})

test_that("get_description gives a friendly friedman_nemenyi_test label (#141)", {
  res <- demo_df %>% friedman_nemenyi_test(score ~ treatment | id)
  expect_equal(get_description(res), "Nemenyi test")
  lab <- unlist(get_test_label(res, type = "text"))
  expect_true(all(grepl("Nemenyi test", lab)))
})
