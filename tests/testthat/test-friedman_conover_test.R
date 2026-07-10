context("test-friedman_conover_test")

# A fixed, non-degenerate balanced complete block design (3 treatments, 6 blocks).
demo_df <- data.frame(
  id        = factor(rep(1:6, 3)),
  treatment = factor(rep(c("A", "B", "C"), each = 6)),
  score     = c(4, 6, 3, 5, 4, 5,   7, 8, 6, 7, 9, 6,   6, 9, 7, 8, 8, 9)
)

# Self-contained base-R re-derivation of the Durbin-Conover pairwise statistic,
# to validate without depending on an external package.
durbin_conover_ref <- function(data, dv, within, wid){
  data[[within]] <- factor(data[[within]])
  trt <- levels(data[[within]])
  m <- tapply(data[[dv]], list(data[[wid]], factor(data[[within]], levels = trt)),
              function(z) z[1])
  m <- m[, trt, drop = FALSE]
  b <- nrow(m); k <- ncol(m)
  R <- t(apply(m, 1, rank)); Rj <- colSums(R); names(Rj) <- trt
  df <- (b - 1) * (k - 1)
  denom <- sqrt(2 * (b * sum(R^2) - sum(Rj^2)) / df)
  out <- list()
  for(a in seq_len(k)) for(bb in seq_len(a - 1)){
    t <- (Rj[trt[a]] - Rj[trt[bb]]) / denom
    out[[paste(trt[bb], trt[a])]] <- data.frame(
      group1 = trt[bb], group2 = trt[a], statistic = unname(t),
      p = 2 * stats::pt(abs(t), df, lower.tail = FALSE), df = df,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

test_that("friedman_conover_test matches the Durbin-Conover formula (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id, detailed = TRUE)
  ref <- durbin_conover_ref(demo_df, "score", "treatment", "id")
  res <- res[order(res$group1, res$group2), ]
  ref <- ref[order(ref$group1, ref$group2), ]
  expect_equal(res$statistic, ref$statistic, tolerance = 1e-6)
  expect_equal(res$p, ref$p, tolerance = 1e-9)
  expect_equal(unique(res$df), (6 - 1) * (3 - 1))      # (b-1)(k-1)
})

test_that("friedman_conover_test reproduces PMCMRplus::frdAllPairsConoverTest values (#8)", {
  # Reference produced by PMCMRplus::frdAllPairsConoverTest(...); hard-coded so the
  # test has no external dependency. Pinned snapshot: PMCMRplus 1.9.12, 2026-07-10.
  # PMCMRplus reports the t statistic for the reversed comparison, so its values
  # are the negatives of these: -4.472136, -5.590170, -1.118034. The p-values are
  # identical.
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id, p.adjust.method = "none", detailed = TRUE)
  res <- res[order(res$group1, res$group2), ]
  expect_equal(res$statistic, c(4.472136, 5.590170, 1.118034), tolerance = 1e-5)
  expect_equal(res$p, c(0.0011934670, 0.0002308192, 0.2896916), tolerance = 1e-6)
})

test_that("friedman_conover_test default (non-detailed) returns the expected columns (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id)
  expect_equal(
    colnames(res),
    c(".y.", "group1", "group2", "n1", "n2", "statistic", "df", "p", "p.adj", "p.adj.signif")
  )
  expect_equal(nrow(res), 3L)                          # k(k-1)/2 comparisons
  expect_equal(unique(c(res$n1, res$n2)), 6L)          # number of blocks
})

test_that("friedman_conover_test detailed adds estimate/method columns (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id, detailed = TRUE)
  expect_true(all(c("estimate", "estimate1", "estimate2", "method") %in% colnames(res)))
  expect_equal(unique(res$method), "Durbin-Conover")
  # estimate is the rank-sum difference, oriented as group2 - group1
  expect_equal(as.numeric(res$estimate), as.numeric(res$estimate2 - res$estimate1), tolerance = 1e-9)
})

test_that("friedman_conover_test ref.group adjusts over only k-1 comparisons (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id, ref.group = "A")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$group1 == "A"))
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))
})

test_that("friedman_conover_test all-pairs p.adj uses all comparisons (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id)
  expect_equal(res$p.adj, p.adjust(res$p, method = "holm"))
})

test_that("friedman_conover_test works on grouped data (#8)", {
  df2 <- rbind(
    cbind(demo_df, cohort = "g1"),
    cbind(transform(demo_df, score = score + c(0, 1, -1, 2, 0, 1)), cohort = "g2")
  )
  res <- df2 %>% dplyr::group_by(cohort) %>% friedman_conover_test(score ~ treatment | id)
  expect_true("cohort" %in% colnames(res))
  expect_equal(nrow(res), 6L)                          # 2 cohorts x 3 comparisons
})

test_that("friedman_conover_test errors on an incomplete/unbalanced block design (#8)", {
  inc <- demo_df[-1, ]                                  # drop one cell -> unbalanced
  expect_error(inc %>% friedman_conover_test(score ~ treatment | id), "complete block")
})

test_that("friedman_conover_test errors when the design has no rank variability (#8)", {
  # every subject ranks the treatments in the same order -> denominator is zero
  degen <- data.frame(
    id        = factor(rep(1:4, each = 3)),
    treatment = factor(rep(c("A", "B", "C"), 4)),
    score     = rep(c(1, 2, 3), 4)
  )
  expect_error(degen %>% friedman_conover_test(score ~ treatment | id), "undefined")
})

test_that("friedman_conover_test errors for an invalid ref.group (#8)", {
  expect_error(
    demo_df %>% friedman_conover_test(score ~ treatment | id, ref.group = "Z"),
    "not a level"
  )
})

test_that("friedman_conover_test rejects missing outcome values (#8)", {
  na_df <- demo_df
  na_df$score[1] <- NA
  expect_error(
    na_df %>% friedman_conover_test(score ~ treatment | id),
    "missing values"
  )
})

test_that("friedman_conover_test output works with add_xy_position (ggpubr brackets) (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id)
  pos <- res %>% add_xy_position(x = "treatment")
  expect_true(all(c("xmin", "xmax", "y.position") %in% colnames(pos)))
  expect_equal(nrow(pos), 3L)
  expect_false(any(is.na(pos$y.position)))
})

test_that("drop_formula_block_term strips '| subject' but leaves simple formulas intact", {
  # repeated-measures formula -> only the within term remains
  expect_equal(
    deparse(drop_formula_block_term(score ~ treatment | id)),
    "score ~ treatment"
  )
  # ordinary pairwise formulas are returned unchanged (no-regression):
  # the exact same object is returned, attributes/environment preserved
  expect_true(identical(len ~ dose, drop_formula_block_term(len ~ dose)))
  expect_equal(deparse(drop_formula_block_term(len ~ supp)), "len ~ supp")
  # non-syntactic (backticked) names and compound within terms are preserved
  expect_equal(deparse(drop_formula_block_term(`my score` ~ trt | id)), "`my score` ~ trt")
  expect_equal(deparse(drop_formula_block_term(y ~ a + b | id)), "y ~ a + b")
})

test_that("add_xy_position works on results with non-syntactic outcome names (#8)", {
  df <- data.frame(
    `my score` = c(10, 12, 9, 22, 18, 20, 28, 33, 30, 17, 14, 15, 5, 9, 7),
    trt        = factor(rep(c("a", "b", "c"), 5)),
    id         = factor(rep(1:5, each = 3)),
    check.names = FALSE
  )
  res <- df %>% friedman_conover_test(`my score` ~ trt | id)
  pos <- res %>% add_xy_position(x = "trt")
  expect_true(all(c("xmin", "xmax", "y.position") %in% colnames(pos)))
  expect_false(any(is.na(pos$y.position)))
})

test_that("get_description gives a friendly friedman_conover_test label (#8)", {
  res <- demo_df %>% friedman_conover_test(score ~ treatment | id)
  expect_equal(get_description(res), "Durbin-Conover test")
  lab <- unlist(get_test_label(res, type = "text"))
  expect_true(all(grepl("Durbin-Conover test", lab)))
})
