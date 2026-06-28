context("test-p_value_precision")

# p-values must be returned at full precision, not pre-rounded to 3 sig figs
# (#108, #135), and adjusted p-values must be computed from the full-precision
# p-values so downstream re-adjustment is exact (#219).

has_more_than_3_sigfigs <- function(p) {
  p <- p[!is.na(p) & p > 0]
  any(abs(p - signif(p, 3)) > 0)
}

test_that("t_test / wilcox_test return full-precision p (#108, #135)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  expect_true(has_more_than_3_sigfigs((df %>% t_test(len ~ dose))$p))
  expect_true(has_more_than_3_sigfigs((df %>% t_test(len ~ dose))$p.adj))
  expect_true(has_more_than_3_sigfigs((df %>% wilcox_test(len ~ dose))$p))
  expect_true(has_more_than_3_sigfigs((df %>% t_test(len ~ supp))$p))      # 2-group
  expect_true(has_more_than_3_sigfigs((df %>% pairwise_t_test(len ~ dose))$p))
})

test_that("post-hoc and base tests return full-precision p (#108, #135)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  expect_true(has_more_than_3_sigfigs((aov(len ~ dose, df) %>% tukey_hsd())$p.adj))
  expect_true(has_more_than_3_sigfigs((df %>% games_howell_test(len ~ dose))$p.adj))
  expect_true(has_more_than_3_sigfigs((df %>% kruskal_test(len ~ dose))$p))
  expect_true(has_more_than_3_sigfigs((mtcars %>% cor_test(mpg, disp))$p))
})

test_that("omnibus ANOVA p stays rounded (anova_test / welch_anova_test out of scope)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  # ANOVA-family omnibus p keeps the 3-sig-fig summary format (not re-adjusted downstream)
  expect_equal((df %>% welch_anova_test(len ~ dose))$p,
               signif((df %>% welch_anova_test(len ~ dose))$p, 3))
})

test_that("adjusted p-values are computed from full-precision p (#219)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  # pairwise t-test: returned p.adj must equal re-adjusting the returned (full) p
  res <- df %>% t_test(len ~ dose, p.adjust.method = "holm")
  expect_equal(res$p.adj, stats::p.adjust(res$p, method = "holm"))
  # pairwise fisher: same invariant (this previously adjusted rounded p)
  fish <- pairwise_fisher_test(as.table(rbind(c(8, 2), c(1, 5), c(3, 9))),
                               p.adjust.method = "holm")
  expect_equal(fish$p.adj, stats::p.adjust(fish$p, method = "holm"))
})

test_that("dunn_test precision is unchanged (already full) (#219 no-regression)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  res <- df %>% dunn_test(len ~ dose)
  expect_equal(res$p.adj, stats::p.adjust(res$p, method = "holm"))
})
