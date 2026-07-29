context("test-add_cld")

# Build a minimal pairwise-comparison tibble (one .y., explicit p.adj) for a set
# of significant pairs; non-listed pairs are non-significant.
make_pairwise <- function(groups, sig_pairs, y = "y"){
  pairs <- utils::combn(groups, 2, simplify = FALSE)
  is.sig <- vapply(pairs, function(p){
    any(vapply(sig_pairs, function(s) setequal(s, p), logical(1)))
  }, logical(1))
  tibble::tibble(
    .y.    = y,
    group1 = vapply(pairs, `[`, character(1), 1),
    group2 = vapply(pairs, `[`, character(1), 2),
    p.adj  = ifelse(is.sig, 0.001, 0.5)
  )
}

# Expected letters below were cross-checked against
# multcompView::multcompLetters() (a development-time reference, not a dependency).

test_that("add_cld assigns distinct letters when all pairs differ (#110)", {
  res <- make_pairwise(c("A", "B", "C"), list(c("A", "B"), c("A", "C"), c("B", "C"))) %>%
    add_cld()
  expect_equal(res$group, c("A", "B", "C"))
  expect_equal(res$cld, c("a", "b", "c"))
})

test_that("add_cld assigns a shared letter when all pairs are non-significant (#110)", {
  res <- make_pairwise(c("A", "B", "C"), list()) %>% add_cld()
  expect_equal(res$cld, c("a", "a", "a"))
})

test_that("add_cld handles the classic overlapping a/ab/b case (#110)", {
  # A-C significant; A-B and B-C non-significant
  res <- make_pairwise(c("A", "B", "C"), list(c("A", "C"))) %>% add_cld()
  expect_equal(res$cld, c("a", "ab", "b"))
})

test_that("add_cld handles a chain topology a/ab/bc/c (#110)", {
  res <- make_pairwise(
    c("A", "B", "C", "D"),
    list(c("A", "C"), c("A", "D"), c("B", "D"))
  ) %>% add_cld()
  expect_equal(res$cld, c("a", "ab", "bc", "c"))
})

test_that("add_cld handles two clusters (#110)", {
  res <- make_pairwise(
    c("A", "B", "C", "D"),
    list(c("A", "C"), c("A", "D"), c("B", "C"), c("B", "D"))
  ) %>% add_cld()
  expect_equal(res$cld, c("a", "a", "b", "b"))
})

test_that("add_cld matches tukey_hsd / dunn_test on ToothGrowth (#110)", {
  tg <- ToothGrowth; tg$dose <- factor(tg$dose)
  tuk <- tg %>% tukey_hsd(len ~ dose) %>% add_cld()
  expect_equal(tuk$group, c("0.5", "1", "2"))
  expect_equal(tuk$cld, c("a", "b", "c"))     # all doses differ
  dn <- ToothGrowth %>% dunn_test(len ~ dose) %>% add_cld()
  expect_equal(dn$cld, c("a", "b", "c"))
})

test_that("add_cld computes one display per group for grouped tests (#110)", {
  tg <- ToothGrowth; tg$dose <- factor(tg$dose)
  res <- tg %>%
    dplyr::group_by(supp) %>%
    tukey_hsd(len ~ dose) %>%
    add_cld()
  expect_true(all(c("supp", "group", "cld") %in% colnames(res)))
  expect_equal(nrow(res), 6L)                 # 2 supp x 3 doses
  # For OJ, doses 1 and 2 are not significantly different (p.adj = 0.13) -> share a letter
  oj <- res[res$supp == "OJ", ]
  expect_equal(oj$cld, c("a", "b", "b"))
  vc <- res[res$supp == "VC", ]
  expect_equal(vc$cld, c("a", "b", "c"))
})

test_that("add_cld supports p.col and reversed (#110)", {
  dn <- ToothGrowth %>% dunn_test(len ~ dose)
  # use raw p instead of p.adj
  res_p <- dn %>% add_cld(p.col = "p")
  expect_equal(res_p$cld, c("a", "b", "c"))
  # reversed letter assignment
  res_rev <- dn %>% add_cld(reversed = TRUE)
  expect_equal(res_rev$cld, c("c", "b", "a"))
})

test_that("add_cld respects the significance threshold (#110)", {
  # p.adj for 1-vs-2 in OJ is ~0.13; a higher threshold makes it significant
  tg <- ToothGrowth; tg$dose <- factor(tg$dose)
  oj <- tg %>% dplyr::filter(supp == "OJ") %>% tukey_hsd(len ~ dose)
  expect_equal((oj %>% add_cld(threshold = 0.05))$cld, c("a", "b", "b"))
  expect_equal((oj %>% add_cld(threshold = 0.20))$cld, c("a", "b", "c"))
})

test_that("add_cld keeps single-character labels beyond 26 groups (#110)", {
  # 30 mutually-significant groups -> 30 distinct single-character letters; the
  # cld string must stay tokenizable (no multi-char labels that would corrupt it)
  g <- sprintf("G%02d", 1:30)
  res <- make_pairwise(g, utils::combn(g, 2, simplify = FALSE)) %>% add_cld()
  expect_equal(length(unique(res$cld)), 30L)
  expect_true(all(nchar(res$cld) == 1L))
  expect_false(any(duplicated(res$cld)))   # all differ -> no shared letters
})

test_that("add_cld errors clearly beyond 52 letter groups (#110)", {
  g <- sprintf("G%03d", 1:53)
  df <- make_pairwise(g, utils::combn(g, 2, simplify = FALSE))
  expect_error(suppressWarnings(add_cld(df)), "at most 52")
})

test_that("add_cld gives no letter where a comparison is missing (#110, #323)", {
  dn <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")   # only k-1 pairs
  expect_warning(res <- dn %>% add_cld(), "was not established|1 - 2")
  res <- suppressWarnings(dn %>% add_cld())
  # 1 vs 2 was never compared, so neither can be lettered; the reference group,
  # whose every pair IS present, still can be
  expect_equal(res$cld[res$group == "0.5"], "a")
  expect_true(all(is.na(res$cld[res$group %in% c("1", "2")])))
})

test_that("add_cld gives no letter where a p-value is NA (#323)", {
  # games_howell_test() returns p.adj = NA when a pair's Welch standard error is
  # zero or undefined, which needs BOTH groups constant (A and B here) or one of
  # them a single observation. Reading that as
  # "not significantly different" gave A and B the same letter, although their
  # means are 0 and 10 and every Tukey p-value on the same data is < 1e-7.
  d <- data.frame(
    g = factor(rep(c("A", "B", "C"), each = 6)),
    v = c(rep(0, 6), rep(10, 6), c(19, 20, 21, 19.5, 20.5, 20))
  )
  # the test itself already warns that the comparison is undefined
  expect_warning(gh <- games_howell_test(d, v ~ g), "zero or undefined variance")
  expect_true(is.na(gh$p.adj[gh$group1 == "A" & gh$group2 == "B"]))

  expect_warning(add_cld(gh), "was not established|A - B")
  res <- suppressWarnings(add_cld(gh))
  expect_true(is.na(res$cld[res$group == "A"]))
  expect_true(is.na(res$cld[res$group == "B"]))
  # C is comparable with everything that remains, so it keeps a letter
  expect_false(is.na(res$cld[res$group == "C"]))

  # an independent check that A and B really are different
  expect_true(all(stats::TukeyHSD(stats::aov(v ~ g, data = d))$g[, "p adj"] < 1e-6))
})

test_that("add_cld is unchanged when every comparison is known (#323)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  # no warning, and the letters are the ones the complete input always gave
  expect_silent(res <- add_cld(tukey_hsd(df, len ~ dose)))
  expect_equal(res$cld, c("a", "b", "c"))
  expect_silent(res2 <- add_cld(dunn_test(df, len ~ dose)))
  expect_equal(res2$cld, c("a", "b", "c"))
})

test_that("add_cld orders groups by factor levels when group columns are factors (#110)", {
  df <- tibble::tibble(
    .y.    = "y",
    group1 = factor(c("lo", "lo", "mid"), levels = c("lo", "mid", "hi")),
    group2 = factor(c("mid", "hi", "hi"), levels = c("lo", "mid", "hi")),
    p.adj  = c(0.001, 0.001, 0.001)
  )
  res <- df %>% add_cld()
  expect_equal(res$group, c("lo", "mid", "hi"))    # factor-level order, not appearance
})

test_that("add_cld errors clearly on invalid input (#110)", {
  expect_error(tibble::tibble(x = 1) %>% add_cld(), "group1")
  expect_error(
    tibble::tibble(group1 = "a", group2 = "b") %>% add_cld(),
    "p-value column"
  )
})

test_that("one constant group alone does not make a comparison unestimable (#323)", {
  # guards the stated cause: a comparison AGAINST a single constant group is
  # perfectly estimable, so nothing here is NA and every group keeps a letter
  d <- data.frame(
    g = factor(rep(c("A", "B", "C"), each = 6)),
    v = c(rep(0, 6), c(9, 10, 11, 9.5, 10.5, 10), c(19, 20, 21, 19.5, 20.5, 20))
  )
  gh <- games_howell_test(d, v ~ g)
  expect_false(anyNA(gh$p.adj))
  expect_silent(res <- add_cld(gh))
  expect_equal(res$cld, c("a", "b", "c"))
})

test_that("a group whose comparisons are all unestimable empties the display (#323)", {
  # The rule is symmetric rather than minimal: F has a single observation, so
  # every pair touching it is NA, and every group is therefore set aside -- even
  # though the ten A-E comparisons were all estimated. Locked so the choice
  # cannot change unnoticed.
  set.seed(1)
  d <- data.frame(
    g = factor(c(rep(c("A", "B", "C", "D", "E"), each = 8), "F")),
    v = c(stats::rnorm(8, 0), stats::rnorm(8, 10), stats::rnorm(8, 20),
          stats::rnorm(8, 30), stats::rnorm(8, 40), 99)
  )
  gh <- suppressWarnings(games_howell_test(d, v ~ g))
  na.rows <- is.na(gh$p.adj)
  expect_true(all(gh$group1[na.rows] == "F" | gh$group2[na.rows] == "F"))
  expect_false(anyNA(gh$p.adj[!na.rows]))          # A-E all estimated

  expect_warning(res <- add_cld(gh), "was not established")
  res <- suppressWarnings(add_cld(gh))
  expect_true(all(is.na(res$cld)))
})
