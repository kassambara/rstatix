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

test_that("add_cld warns on an incomplete (e.g. ref.group) comparison set (#110)", {
  dn <- ToothGrowth %>% dunn_test(len ~ dose, ref.group = "0.5")   # only k-1 pairs
  expect_warning(dn %>% add_cld(), "all-pairwise|misleading")
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
