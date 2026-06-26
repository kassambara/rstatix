context("test-binom_test")

test_that("pairwise_binom_test_against_p works with an unnamed vector (#44)", {
  res <- pairwise_binom_test_against_p(c(20, 30, 50))
  expect_s3_class(res, "rstatix_test")
  expect_equal(nrow(res), 3)
  expect_equal(res$group, c("grp1", "grp2", "grp3"))
  expect_true(all(c("group", "p", "p.adj") %in% colnames(res)))
})

test_that("pairwise_binom_test_against_p preserves names for named/table input (no regression, #44)", {
  named   <- pairwise_binom_test_against_p(c(a = 20, b = 30, c = 50))
  unnamed <- pairwise_binom_test_against_p(c(20, 30, 50))
  tbl     <- pairwise_binom_test_against_p(as.table(c(x = 20, y = 30, z = 50)))
  expect_equal(named$group, c("a", "b", "c"))
  expect_equal(tbl$group,   c("x", "y", "z"))
  # only the group labels differ between named and unnamed; the statistics are identical
  expect_equal(named$p, unnamed$p)
  expect_equal(named$p.adj, unnamed$p.adj)
})
