context("test-chisq_test")

make_df <- function(){
  set.seed(1)
  data.frame(
    eyes = sample(c("blue", "brown"), 200, replace = TRUE),
    hair = sample(c("dark", "fair"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("chisq_test data-frame interface: positional columns (#43)", {
  df <- make_df()
  res <- df %>% chisq_test(eyes, hair)
  ref <- stats::chisq.test(table(df$eyes, df$hair))
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(unname(res$df), unname(ref$parameter))
  expect_equal(res$p, ref$p.value)
  expect_equal(res$n, nrow(df))
  expect_equal(
    colnames(res),
    c("n", "statistic", "p", "df", "method", "p.signif")
  )
})

test_that("chisq_test vars= form equals positional form (#43)", {
  df <- make_df()
  pos  <- df %>% chisq_test(eyes, hair)
  vars <- df %>% chisq_test(vars = c("eyes", "hair"))
  expect_equal(as.data.frame(pos)[1:6], as.data.frame(vars)[1:6])
})

test_that("chisq_test vars= keeps correct= settable (#43)", {
  df <- make_df()
  res <- df %>% chisq_test(vars = c("eyes", "hair"), correct = FALSE)
  ref <- stats::chisq.test(table(df$eyes, df$hair), correct = FALSE)
  expect_equal(unname(res$statistic), unname(ref$statistic))
})

test_that("chisq_test accepts string column names positionally (#43)", {
  df <- make_df()
  bare <- df %>% chisq_test(eyes, hair)
  strg <- df %>% chisq_test("eyes", "hair")
  expect_equal(as.data.frame(bare)[1:6], as.data.frame(strg)[1:6])
})

test_that("chisq_test data-frame interface needs exactly two columns (#43)", {
  df <- make_df()
  expect_error(df %>% chisq_test(eyes), "exactly two columns")
})

test_that("chisq_test honors group_by, one test per group (#43)", {
  df <- make_df()
  df$site <- rep(c("A", "B"), each = 100)
  res <- df %>% dplyr::group_by(site) %>% chisq_test(eyes, hair)
  expect_true("site" %in% colnames(res))
  expect_equal(nrow(res), 2L)                       # one row per group, not pooled
  expect_setequal(res$site, c("A", "B"))
  # each group's statistic matches the per-subset table
  for(s in c("A", "B")){
    sub <- df[df$site == s, ]
    ref <- stats::chisq.test(table(sub$eyes, sub$hair))
    expect_equal(unname(res$statistic[res$site == s]), unname(ref$statistic))
    expect_equal(res$n[res$site == s], nrow(sub))
  }
})

test_that("chisq_test(df, v1, v2, correct=FALSE) works (column in p slot) (#43)", {
  df <- make_df()
  res <- df %>% chisq_test(eyes, hair, correct = FALSE)
  ref <- stats::chisq.test(table(df$eyes, df$hair), correct = FALSE)
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(res$n, nrow(df))
})

test_that("chisq_descriptives works on the data-frame interface (#43)", {
  df <- make_df()
  res <- df %>% chisq_test(eyes, hair)
  desc <- chisq_descriptives(res)
  expect_equal(sum(desc$observed), nrow(df))
  expect_true(is.matrix(expected_freq(res)))
})

test_that("descriptive accessors error clearly on a grouped result (#43)", {
  df <- make_df()
  df$site <- rep(c("A", "B"), each = 100)
  res <- df %>% dplyr::group_by(site) %>% chisq_test(eyes, hair)
  expect_error(chisq_descriptives(res), "single \\(ungrouped\\)")
  expect_error(expected_freq(res), "single \\(ungrouped\\)")
  expect_error(std_residuals(res), "single \\(ungrouped\\)")
})

# ---- No-regression: existing table / vector interfaces unchanged ----

test_that("chisq_test goodness-of-fit (vector) is unchanged (#43 no-regression)", {
  tulip <- c(red = 81, yellow = 50, white = 27)
  res <- chisq_test(tulip)
  ref <- stats::chisq.test(tulip)
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(res$p, ref$p.value)
  expect_equal(res$n, length(tulip))   # existing behavior: n = number of categories
})

test_that("chisq_test on a table/matrix is unchanged (#43 no-regression)", {
  xtab <- as.table(rbind(c(203, 118, 178, 212), c(122, 167, 528, 673)))
  res <- chisq_test(xtab)
  ref <- stats::chisq.test(xtab)
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(unname(res$df), unname(ref$parameter))
  expect_equal(res$n, sum(xtab))
})

test_that("chisq_test on a data-frame-as-table is unchanged (#43 no-regression)", {
  # a data frame that IS a contingency table (no column refs) -> treated as table
  df_tab <- as.data.frame.matrix(matrix(c(12, 5, 7, 20), nrow = 2))
  res <- chisq_test(df_tab)
  ref <- stats::chisq.test(as.matrix(df_tab))
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_equal(res$n, sum(df_tab))
})
