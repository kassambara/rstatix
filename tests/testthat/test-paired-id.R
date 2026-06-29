context("test-paired-id")

# Long-format paired data: subjects 1..10 measured under groups a, b, c.
make_paired <- function(){
  set.seed(42)
  data.frame(
    y  = rnorm(30),
    g  = rep(c("a", "b", "c"), each = 10),
    id = rep(1:10, 3)
  )
}

test_that("id= matches a manual complete-case paired t-test (#136)", {
  df <- make_paired()
  df <- df[df$g %in% c("a", "b"), ]
  res <- df %>% t_test(y ~ g, paired = TRUE, id = "id")
  x <- df$y[df$g == "a"]; y <- df$y[df$g == "b"]   # already subject-aligned here
  ref <- t.test(x, y, paired = TRUE)
  expect_equal(unname(res$statistic), unname(ref$statistic), tolerance = 1e-7)
  expect_equal(res$p, ref$p.value, tolerance = 1e-7)
  expect_equal(res$n1, 10); expect_equal(res$n2, 10)
})

test_that("id= aligns by subject regardless of row order (#136 silent mis-pairing)", {
  df <- make_paired()
  df <- df[df$g %in% c("a", "b"), ]
  correct <- (df %>% t_test(y ~ g, paired = TRUE, id = "id"))$statistic
  # shuffle the rows of group b: row order no longer matches subject order
  shuffled <- rbind(df[df$g == "a", ], df[df$g == "b", ][c(5, 1, 8, 3, 10, 2, 7, 4, 9, 6), ])
  res <- shuffled %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(res$statistic, correct, tolerance = 1e-9)   # id pairing is order-invariant
})

test_that("id= handles unequal group sizes from missing observations (#192)", {
  df <- make_paired()
  df <- df[df$g %in% c("a", "b"), ]
  # subject 3 entirely missing from group b -> groups have unequal sizes
  df2 <- df[!(df$g == "b" & df$id == 3), ]
  # the default paired path errors on unequal lengths ...
  expect_error(df2 %>% t_test(y ~ g, paired = TRUE))
  # ... but id= uses the 9 common subjects
  res <- df2 %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(res$n1, 9); expect_equal(res$n2, 9)
  common <- intersect(df2$id[df2$g == "a"], df2$id[df2$g == "b"])
  xa <- df2$y[df2$g == "a"][match(common, df2$id[df2$g == "a"])]
  yb <- df2$y[df2$g == "b"][match(common, df2$id[df2$g == "b"])]
  ref <- t.test(xa, yb, paired = TRUE)
  expect_equal(unname(res$statistic), unname(ref$statistic), tolerance = 1e-7)
})

test_that("id= NA values are dropped per pair (complete pairs) (#175)", {
  df <- make_paired()
  df <- df[df$g %in% c("a", "b"), ]
  df$y[df$g == "b" & df$id == 4] <- NA   # subject 4 missing in b
  res <- df %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(res$n1, 9); expect_equal(res$n2, 9)   # the NA pair is dropped
})

test_that("id= does per-comparison pairwise deletion for >2 groups (#192)", {
  df <- make_paired()
  # remove subject 3 from group b, and subject 7 from group c
  df <- df[!(df$g == "b" & df$id == 3), ]
  df <- df[!(df$g == "c" & df$id == 7), ]
  res <- df %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(nrow(res), 3L)                          # a-b, a-c, b-c
  np <- setNames(res$n1, paste(res$group1, res$group2))
  expect_equal(unname(np["a b"]), 9)                   # a (10) vs b (9) -> 9 pairs
  expect_equal(unname(np["a c"]), 9)                   # a (10) vs c (9) -> 9 pairs
  expect_equal(unname(np["b c"]), 8)                   # b (9, no 3) vs c (9, no 7) -> 8 common
})

test_that("id= works on grouped data, independently per group (#153)", {
  df <- make_paired()
  df$cohort <- rep(c("x", "y"), length.out = nrow(df))
  res <- df %>% dplyr::group_by(cohort) %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_true("cohort" %in% colnames(res))
  expect_true(nrow(res) >= 1)
})

test_that("id= also works for wilcox_test (#136)", {
  df <- make_paired()
  df <- df[df$g %in% c("a", "b"), ]
  df2 <- df[!(df$g == "b" & df$id == 3), ]
  res <- df2 %>% wilcox_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(res$n1, 9); expect_equal(res$n2, 9)
  common <- intersect(df2$id[df2$g == "a"], df2$id[df2$g == "b"])
  xa <- df2$y[df2$g == "a"][match(common, df2$id[df2$g == "a"])]
  yb <- df2$y[df2$g == "b"][match(common, df2$id[df2$g == "b"])]
  ref <- suppressWarnings(wilcox.test(xa, yb, paired = TRUE))
  expect_equal(unname(res$statistic), unname(ref$statistic), tolerance = 1e-7)
})

test_that("id= errors on duplicated ids within a group (#136)", {
  df <- data.frame(
    y = rnorm(6), g = rep(c("a", "b"), each = 3), id = c(1, 1, 2, 1, 2, 3)
  )
  expect_error(df %>% t_test(y ~ g, paired = TRUE, id = "id"), "unique")
})

test_that("id= drops rows with a missing (NA) id instead of cross-joining them (#136)", {
  # 3 clean pairs (ids 1,2,3) plus 2 unidentified (NA-id) rows per group
  df <- data.frame(
    y  = rnorm(10),
    g  = rep(c("a", "b"), each = 5),
    id = c(1, 2, 3, NA, NA, 1, 2, 3, NA, NA)
  )
  res <- df %>% t_test(y ~ g, paired = TRUE, id = "id")
  expect_equal(res$n1, 3); expect_equal(res$n2, 3)   # only the 3 identified pairs
})

test_that("id= is rejected with ref.group = 'all' (#136)", {
  df <- make_paired()
  expect_error(
    df %>% t_test(y ~ g, paired = TRUE, id = "id", ref.group = "all"),
    "ref.group = 'all'"
  )
  expect_error(
    df %>% wilcox_test(y ~ g, paired = TRUE, id = "id", ref.group = "all"),
    "ref.group = 'all'"
  )
})

test_that("id= errors when the id column is missing (#136)", {
  df <- make_paired(); df <- df[df$g %in% c("a", "b"), ]
  expect_error(df %>% t_test(y ~ g, paired = TRUE, id = "subject"), "not found")
})

test_that("id is ignored for an unpaired test (no behaviour change)", {
  df <- make_paired(); df <- df[df$g %in% c("a", "b"), ]
  with_id <- df %>% t_test(y ~ g, id = "id")          # paired = FALSE
  without  <- df %>% t_test(y ~ g)
  expect_equal(with_id$statistic, without$statistic)
  expect_equal(with_id$n1, without$n1)
})
