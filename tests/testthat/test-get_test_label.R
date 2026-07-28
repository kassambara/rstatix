context("test-get_test_label")

test_that("get_test_label includes the sample size n for ANOVA (#150)", {
  one_way <- get_test_label(ToothGrowth %>% anova_test(len ~ dose),
                            detailed = TRUE, type = "text")
  expect_match(one_way, "n = 60")
  # two-way: total sample size
  two_way <- get_test_label(ToothGrowth %>% anova_test(len ~ supp * dose),
                            detailed = TRUE, type = "text")
  expect_match(two_way, "n = 60")
  # repeated measures: n = number of subjects (wid)
  set.seed(1)
  rm <- data.frame(id = factor(rep(1:10, 3)),
                   time = factor(rep(c("t1", "t2", "t3"), each = 10)),
                   score = rnorm(30))
  rm_label <- get_test_label(anova_test(rm, dv = score, wid = id, within = time),
                             detailed = TRUE, type = "text")
  expect_match(rm_label, "n = 10")
  # expression type still builds without error
  expect_silent(get_test_label(ToothGrowth %>% anova_test(len ~ dose), detailed = TRUE))
})

test_that("ANOVA n counts the observations analysed, not the rows supplied (#322)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  df$len[1:20] <- NA          # 40 of 60 rows usable
  analysed <- sum(!is.na(df$len))
  expect_equal(analysed, 40)

  res <- suppressWarnings(anova_test(df, len ~ dose))
  expect_equal(as.numeric(get_n(res)[1]), 40)
  expect_match(get_test_label(res, detailed = TRUE, type = "text"), "n = 40")

  # the label must agree with the degrees of freedom printed beside it: a
  # one-way between-subjects ANOVA has DFn + DFd + 1 observations
  tab <- get_anova_table(res)
  expect_equal(tab$DFn[1] + tab$DFd[1] + 1, 40)

  # ... and with kruskal_test(), which carries its own n and was already right
  expect_match(
    get_test_label(suppressWarnings(kruskal_test(df, len ~ dose)), detailed = TRUE, type = "text"),
    "n = 40"
  )

  # factorial formula: still the total analysed sample size, one value per row
  df$supp <- factor(df$supp)
  two_way <- suppressWarnings(anova_test(df, len ~ supp * dose))
  expect_true(all(as.numeric(get_n(two_way)) == 40))

  # a model fitted by the caller: lm() dropped the rows itself, and get_n()
  # returned NA because the stashed "data" is the model, not a data frame
  expect_equal(as.numeric(get_n(anova_test(lm(len ~ dose, data = df)))[1]), 40)
})

test_that("a caller-fitted model reports n like a data frame does (#322)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  res <- anova_test(lm(len ~ dose, data = df))

  # n was NA for a model input, so the label omitted it entirely; it now
  # carries the observation count the model was fitted on, as a data frame does
  expect_equal(as.numeric(get_n(res)[1]), 60)
  expect_match(get_test_label(res, detailed = TRUE, type = "text"), "n = 60")
  expect_equal(
    get_test_label(res, detailed = TRUE, type = "text"),
    get_test_label(anova_test(df, len ~ dose), detailed = TRUE, type = "text")
  )
  # an aov() fit reaches the same branch
  expect_equal(as.numeric(get_n(anova_test(aov(len ~ dose, data = df)))[1]), 60)

  # the APA style does not print n, so it is unchanged either way
  expect_false(grepl("n = ", get_test_label(res, detailed = TRUE, type = "text", style = "apa")))
})

test_that("ANOVA n is unchanged when no row is dropped, and for wid designs (#322)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  df$supp <- factor(df$supp)
  df$w <- seq_len(nrow(df))

  # complete data: every between-subjects design reports the row count as before
  expect_equal(as.numeric(get_n(anova_test(df, len ~ dose))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, len ~ supp * dose))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, len ~ dose, type = 1))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, len ~ dose, type = 3))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, len ~ w + dose))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, dv = "len", between = "dose"))[1]), 60)
  expect_equal(as.numeric(get_n(anova_test(df, dv = "len", between = "dose", covariate = "w"))[1]), 60)

  # the dv/between path stores data that factorial_design() already cleaned, so
  # it counted the analysed rows before this change and still does
  df.na <- df
  df.na$len[1:20] <- NA
  expect_equal(as.numeric(get_n(suppressWarnings(anova_test(df.na, dv = "len", between = "dose")))[1]), 40)

  # repeated measures count subjects, not rows: dropping one subject's rows
  # leaves 19, and that branch is not the one this change touches
  set.seed(1)
  rm_data <- data.frame(id = factor(rep(1:20, each = 3)),
                        time = factor(rep(c("t1", "t2", "t3"), 20)),
                        score = rnorm(60))
  expect_match(
    get_test_label(anova_test(rm_data, dv = score, wid = id, within = time),
                   detailed = TRUE, type = "text"),
    "n = 20"
  )
  rm_data$score[1:3] <- NA    # subject 1 loses every row
  expect_match(
    get_test_label(suppressWarnings(anova_test(rm_data, dv = score, wid = id, within = time)),
                   detailed = TRUE, type = "text"),
    "n = 19"
  )
})

test_that("the count follows the fit when a transformation drops rows (#322)", {
  # A missing value is not the only way to lose a row: log() of a non-positive
  # value is NaN, so lm() drops those rows although the data frame has no NA.
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  df$x <- c(rep(-1, 5), rep(1, 25), rep(2, 30))
  expect_false(anyNA(df))

  res <- suppressWarnings(anova_test(df, len ~ log(x)))
  tab <- get_anova_table(res)
  # 1 + 53 + 1 = 55 observations, which is what the count must agree with
  expect_equal(tab$DFn[1] + tab$DFd[1] + 1, 55)
  expect_equal(as.numeric(get_n(res)[1]), 55)

  # the same holds for a transformed response
  df2 <- ToothGrowth
  df2$dose <- factor(df2$dose)
  df2$len[1:5] <- -1
  res2 <- suppressWarnings(anova_test(df2, log(len) ~ dose))
  expect_equal(as.numeric(get_n(res2)[1]), 55)
  expect_match(get_test_label(res2, detailed = TRUE, type = "text"), "n = 55")
})

test_that("a wid design still counts every subject with a usable row (#333)", {
  # Known limitation this change does NOT address, locked so it cannot move
  # unnoticed: a subject missing only SOME of its within-cells is still counted,
  # although the model drops it. Whole-subject missingness is the case that
  # looks right, because those rows are gone before the count happens.
  set.seed(1)
  rm_data <- data.frame(id = factor(rep(1:20, each = 3)),
                        time = factor(rep(c("t1", "t2", "t3"), 20)),
                        score = rnorm(60))
  rm_data$score[c(1, 5, 9)] <- NA   # subjects 1, 2, 3 each lose ONE session

  res <- suppressWarnings(anova_test(rm_data, dv = score, wid = id, within = time))
  # only 17 subjects have a complete set of cells ...
  expect_equal(sum(tapply(!is.na(rm_data$score), rm_data$id, all)), 17L)
  # ... and the degrees of freedom in the same label say so: 2 * (17 - 1)
  expect_equal(get_anova_table(res)$DFd[1], 32)
  # the reported n is nevertheless the 20 subjects supplied
  expect_match(get_test_label(res, detailed = TRUE, type = "text"), "n = 20")
})

test_that("get_test_label is unchanged for non-ANOVA tests and non-detailed labels (#150)", {
  expect_match(get_test_label(ToothGrowth %>% t_test(len ~ supp), detailed = TRUE, type = "text"), "n = 60")
  expect_match(get_test_label(ToothGrowth %>% kruskal_test(len ~ dose), detailed = TRUE, type = "text"), "n = 60")
  # the non-detailed ANOVA label has no n (only the description + p)
  expect_false(grepl("n =", get_test_label(ToothGrowth %>% anova_test(len ~ dose), type = "text")))
})
