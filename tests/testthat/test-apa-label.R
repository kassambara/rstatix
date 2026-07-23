context("test-apa-label")

tg_f <- function() {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  d
}

test_that("style = 'apa' drops the label and uses APA-7 p formatting", {
  d <- tg_f()
  # p >= .001 -> "p = .061" (three decimals, no leading zero); no leading "T test,"
  expect_equal(
    get_test_label(d %>% t_test(len ~ supp), type = "text", style = "apa"),
    "t(55.31) = 1.92, p = .061"
  )
  # p < .001 -> "p < .001"
  expect_equal(
    get_test_label(d %>% kruskal_test(len ~ dose), type = "text", style = "apa"),
    "X2(2) = 40.67, p < .001"
  )
})

test_that("style = 'apa' appends the effect size with the APA leading-zero rule", {
  d <- tg_f()
  # Cohen's d is unbounded -> keeps the leading zero (d = 0.49)
  expect_equal(
    get_test_label(d %>% t_test(len ~ supp, effect.size = TRUE), type = "text", style = "apa"),
    "t(55.31) = 1.92, p = .061, d = 0.49"
  )
  # eta-squared is bounded in [0, 1] -> drops the leading zero (.70)
  expect_equal(
    get_test_label(d %>% anova_test(len ~ dose), type = "text", style = "apa"),
    "F(2, 57) = 67.42, p < .001, eta2[g] = .70"
  )
  # Cliff's delta is bounded in [-1, 1] -> drops the leading zero, keeps the sign
  expect_equal(
    get_test_label(d %>% wilcox_test(len ~ dose, effect.size = TRUE), row = 1,
                   type = "text", style = "apa"),
    "W = 33.50, p < .001, delta = -.83"
  )
})

test_that("style = 'apa' inserts the effect-size CI when the object carries one", {
  d <- tg_f()
  av <- d %>% anova_test(len ~ dose, effect.size = "pes", ci = 0.95)
  expect_equal(
    get_test_label(av, type = "text", style = "apa"),
    "F(2, 57) = 67.42, p < .001, eta2[p] = .70, 95% CI [.57, .79]"
  )
})

test_that("the effect-size CI is attached only to the effect size it belongs to", {
  # anova_test(ci = ) computes the interval for PARTIAL eta-squared (pes). When
  # the label shows generalized eta-squared (ges), that pes interval must NOT be
  # attached (it would not even bracket the ges estimate); it is attached only
  # when pes is the shown estimate.
  d <- tg_f()
  ges.pes <- get_test_label(
    d %>% anova_test(len ~ dose, effect.size = c("ges", "pes"), ci = 0.95),
    type = "text", style = "apa"
  )
  expect_true(grepl("eta2\\[g\\]", ges.pes))   # generalized eta shown
  expect_false(grepl("CI", ges.pes))           # partial-eta CI not attached
  pes.only <- get_test_label(
    d %>% anova_test(len ~ dose, effect.size = "pes", ci = 0.95),
    type = "text", style = "apa"
  )
  expect_true(grepl("eta2\\[p\\].*95% CI", pes.only))
})

test_that("style = 'classic' ignores the per-metric effect.size columns (no regression)", {
  # The cohens.d/cliff.delta/r/rank.biserial columns from effect.size = TRUE are
  # consulted only for style = "apa"; the classic label is byte-identical to the
  # historical output (no effect-size clause).
  d <- tg_f()
  expect_equal(
    get_test_label(d %>% t_test(len ~ supp, effect.size = TRUE),
                   type = "text", detailed = TRUE),
    "T test, t(55.31) = 1.92, p = 0.061, n = 60"
  )
  expect_false(grepl("delta|cliff",
    get_test_label(d %>% wilcox_test(len ~ dose, effect.size = TRUE), row = 1,
                   type = "text", detailed = TRUE)))
})

test_that("style = 'apa' degrades gracefully when there is no effect size or CI", {
  d <- tg_f()
  # plain t_test: no effect-size column -> statistic + p only
  expect_equal(
    get_test_label(d %>% t_test(len ~ supp), type = "text", style = "apa"),
    "t(55.31) = 1.92, p = .061"
  )
  # a t_test detailed carries conf.low/high for the MEAN DIFFERENCE, not the
  # effect size, so no CI clause is added (it is not the effect size's interval)
  lab <- get_test_label(d %>% t_test(len ~ supp, detailed = TRUE), type = "text", style = "apa")
  expect_false(grepl("CI", lab))
})

test_that("style defaults to 'classic', leaving existing labels unchanged", {
  d <- tg_f()
  classic <- get_test_label(d %>% t_test(len ~ supp), type = "text", detailed = TRUE)
  expect_equal(classic, "T test, t(55.31) = 1.92, p = 0.061, n = 60")
  # explicit classic == default
  expect_equal(
    classic,
    get_test_label(d %>% t_test(len ~ supp), type = "text", detailed = TRUE, style = "classic")
  )
})

test_that("style = 'apa' works in expression (plotmath) mode with italic symbols", {
  d <- tg_f()
  ex <- get_test_label(d %>% t_test(len ~ supp), style = "apa")
  expect_true(is.call(ex) || is.expression(ex))
  chr <- paste(deparse(ex), collapse = " ")
  expect_true(grepl('italic\\("t"\\)', chr))   # statistic italicised
  expect_true(grepl('italic\\("p"\\)', chr))   # p italicised
  expect_true(grepl("\\.061", chr))            # APA p, no leading zero
  expect_false(grepl("T test", chr))           # no description label
})

test_that("create_test_label() honours style = 'apa'", {
  lab <- create_test_label(
    statistic.text = "t", statistic = 2.31, p = 0.025, parameter = "58",
    effect.size = 0.61, effect.size.text = "d", effect.size.bounded = FALSE,
    type = "text", style = "apa"
  )
  expect_equal(lab, "t(58) = 2.31, p = .025, d = 0.61")
})

test_that("the APA pes interval is re-derived at full precision and labelled with its level", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  # The stored table rounds conf.high to 0.785; the true bound is 0.7853874
  # (matches effectsize::F_to_eta2(67.4157, 2, 57, ci = 0.95, alternative =
  # "two.sided") CI_high, effectsize 1.0.1, 2026-07-23), so APA's 2 decimals
  # must show .79, not the double-rounded .78.
  res <- anova_test(df, len ~ dose, effect.size = "pes", ci = 0.95)
  expect_equal(
    get_test_label(res, type = "text", style = "apa"),
    "F(2, 57) = 67.42, p < .001, eta2[p] = .70, 95% CI [.57, .79]"
  )
  # a non-default level is shown as computed, not hard-coded to 95%
  res90 <- anova_test(df, len ~ dose, effect.size = "pes", ci = 0.90)
  expect_equal(
    get_test_label(res90, type = "text", style = "apa"),
    "F(2, 57) = 67.42, p < .001, eta2[p] = .70, 90% CI [.59, .77]"
  )
})

test_that("apa p formatting handles character p in any locale and caps at .999", {
  # character p strings come from the documented create_test_label() usage
  # (p = "<0.0001"); they must be parsed, not string-compared (locale collation)
  lab <- function(p) create_test_label(statistic.text = "t", statistic = 2.1,
                                       parameter = "30", p = p, type = "text",
                                       style = "apa")
  expect_equal(lab("<0.0001"), "t(30) = 2.10, p < .001")
  expect_equal(lab("<0.05"),   "t(30) = 2.10, p < .05")
  expect_equal(lab("0.023"),   "t(30) = 2.10, p = .023")
  # APA never reports p = 1.000
  expect_equal(lab(0.9996), "t(30) = 2.10, p > .999")
  expect_equal(lab(1),      "t(30) = 2.10, p > .999")
  expect_equal(lab(0.9994), "t(30) = 2.10, p = .999")
  # boundary behavior around .001 is unchanged
  expect_equal(lab(0.001),     "t(30) = 2.10, p = .001")
  expect_equal(lab(0.0009999), "t(30) = 2.10, p < .001")
})

test_that("apa df spacing follows APA-7 while classic stays compact", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  res <- anova_test(df, len ~ dose)
  expect_match(get_test_label(res, type = "text", style = "apa"),
               "^F\\(2, 57\\)")
  expect_match(get_test_label(res, type = "text", detailed = TRUE),
               "F\\(2,57\\)")
})

test_that("a sphericity-corrected label shows the object's stored interval", {
  # get_anova_table(correction = "GG") corrects DFn/DFd but the stored
  # conf.low/conf.high are defined at the uncorrected df; re-deriving the
  # interval from the corrected row would contradict the object's own interval,
  # so the corrected path must display the stored bounds as they are.
  score <- c(
    12, 15, 18, 11, 14, 20, 13, 17, 19, 10, 16, 21,
    14, 13, 22, 15, 12, 17, 11, 18, 20, 16, 14, 23,
    13, 16, 17, 12, 15, 19, 14, 18, 21, 11, 17, 22
  )
  d <- data.frame(
    id = factor(rep(1:12, each = 3)),
    time = factor(rep(c("t1", "t2", "t3"), times = 12)),
    score = score
  )
  ra <- anova_test(d, dv = score, wid = id, within = time,
                   effect.size = "pes", ci = 0.95)
  tab <- get_anova_table(ra, correction = "GG")
  lab <- get_test_label(ra, type = "text", style = "apa", correction = "GG")
  fmt <- function(x) sub("^0\\.", ".", formatC(round(x, 2), format = "f", digits = 2))
  expect_match(lab, paste0("95% CI \\[", fmt(tab$conf.low[1]), ", ",
                           fmt(tab$conf.high[1]), "\\]"), fixed = FALSE)
})
