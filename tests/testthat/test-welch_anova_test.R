
test_that("welch_anova_test reports the observations analysed (#334)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  df$len[1:20] <- NA          # 40 of 60 rows usable
  expect_equal(sum(!is.na(df$len)), 40)

  res <- welch_anova_test(df, len ~ dose)
  expect_equal(res$n, 40)
  # and it now agrees with its two siblings on the same data
  expect_equal(as.numeric(get_n(suppressWarnings(kruskal_test(df, len ~ dose)))), 40)
  expect_equal(as.numeric(get_n(suppressWarnings(anova_test(df, len ~ dose)))[1]), 40)

  # a missing GROUP costs the row too, with the outcome intact
  g <- ToothGrowth
  g$dose <- factor(g$dose)
  g$dose[1:7] <- NA
  expect_equal(sum(is.na(g$len)), 0L)
  expect_equal(suppressWarnings(welch_anova_test(g, len ~ dose))$n, 53)

  # unchanged where every supplied row is usable
  clean <- ToothGrowth
  clean$dose <- factor(clean$dose)
  expect_equal(welch_anova_test(clean, len ~ dose)$n, 60)
})

test_that("welch_anova_test counts without evaluating the formula (#334)", {
  # The count comes from complete.cases() over the formula's VARIABLES, not from
  # a model frame. Building one would evaluate the formula a second time, which
  # for a formula whose terms are not a pure function of the data changes the
  # statistic itself, advances .Random.seed twice, and consumes a
  # once-per-session warning that oneway.test() would then not re-raise.
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  d$base <- 2

  set.seed(1)
  nw <- 0
  res <- withCallingHandlers(
    welch_anova_test(d, log(base + rnorm(60)) ~ dose),
    warning = function(w) { nw <<- nw + 1; invokeRestart("muffleWarning") }
  )
  expect_equal(nw, 1L)                       # the warning is raised exactly once
  expect_equal(as.numeric(res$statistic), 1.35)   # the statistic master computes
  expect_equal(round(as.numeric(res$DFd), 3), 33.429)

  # the formula is evaluated once, so the caller's random stream is untouched
  set.seed(99)
  invisible(suppressWarnings(welch_anova_test(d, log(base + rnorm(60)) ~ dose)))
  expect_equal(round(stats::rnorm(3), 6), c(-0.162918, -0.114215, -0.444659))

  # the cost of not evaluating: a row lost to a non-finite TRANSFORMED value is
  # still counted, so this call keeps the count it always had
  lg <- ToothGrowth
  lg$dose <- factor(lg$dose)
  lg$len[c(1, 11, 21, 31, 41)] <- -1
  expect_equal(suppressWarnings(welch_anova_test(lg, log(len) ~ dose))$n, 60)
})
