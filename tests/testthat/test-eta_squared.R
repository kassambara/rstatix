context("test-eta_squared")

tg_factorial <- function() {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  stats::aov(len ~ supp * dose, data = df)
}
tg_oneway <- function() {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  stats::aov(len ~ dose, data = df)
}

test_that("eta_squared and partial_eta_squared still return the bare named vector (no regression)", {
  m <- tg_factorial()
  es <- eta_squared(m)
  pes <- partial_eta_squared(m)
  expect_type(es, "double")
  expect_null(dim(es))
  expect_equal(names(es), c("supp", "dose", "supp:dose"))
  # values unchanged: eta = SS_term / SS_total, partial = SS_term / (SS_term + SS_resid)
  expect_equal(unname(es), c(0.05948364661, 0.70286419479, 0.03137671837), tolerance = 1e-8)
  expect_equal(unname(pes), c(0.2238254478, 0.7731091768, 0.1320279124), tolerance = 1e-8)
  # the default is a numeric vector, not a tibble
  expect_false(inherits(es, "data.frame"))
  expect_false(inherits(pes, "data.frame"))
})

test_that("eta_squared(ci=) returns a tibble with a confidence interval per term", {
  res <- eta_squared(tg_factorial(), ci = 0.95)
  expect_s3_class(res, "tbl_df")
  expect_equal(colnames(res), c("Effect", "effsize", "conf.low", "conf.high"))
  expect_equal(res$Effect, c("supp", "dose", "supp:dose"))
  expect_equal(nrow(res), 3L)
  # the point estimate column equals the bare vector, unrounded
  expect_equal(res$effsize, unname(eta_squared(tg_factorial())), tolerance = 1e-12)
  # bounds bracket the estimate, and lie in [0, 1]
  expect_true(all(res$conf.low <= res$effsize + 1e-8))
  expect_true(all(res$effsize <= res$conf.high + 1e-8))
  expect_true(all(res$conf.low >= 0 & res$conf.high <= 1))
})

test_that("the eta_squared interval matches effectsize::eta_squared(partial = FALSE)", {
  # Reference from effectsize::eta_squared(model, partial = FALSE, ci = 0.95,
  # alternative = "two.sided") on aov(len ~ supp * dose). effectsize's default is
  # a one-sided interval; the two-sided call is the one that matches. Hard-coded
  # so the test needs no dependency on effectsize. Pinned snapshot: effectsize
  # 1.0.1, 2026-07-10. The interval is computed in base R by inverting the
  # noncentral F, so a pinned literal cannot notice effectsize changing its
  # algorithm; re-verify when refreshing the snapshot.
  res <- eta_squared(tg_factorial(), ci = 0.95)
  expect_equal(res$conf.low,  c(0.000000000, 0.562330931, 0.000000000), tolerance = 1e-4)
  expect_equal(res$conf.high, c(0.2136466453, 0.7871473588, 0.1460206667), tolerance = 1e-4)
})

test_that("the partial_eta_squared interval matches effectsize::eta_squared(partial = TRUE)", {
  # Same snapshot (effectsize 1.0.1, 2026-07-10), partial = TRUE. These are the
  # same bounds anova_test(len ~ supp * dose, ci = 0.95) reports for pes, which
  # confirms the two entry points agree.
  res <- partial_eta_squared(tg_factorial(), ci = 0.95)
  expect_equal(res$effsize,   c(0.2238254478, 0.7731091768, 0.1320279124), tolerance = 1e-8)
  expect_equal(res$conf.low,  c(0.05864864488, 0.66166186318, 0.00146556212), tolerance = 1e-4)
  expect_equal(res$conf.high, c(0.4020763240, 0.8382110163, 0.2949720386), tolerance = 1e-4)
})

test_that("the two entry points agree with anova_test(ci=) on the partial interval", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  from_at <- df %>%
    anova_test(len ~ supp * dose, effect.size = "pes", ci = 0.95) %>%
    as.data.frame()
  from_at <- from_at[order(from_at$Effect), ]
  from_pes <- partial_eta_squared(tg_factorial(), ci = 0.95)
  from_pes <- from_pes[order(from_pes$Effect), ]
  # anova_test rounds to three decimals; compare at that resolution
  expect_equal(round(from_pes$conf.low, 3),  from_at$conf.low)
  expect_equal(round(from_pes$conf.high, 3), from_at$conf.high)
})

test_that("in a one-way design eta_squared and partial_eta_squared coincide, CI included", {
  # With a single factor there is no distinction between the two estimands, so
  # the intervals are identical -- a property, not a pinned number.
  m <- tg_oneway()
  e <- eta_squared(m, ci = 0.95)
  p <- partial_eta_squared(m, ci = 0.95)
  expect_equal(e$effsize, p$effsize, tolerance = 1e-12)
  expect_equal(e$conf.low, p$conf.low, tolerance = 1e-10)
  expect_equal(e$conf.high, p$conf.high, tolerance = 1e-10)
  # and it reproduces effectsize's one-way value
  expect_equal(e$conf.low, 0.5671195648, tolerance = 1e-4)
  expect_equal(e$conf.high, 0.7853873616, tolerance = 1e-4)
})

test_that("the non-partial interval is built from the pseudo-F, independently derived", {
  # The interval for a non-partial eta-squared is the noncentral-F interval of a
  # pseudo-F, f* = (eta / df) / ((1 - eta) / df.error), mapped back by
  # lambda / (lambda + df.error). Re-derive it here from base R, without the
  # package's helper, and confirm the function returns it.
  m <- tg_factorial()
  tab <- summary(m)[[1]]
  rownames(tab) <- trimws(rownames(tab))
  dfe <- tab["Residuals", "Df"]
  ss  <- tab[["Sum Sq"]]
  names(ss) <- rownames(tab)
  ss.total <- sum(ss)
  ref <- lapply(c("supp", "dose", "supp:dose"), function(term) {
    eta <- ss[[term]] / ss.total
    df1 <- tab[term, "Df"]
    fstar <- (eta / df1) / ((1 - eta) / dfe)
    find <- function(target) {
      if (stats::pf(fstar, df1, dfe, ncp = 0) < target) return(0)
      up <- 2
      while (stats::pf(fstar, df1, dfe, ncp = up) > target) up <- up * 2
      stats::uniroot(function(l) stats::pf(fstar, df1, dfe, ncp = l) - target, c(0, up))$root
    }
    lo <- find(0.975); hi <- find(0.025)
    c(lo / (lo + dfe), hi / (hi + dfe))
  })
  ref <- do.call(rbind, ref)
  res <- eta_squared(m, ci = 0.95)
  expect_equal(res$conf.low,  ref[, 1], tolerance = 1e-6)
  expect_equal(res$conf.high, ref[, 2], tolerance = 1e-6)
})

test_that("eta_squared works from an anova object, not only aov", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  a <- stats::anova(stats::lm(len ~ supp * dose, data = df))
  res <- eta_squared(a, ci = 0.95)
  expect_equal(colnames(res), c("Effect", "effsize", "conf.low", "conf.high"))
  expect_equal(res$effsize, unname(eta_squared(a)), tolerance = 1e-12)
})

test_that("eta_squared validates ci", {
  m <- tg_factorial()
  expect_error(eta_squared(m, ci = 1.5), "ci")
  expect_error(eta_squared(m, ci = 0), "ci")
  expect_error(eta_squared(m, ci = c(0.9, 0.95)), "ci")
  expect_error(partial_eta_squared(m, ci = 1.5), "ci")
  # a wider level gives a wider interval
  wide <- eta_squared(m, ci = 0.99)
  narrow <- eta_squared(m, ci = 0.95)
  expect_true(all(wide$conf.high >= narrow$conf.high - 1e-8))
})

test_that("a car::Anova type 3 table's (Intercept) row is excluded", {
  # The intercept row's SS must stay out of the eta-squared denominator and no
  # (Intercept) effect row may be reported. References from
  # effectsize::eta_squared(a3, partial = FALSE/TRUE), effectsize 1.0.1,
  # 2026-07-23.
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  mt$am <- factor(mt$am)
  m <- stats::lm(mpg ~ cyl * am, data = mt,
                 contrasts = list(cyl = "contr.sum", am = "contr.sum"))
  a3 <- car::Anova(m, type = 3)
  e <- eta_squared(a3)
  pe <- partial_eta_squared(a3)
  expect_equal(names(e), c("cyl", "am", "cyl:am"))
  expect_equal(names(pe), c("cyl", "am", "cyl:am"))
  expect_equal(unname(e),
               c(0.5823612581, 0.0423754394, 0.0360890177), tolerance = 1e-7)
  expect_equal(unname(pe),
               c(0.6319466054, 0.1110613812, 0.0961698559), tolerance = 1e-7)
  # the ci path reports one row per real term, none for the intercept
  ec <- eta_squared(a3, ci = 0.95)
  expect_equal(ec$Effect, c("cyl", "am", "cyl:am"))
  expect_equal(ec$effsize, unname(e), tolerance = 1e-10)
})
