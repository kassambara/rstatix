context("test-omega_squared")

tg_factorial <- function() {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  stats::aov(len ~ supp * dose, data = df)
}

test_that("omega_squared and partial_omega_squared return a named numeric vector", {
  m <- tg_factorial()
  o <- omega_squared(m)
  po <- partial_omega_squared(m)
  expect_type(o, "double")
  expect_type(po, "double")
  expect_null(dim(o))
  expect_equal(names(o), c("supp", "dose", "supp:dose"))
  expect_equal(names(po), c("supp", "dose", "supp:dose"))
  expect_false(inherits(o, "data.frame"))
})

test_that("omega_squared matches effectsize::omega_squared(partial = FALSE)", {
  # Reference from effectsize::omega_squared(model, partial = FALSE) on
  # aov(len ~ supp * dose). Hard-coded so the test needs no dependency on
  # effectsize. Pinned snapshot: effectsize 1.0.1, 2026-07-11.
  o <- omega_squared(tg_factorial())
  expect_equal(unname(o), c(0.05545190944, 0.69257877125, 0.02364655939), tolerance = 1e-7)
})

test_that("partial_omega_squared matches effectsize::omega_squared(partial = TRUE)", {
  # Same snapshot (effectsize 1.0.1, 2026-07-11), partial = TRUE.
  po <- partial_omega_squared(tg_factorial())
  expect_equal(unname(po), c(0.19540824261, 0.75206604377, 0.09384697888), tolerance = 1e-7)
})

test_that("omega_squared is recomputed independently from the ANOVA table", {
  # Classic omega = (SS - df * MS_error) / (SS_total + MS_error);
  # partial omega = (SS - df * MS_error) / (SS + (N - df) * MS_error),
  # Olejnik & Algina (2003). Re-derive from base R, without the package helpers.
  m <- tg_factorial()
  tab <- summary(m)[[1]]
  rownames(tab) <- trimws(rownames(tab))
  MSr <- tab["Residuals", "Mean Sq"]
  df.err <- tab["Residuals", "Df"]
  n_obs <- sum(tab[["Df"]]) + 1
  ss.total <- sum(tab[["Sum Sq"]])
  terms <- c("supp", "dose", "supp:dose")
  classic <- vapply(terms, function(t) {
    (tab[t, "Sum Sq"] - tab[t, "Df"] * MSr) / (ss.total + MSr)
  }, numeric(1))
  partial <- vapply(terms, function(t) {
    (tab[t, "Sum Sq"] - tab[t, "Df"] * MSr) / (tab[t, "Sum Sq"] + (n_obs - tab[t, "Df"]) * MSr)
  }, numeric(1))
  expect_equal(unname(omega_squared(m)), unname(classic), tolerance = 1e-10)
  expect_equal(unname(partial_omega_squared(m)), unname(partial), tolerance = 1e-10)
})

test_that("in a one-way design partial omega equals classic omega", {
  # With a single factor there is only one effect, so the partial and classic
  # denominators coincide: SS_total + MS_error == SS_effect + (N - df) * MS_error.
  m <- stats::aov(Sepal.Length ~ Species, data = iris)
  expect_equal(unname(omega_squared(m)), unname(partial_omega_squared(m)), tolerance = 1e-12)
  # and reproduces effectsize's one-way value
  expect_equal(unname(omega_squared(m)), 0.6119308, tolerance = 1e-6)
})

test_that("omega_squared works from an anova object and errors cleanly on a repeated-measures model", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  a <- stats::anova(stats::lm(len ~ supp * dose, data = df))
  expect_equal(unname(omega_squared(a)), unname(omega_squared(tg_factorial())), tolerance = 1e-10)
  # between-subjects only, like eta_squared(): a repeated-measures aovlist fails
  # rather than returning a wrong number
  rm <- stats::aov(len ~ dose + Error(1), data = df)
  expect_error(omega_squared(rm))
  expect_error(partial_omega_squared(rm))
})
