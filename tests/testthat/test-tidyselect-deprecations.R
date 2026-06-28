context("test-tidyselect-deprecations")

# Guards against reintroducing tidyselect deprecation warnings
# (".data in tidyselect", "external vector in selections", all_of() misuse).
# See the package-wide cleanup referenced in NEWS (#202).
#
# NOTE: tidyselect/lifecycle suppress these deprecation signals for usage that
# originates *inside* a loaded package namespace, so in an ordinary local run
# this test passes whether or not the deprecated patterns are present. Its real
# teeth are under R-devel / `R CMD check` conditions (where #202 surfaced) and
# alongside the suite-wide "WARN 0" expectation. Kept as a CI-oriented guard.

# capture any tidyselect deprecation emitted while evaluating `expr`
tidyselect_deprecations <- function(expr) {
  rlang::local_options(lifecycle_verbosity = "warning")
  deps <- character(0)
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      m <- conditionMessage(w)
      if (grepl("external vector|Use of .data in tidyselect|all_of\\(\\) outside", m)) {
        deps[[length(deps) + 1]] <<- m
      }
      invokeRestart("muffleWarning")
    }
  )
  deps
}

test_that("core test functions emit no tidyselect deprecation (#202)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  deps <- tidyselect_deprecations({
    df %>% anova_test(len ~ dose)
    df %>% anova_test(len ~ supp * dose)
    df %>% cohens_d(len ~ supp)
    df %>% wilcox_effsize(len ~ dose)
    df %>% dunn_test(len ~ dose)
    df %>% games_howell_test(len ~ dose)
    df %>% get_summary_stats(len)
    df %>% get_summary_stats(len, type = "mean_sd")
    df %>% levene_test(len ~ dose)
    df %>% shapiro_test(len)
    aov(len ~ dose, data = df) %>% tukey_hsd()
    df %>% freq_table(supp)
    binom_test(c(10, 90))
    chisq_test(as.table(rbind(c(10, 20, 30), c(15, 25, 20))))
    invisible(NULL)
  })
  expect_equal(deps, character(0))
})

test_that("p-value formatting and label helpers emit no tidyselect deprecation (#202)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  st <- df %>% t_test(len ~ dose)
  deps <- tidyselect_deprecations({
    st %>% p_round(digits = 2)
    st %>% p_format()
    st %>% p_format(new.col = TRUE)
    st %>% p_mark_significant()
    get_test_label(df %>% anova_test(len ~ dose), detailed = TRUE, type = "text")
    rm <- data.frame(id = factor(rep(1:5, 3)),
                     time = factor(rep(c("t1", "t2", "t3"), each = 5)),
                     score = stats::rnorm(15))
    get_test_label(anova_test(rm, dv = score, wid = id, within = time),
                   detailed = TRUE, type = "text")
    st %>% add_xy_position(x = "dose")
    convert_as_factor(df, supp, dose)
    invisible(NULL)
  })
  expect_equal(deps, character(0))
})
