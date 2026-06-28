context("test-cor_test")

test_that("cor_test reports df for Pearson (#107)", {
  res <- mtcars %>% cor_test(mpg, wt)
  expect_true("df" %in% colnames(res))
  # the df column carries the name "df" (consistent with t_test's df column), so compare values
  expect_equal(unname(res$df), as.integer(nrow(mtcars) - 2))                 # n - 2
  expect_equal(unname(res$df), unname(as.integer(cor.test(mtcars$mpg, mtcars$wt)$parameter)))
  # df sits next to statistic
  expect_equal(colnames(res),
               c("var1", "var2", "cor", "statistic", "df", "p",
                 "conf.low", "conf.high", "method"))
})

test_that("cor_test has no df column for Spearman/Kendall (#107 no-regression)", {
  expect_false("df" %in% colnames(mtcars %>% cor_test(mpg, wt, method = "spearman")))
  expect_false("df" %in% colnames(mtcars %>% cor_test(mpg, wt, method = "kendall")))
})

test_that("grouped Pearson cor_test reports df per group (#107)", {
  g <- iris %>% group_by(Species) %>% cor_test(Sepal.Width, Sepal.Length)
  expect_true("df" %in% colnames(g))
  expect_true(all(g$df == 48L))   # 50 per species - 2
})

test_that("cor_mat / cor_pmat are unaffected by the df column (#107 no-regression)", {
  cm <- mtcars %>% cor_mat(mpg, disp, hp)
  expect_false("df" %in% colnames(cm))
  expect_equal(colnames(cm), c("rowname", "mpg", "disp", "hp"))
  expect_true(is.data.frame(mtcars %>% cor_pmat(mpg, disp, hp)))
})

test_that("cor_test with external character vectors does not warn (deprecation) (#202)", {
  # force lifecycle deprecations to always warn (not once-per-session)
  rlang::local_options(lifecycle_verbosity = "warning")
  matrix_vars  <- c("mpg", "disp")
  matrix_vars2 <- c("hp", "wt")
  w <- capture_warnings(
    res <- cor_test(mtcars, vars = matrix_vars, vars2 = matrix_vars2)
  )
  expect_false(any(grepl("external vector", w)))   # the tidyselect deprecation is gone
  expect_equal(nrow(res), 4L)                       # 2 vars x 2 vars2
  expect_equal(res$var1, c("mpg", "mpg", "disp", "disp"))
  expect_equal(res$var2, c("hp", "wt", "hp", "wt"))

  # inline character vectors behave the same
  w2 <- capture_warnings(
    res2 <- cor_test(mtcars, vars = c("mpg", "disp"), vars2 = c("hp", "wt"))
  )
  expect_false(any(grepl("external vector", w2)))
  expect_equal(res2[, c("var1", "var2")], res[, c("var1", "var2")])
})

test_that("cor_test selection is unchanged for bare names and tidyselect helpers (#202)", {
  # bare names
  bare <- cor_test(mtcars, mpg, disp)
  expect_equal(nrow(bare), 1L)
  expect_equal(c(bare$var1, bare$var2), c("mpg", "disp"))
  # external character vector selects exactly the same variables as bare names
  v <- cor_test(mtcars, vars = c("mpg", "disp"))
  expect_equal(sort(unique(c(v$var1, v$var2))), sort(c("disp", "mpg")))
  # tidyselect helper still works
  helper <- cor_test(mtcars, dplyr::starts_with("d"))
  expect_setequal(unique(c(helper$var1, helper$var2)), c("disp", "drat"))
})

test_that("get_quo_vars: a bare column name is never shadowed by an env object (#202)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  df$grp  <- as.character(df$supp)
  dose <- "supp"   # an env object that shadows the column name
  # bare symbol must resolve to the COLUMN, not the env object
  expect_equal(rstatix:::get_quo_vars(df, rlang::quo(dose)), "dose")
  # bare character column resolves to its name (not its values)
  expect_equal(rstatix:::get_quo_vars(df, rlang::quo(grp)), "grp")
  # external character vector is still selected by name (the #202 path)
  v <- c("len", "dose")
  expect_equal(rstatix:::get_quo_vars(df, rlang::quo(v)), c("len", "dose"))
})

test_that("anova_test bare grouping var is not shadowed by an env object (#202)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  dose <- "supp"   # shadowing object in scope
  res <- anova_test(df, dv = len, between = dose)
  expect_true("dose" %in% res$Effect)
  expect_false("supp" %in% res$Effect)
})

test_that("cor_mat with external character vectors does not warn (#202)", {
  rlang::local_options(lifecycle_verbosity = "warning")
  keep <- c("mpg", "disp", "hp")
  w <- capture_warnings(res <- cor_mat(mtcars, vars = keep))
  expect_false(any(grepl("external vector", w)))
  expect_equal(res$rowname, keep)
})
