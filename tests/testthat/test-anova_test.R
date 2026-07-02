context("test-anova_test")

test_that("Checking one-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>% anova_test(len ~ dose)
  expect_equal(res.aov$Effect, "dose")
  expect_equal(res.aov$DFn, 1)
  expect_equal(res.aov$DFd, 58)
  expect_equal(res.aov$F, 105.065)
  expect_equal(res.aov$ges, 0.644)
})

test_that("Checking grouped one-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>%
    group_by(supp) %>%
    anova_test(len ~ dose)
  expect_equal(res.aov$Effect, c("dose", "dose"))
  expect_equal(res.aov$DFn, c(1, 1))
  expect_equal(res.aov$DFd, c(28, 28))
  expect_equal(res.aov$F, c(36.013, 117.948))
  expect_equal(res.aov$ges, c(0.563, 0.808))
})


test_that("Checking two-way ANOVA test", {
  data("ToothGrowth")
  res.aov <- ToothGrowth %>% anova_test(len ~ supp*dose)
  expect_equal(res.aov$Effect, c("supp", "dose", "supp:dose"))
  expect_equal(res.aov$DFn, c(1, 1, 1))
  expect_equal(res.aov$DFd, c(56, 56, 56))
  expect_equal(res.aov$F, c(12.317, 133.415, 5.333))
  expect_equal(res.aov$p, c(8.94e-04, 1.91e-16, 2.50e-02))
  expect_equal(res.aov$ges, c(0.180, 0.704, 0.087))
})


test_that("Checking repeated measures ANOVA test", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
  anova.table <- res.aov$ANOVA
  sphericity <- res.aov$`Mauchly's Test for Sphericity`
  corrections <- res.aov$`Sphericity Corrections`
  expect_equal(anova.table$Effect, c("supp", "dose", "supp:dose"))
  expect_equal(anova.table$DFn, c(1, 2, 2))
  expect_equal(anova.table$DFd, c(9, 18, 18))
  expect_equal(anova.table$F, c(34.866, 106.470, 2.534))
  expect_equal(anova.table$p, c(2.28e-04, 1.06e-10, 1.07e-01))
  expect_equal(anova.table$ges, c(0.224, 0.773, 0.132))
  expect_equal(sphericity$W, c(0.807, 0.934))
  expect_equal(corrections$GGe, c(0.838, 0.938))
  expect_equal(corrections$HFe, c(1.008, 1.176))
})


test_that("Checking that get_anova_table works with any data frame", {
  data("ToothGrowth")
  expect_is(get_anova_table(ToothGrowth), "data.frame")
})

test_that("Checking that get_anova_table works for grouped repeated measures ANOVA", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>%
    group_by(supp) %>%
    anova_test(dv = len, wid = id, within = dose)
  aov.table <- get_anova_table(res.aov)
  expect_equal(aov.table$F, c(23.936, 57.783))
})


test_that("Checking that get_anova_table performs auto sphericity correction", {
  data("ToothGrowth")
  df <- ToothGrowth
  df$id <- rep(1:10, 6)
  res.aov <- df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
  res.aov2 <- res.aov
  res.aov2$`Mauchly's Test for Sphericity`$p[1] <- 0.05 # significant
  # Correction not applied, because there is not significant sphericity test
  auto <- get_anova_table(res.aov, correction = "auto")
  expect_equal(auto$DFn, c(1, 2, 2))
  expect_equal(auto$DFd, c(9, 18, 18))
  expect_equal(auto$F, c(34.866, 106.470, 2.534))
  # Correction automatically applied to the DF of the effect where sphericity is signiica,t
  auto2 <- get_anova_table(res.aov2, correction = "auto")
  expect_equal(auto2$DFn, c(1, 1.68, 2))
  expect_equal(auto2$DFd, c(9, 15.09, 18))
  expect_equal(auto2$F, c(34.866, 106.470, 2.534))
  # Check that GG correction works for all within-subject variables
  gg <- get_anova_table(res.aov2, correction = "GG")
  expect_equal(gg$DFn, c(1, 1.68, 1.88))
  expect_equal(gg$DFd, c(9, 15.09, 16.88))
  expect_equal(gg$p, c(2.28e-04, 2.79e-09, 1.12e-01))
})

test_that("anova_test gives a well-formed error for a single-level factor (#137)", {
  d <- data.frame(id = factor(1:6), grp = factor(rep("a", 6)), score = c(1, 2, 3, 4, 5, 6))
  expect_error(
    anova_test(d, dv = score, wid = id, between = grp),
    "has only one level"
  )
  # regression for the missing space (#137): must read "grp has", not "grphas"
  msg <- tryCatch(
    anova_test(d, dv = score, wid = id, between = grp),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "Variable grp has only one level")
})

test_that("anova_test results are dplyr-compatible: rstatix_test before data.frame (#106)", {
  g <- ToothGrowth %>% group_by(supp) %>% anova_test(len ~ dose)
  # class order: the subclasses must come before data.frame (vctrs/dplyr requirement)
  expect_lt(match("rstatix_test", class(g)), match("data.frame", class(g)))
  # grouped: filter / mutate must work (previously errored "must be a vector")
  expect_true(is.data.frame(g %>% dplyr::filter(p < 1)))
  expect_true(is.data.frame(g %>% dplyr::mutate(z = p)))
  # ungrouped and get_anova_table() output too
  u <- ToothGrowth %>% anova_test(len ~ dose)
  expect_true(is.data.frame(u %>% dplyr::filter(p < 1)))
  expect_true(is.data.frame(get_anova_table(g) %>% dplyr::filter(p < 1)))
  # repeated-measures get_anova_table() (with sphericity correction) is dplyr-compatible too
  set.seed(1)
  rm <- data.frame(id = factor(rep(1:10, 3)), time = factor(rep(c("t1","t2","t3"), each = 10)),
                   score = c(rnorm(10, 5), rnorm(10, 6), rnorm(10, 8)))
  rm.aov <- anova_test(rm, dv = score, wid = id, within = time)
  expect_true(is.data.frame(get_anova_table(rm.aov, correction = "GG") %>% dplyr::filter(p < 1)))
  # dispatch is preserved by the reorder
  expect_true(inherits(u, "anova_test"))
  expect_true(inherits(g, "grouped_anova_test"))
})

test_that("anova_test class contract: rstatix_test first, specific class at [2] (#283 revdep)", {
  # Reverse dependencies (e.g. GimmeMyStats, GimmeMyPlot) dispatch on class()[2]
  # (method <- sub('_test', '', class(x)[2])). The class vector must keep
  # 'rstatix_test' first (dplyr/vctrs, #106) AND the specific test class at
  # position 2, so class()[2] resolves to the test name -- do not silently
  # reorder these again (that broke revdeps in the 1.0.0 pretest).
  u <- ToothGrowth %>% anova_test(len ~ dose)
  expect_identical(class(u), c("rstatix_test", "anova_test", "data.frame"))
  expect_identical(class(u)[2], "anova_test")            # class[2] -> "anova"
  # grouped output follows the same contract
  g <- ToothGrowth %>% group_by(supp) %>% anova_test(len ~ dose)
  expect_identical(class(g)[1], "rstatix_test")
  expect_identical(class(g)[2], "grouped_anova_test")
  # #106 invariant restated: rstatix_test strictly before data.frame
  expect_lt(match("rstatix_test", class(u)), match("data.frame", class(u)))
  # the exact downstream idiom must resolve to the intended method
  method <- sub("_test", "", class(u)[2])
  method <- ifelse(method == "data.frame", "anova", method)
  expect_identical(method, "anova")
})

test_that("add_xy_position on an anova_test gives an informative error, not a class error (#111)", {
  g <- ToothGrowth %>% group_by(supp) %>% anova_test(len ~ dose)
  # omnibus ANOVA has no pairwise comparisons: needs a pairwise post-hoc instead
  expect_error(add_xy_position(g), "group1 and group2")
})

test_that("anova_test effect.size = c('pes','ges') returns BOTH columns (#180)", {
  both <- iris %>%
    anova_test(Sepal.Length ~ Sepal.Width + Species, effect.size = c("pes", "ges")) %>%
    get_anova_table()
  expect_true(all(c("pes", "ges") %in% colnames(both)))
  # both-call values match the standalone single-effect-size computations (no regression)
  ges <- iris %>% anova_test(Sepal.Length ~ Sepal.Width + Species, effect.size = "ges") %>% get_anova_table()
  pes <- iris %>% anova_test(Sepal.Length ~ Sepal.Width + Species, effect.size = "pes") %>% get_anova_table()
  expect_equal(both$ges, ges$ges)
  expect_equal(both$pes, pes$pes)
  # single-effect-size output is unchanged: exactly one of the two columns
  expect_false("pes" %in% colnames(ges))
  expect_false("ges" %in% colnames(pes))
})

test_that("repeated-measures anova_test gives clear errors for degenerate designs (#216, #146, #116, #134, #102)", {
  set.seed(1)
  good <- data.frame(
    id = factor(rep(1:8, 3)),
    time = factor(rep(c("t1", "t2", "t3"), each = 8)),
    y = rnorm(24)
  )
  # valid balanced design still works (no false positive)
  expect_silent(suppressMessages(anova_test(good, dv = y, wid = id, within = time)))
  # a subject with >1 observation per within-cell -> clear "duplicated cells" message
  dup <- rbind(good, good[1, ])
  expect_error(anova_test(dup, dv = y, wid = id, within = time), "duplicated cells")
  # wid not repeated across within levels (no complete subject) -> clear message
  notrep <- data.frame(
    id = factor(1:18),
    grp = factor(rep(c("w1", "w2", "w3"), each = 6)),
    y = rnorm(18)
  )
  expect_error(anova_test(notrep, dv = y, wid = id, within = grp), "complete set")
  # a single incomplete subject is NOT rejected (no false positive)
  expect_silent(suppressMessages(anova_test(good[-1, ], dv = y, wid = id, within = time)))
})
