context("test-doo")

test_that("doo keeps heterogeneous per-group results in a list-column (#83)", {
  # Regression for #83: a grouped repeated-measures anova_test() can return a
  # plain data frame for one group (no sphericity correction) and a list (ANOVA +
  # Mauchly + sphericity corrections) for others. doo() previously decided whether
  # to unnest from the FIRST element only, then crashed in unnest() trying to
  # combine a data.frame with a list. It must instead keep a list-column.
  df <- data.frame(g = rep(c("a", "b"), each = 4), x = c(1, 2, 3, 4, 5, 6, 7, 8))
  # nest() drops the grouping column, so discriminate on the nested data: group
  # "a" -> data frame result, group "b" -> list result
  f <- function(d){
    if(d$x[1] <= 4) tibble::tibble(v = 1) else list(tab = 1:3)
  }
  res <- df %>% dplyr::group_by(g) %>% doo(f)
  expect_true(".results." %in% colnames(res))   # list-column kept, no crash
  expect_equal(nrow(res), 2L)
  expect_true(is.data.frame(res$.results.[[1]]))
  expect_true(is.list(res$.results.[[2]]) && !is.data.frame(res$.results.[[2]]))
})

test_that("doo still unnests homogeneous data-frame results (#83 no-regression)", {
  res <- ToothGrowth %>%
    dplyr::group_by(dose) %>%
    doo(~broom::tidy(t.test(len ~ supp, data = .)))
  expect_false(".results." %in% colnames(res))          # unnested as before
  expect_true(all(c("estimate", "p.value") %in% colnames(res)))
  expect_equal(nrow(res), 3L)
})

test_that("get_anova_table() handles a grouped rm-ANOVA with mixed corrections (#83)", {
  # End-to-end: a grouped one-way repeated-measures ANOVA where the per-group
  # results differ in whether a sphericity correction is produced. Build one group
  # whose within scores are (near) perfectly linear across blocks (so the
  # sphericity machinery degenerates) and others with ordinary variation.
  set.seed(123)
  k <- 4; n <- 8
  make_group <- function(name, linear){
    block <- factor(rep(seq_len(n), each = k))
    w <- factor(rep(seq_len(k), times = n))
    y <- if(linear) as.numeric(w) + rep(seq_len(n), each = k) * 0.01
         else rnorm(n * k) + as.numeric(w) * 0.4
    data.frame(g = name, id = interaction(name, block), w = w, y = y)
  }
  df <- rbind(make_group("g1", TRUE), make_group("g2", FALSE), make_group("g3", FALSE))
  res <- df %>%
    dplyr::group_by(g) %>%
    anova_test(dv = y, wid = id, within = w)
  tab <- get_anova_table(res)        # must not error and must cover all 3 groups
  expect_equal(nrow(tab), 3L)
  expect_setequal(as.character(tab$g), c("g1", "g2", "g3"))
  expect_false(any(is.na(tab$F)))
  expect_false("data" %in% colnames(tab))
})
