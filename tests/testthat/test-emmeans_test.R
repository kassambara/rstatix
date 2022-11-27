
test_that("emmeans_test works", {
  # Data preparation
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  # Pairwise comparisons
  comparisons <- df %>%
    group_by(supp) %>%
    emmeans_test(len ~ dose, p.adjust.method = "bonferroni") %>%
    as.data.frame(stringsAsFactors = FALSE)
  # raw emmeans output
  res_emmeans <- attr(comparisons, "emmeans") %>%
    as.data.frame(stringsAsFactors = FALSE)

  attributes(comparisons) <- list(
    names = colnames(comparisons),
    row.names = row.names(comparisons),
    class = "data.frame"
  )
  attributes(res_emmeans) <- list(
    names = colnames(res_emmeans),
    row.names = row.names(res_emmeans),
    class = "data.frame"
  )

  # Expected values
  expected_comparisons <- tibble::tribble(
     ~supp,  ~term,  ~.y., ~group1, ~group2, ~df,        ~statistic,                   ~p,               ~p.adj, ~p.adj.signif,
      "OJ", "dose", "len",   "0.5",     "1",  54, -5.83122150109434, 3.17564054631384e-07, 9.52692163894153e-07,        "****",
      "OJ", "dose", "len",   "0.5",     "2",  54,  -7.9001659830032, 1.42971201237994e-10, 4.28913603713983e-10,        "****",
      "OJ", "dose", "len",     "1",     "2",  54, -2.06894448190887,   0.0433521450968846,    0.130056435290654,          "ns",
      "VC", "dose", "len",   "0.5",     "1",  54, -5.41250654642231, 1.46293144787886e-06, 4.38879434363658e-06,        "****",
      "VC", "dose", "len",   "0.5",     "2",  54, -11.1821523188884, 1.13067681037436e-15, 3.39203043112308e-15,        "****",
      "VC", "dose", "len",     "1",     "2",  54,  -5.7696457724661, 3.98114048489776e-07, 1.19434214546933e-06,        "****"
     ) %>%
    dplyr::mutate(supp = factor(supp, levels =  c("OJ", "VC"))) %>%
    data.frame(stringsAsFactors = FALSE,  row.names = as.character(1:6))

  expected_emmeans <- tibble::tribble(
     ~supp, ~dose, ~emmean,              ~se, ~df,        ~conf.low,       ~conf.high,        ~method,
      "OJ", "0.5",   13.23, 1.14835308804166,  54, 10.9276906782585, 15.5323093217415, "Emmeans test",
      "OJ",   "1",    22.7, 1.14835308804166,  54, 20.3976906782585, 25.0023093217415, "Emmeans test",
      "OJ",   "2",   26.06, 1.14835308804166,  54, 23.7576906782585, 28.3623093217415, "Emmeans test",
      "VC", "0.5",    7.98, 1.14835308804166,  54, 5.67769067825848, 10.2823093217415, "Emmeans test",
      "VC",   "1",   16.77, 1.14835308804166,  54, 14.4676906782585, 19.0723093217415, "Emmeans test",
      "VC",   "2",   26.14, 1.14835308804166,  54, 23.8376906782585, 28.4423093217415, "Emmeans test"
     ) %>%
    dplyr::mutate(
      supp = factor(supp, levels =  c("OJ", "VC")),
      dose = factor(dose, levels = c("0.5", "1", "2"))
      ) %>%
    data.frame(stringsAsFactors = FALSE, row.names = as.character(1:6))

  # Make sure that he class of grouping variable is preserved
  expect_equal(class(comparisons$supp), "factor")
  expect_equal(comparisons, expected_comparisons, tolerance = 1e-4)
  expect_equal(res_emmeans, expected_emmeans, tolerance = 1e-4)
})
