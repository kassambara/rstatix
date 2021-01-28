context("test-add_x_position")

# Data preparation
data("ToothGrowth")
df <- ToothGrowth
df$dose <- as.factor(df$dose)

test_that("add_x_position works for any data frame with group1 and group2 cols", {
  stat.test <- data.frame(
    stringsAsFactors = FALSE,
    group1 = c("0.5", "0.5", "1"),
    group2 = c("1", "2", "2"),
    p = c(1.27e-07, 4.4e-14, 1.91e-05)
  ) %>%
    add_x_position()
  expect_equal(stat.test$xmin, c(1, 1, 2))
  expect_equal(stat.test$xmax, c(2, 3, 3))
})


test_that("add_x_position works for rstatix in a basic ggplot setting", {
  stat.test <- df %>%
    t_test(len ~ supp) %>%
    add_x_position()
  expect_equal(stat.test$xmin, 1)
  expect_equal(stat.test$xmax, 2)
})

test_that("add_x_position works for rstatix in a basic ggplot facet setting", {
  stat.test <- df %>%
    group_by(dose) %>%
    t_test(len ~ supp) %>%
    add_x_position(x = "supp")
  expect_equal(stat.test$xmin, c(1, 1, 1))
  expect_equal(stat.test$xmax, c(2, 2, 2))
})

test_that("add_x_position works for comparison against reference groups", {
  stat.test <- df %>%
    t_test(len ~ dose, ref.group = "0.5") %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 1))
  expect_equal(stat.test$xmax, c(2, 3))
})

test_that("add_x_position works for comparison against all (basemean)", {
  stat.test <- df %>%
    t_test(len ~ dose, ref.group = "all") %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 2, 3))
  expect_equal(stat.test$xmax, c(1,2, 3))
})

test_that("add_x_position works for comparison against null (one-sample test)", {
  stat.test <- df %>%
    group_by(dose) %>%
    t_test(len ~ 1) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$x, c(1, 2, 3))
})


test_that("add_x_position works for specified comparisons of interest", {
  stat.test <- df %>%
    t_test(len ~ dose, comparisons = list(c("0.5", "1"), c("0.5", "2"))) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 1))
  expect_equal(stat.test$xmax, c(2,3))
})

test_that("add_x_position works for grouped plots: grouping by x-var and performing test between legend groups", {
  stat.test <- df %>%
    group_by(dose) %>%
    t_test(len ~ supp) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$x, c(1, 2, 3))
  expect_equal(stat.test$xmin, c(0.8, 1.8, 2.8))
  expect_equal(stat.test$xmax, c(1.2, 2.2, 3.2))
})



test_that("add_x_position works for grouped plots: grouping by legend-var and performing test between x-group", {
  stat.test <- df %>%
    group_by(supp) %>%
    t_test(len ~ dose) %>%
    add_x_position(x = "dose")
  expect_equal(stat.test$xmin, c(1, 1, 2, 1, 1, 2))
  expect_equal(stat.test$xmax, c(2, 3, 3, 2, 3, 3))
})


test_that("Grouped pairwise tests: grouping by x-var and performing test between legend groups", {
  stat.test <- df %>%
    group_by(supp) %>%
    t_test(len ~ dose) %>%
    add_x_position(x = "supp", dodge = 0.8)
  expect_equal(stat.test$x, c(1, 1, 1, 2, 2, 2))
  expect_equal(round(stat.test$xmin, 2), c(0.73, 0.73, 1, 1.73, 1.73, 2))
  expect_equal(round(stat.test$xmax, 2), c(1, 1.27, 1.27, 2, 2.27, 2.27))
})

test_that("Grouped pairwise tests: grouping by x-var and performing test between legend groups using ref.group", {
  stat.test <- df %>%
    group_by(supp) %>%
    t_test(len ~ dose, ref.group = "0.5") %>%
    add_x_position(x = "supp", dodge = 0.8)
  expect_equal(stat.test$x, c(1, 1, 2, 2))
  expect_equal(round(stat.test$xmin, 2), c(0.73, 0.73, 1.73, 1.73))
  expect_equal(round(stat.test$xmax, 2), c(1, 1.27, 2, 2.27))
})


test_that("Grouped plots: test that add_x_position works with different number of groups at each x pos.", {
  # https://github.com/kassambara/ggpubr/issues/326
  demo_data <- data.frame(
    stringsAsFactors = FALSE,
    Study = c("A","A","A","A","A","A",
              "A","A","A","A","B","B","B","B","B","B","B","B",
              "B","B","C","C","C","C","C","C","C","C","C",
              "C","C","C","C","C","C","D","D","D","D","D",
              "D","D","D","D","D","D","D","D","D","D"),
    Studytype = c("X","X","X","X","X","X",
                  "X","X","X","X","X","X","X","X","X","X","X","X",
                  "X","X","Y","Y","Y","Y","Y","Y","Y","Y","Y",
                  "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
                  "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"),
    Values = c(4469L,4797L,5101L,5397L,
               4542L,2780L,4326L,3396L,3657L,3199L,9221L,10176L,
               9277L,10500L,9707L,7406L,7756L,7601L,7586L,7353L,
               1811L,1485L,3003L,1629L,2495L,4207L,4265L,3629L,
               4157L,3495L,2075L,2112L,2973L,3086L,2943L,5664L,6690L,
               3538L,5741L,7880L,5848L,6390L,6569L,6114L,6520L,
               7389L,6843L,7611L,6621L,7340L),
    Group = as.factor(c("CTR",
                        "CTR","CTR","CTR","CTR","Dis1","Dis1","Dis1",
                        "Dis1","Dis1","CTR","CTR","CTR","CTR",
                        "CTR","Dis1","Dis1","Dis1","Dis1","Dis1",
                        "CTR","CTR","CTR","CTR","CTR","Dis2","Dis2",
                        "Dis2","Dis2","Dis2","Dis3","Dis3",
                        "Dis3","Dis3","Dis3","CTR","CTR","CTR","CTR",
                        "CTR","Dis2","Dis2","Dis2","Dis2","Dis2",
                        "Dis3","Dis3","Dis3","Dis3","Dis3"))
  )

  stat.test <- demo_data %>%
    group_by(Study) %>%
    wilcox_test(Values ~ Group, ref.group = "CTR") %>%
    add_significance("p")
  stat.test <- stat.test %>%
    add_xy_position(x = "Study", dodge = 0.8)

  #bxp <- ggpubr::ggboxplot(demo_data, x = "Study", y = "Values", fill = "Group") +
  #ggpubr::stat_pvalue_manual(stat.test, label = "p")

  stat.test$x <- round(stat.test$x, 2)
  stat.test$xmin <- round(stat.test$xmin, 2)
  stat.test$xmax <- round(stat.test$xmax, 2)
  expect_equal(stat.test$x, c(1, 2, 3, 3, 4, 4))
  expect_equal(stat.test$xmin, c(0.8, 1.8, 2.73, 2.73, 3.73, 3.73))
  expect_equal(stat.test$xmax, c(1.2, 2.2, 3, 3.27, 4, 4.27))
})
