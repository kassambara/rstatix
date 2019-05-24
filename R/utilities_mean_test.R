
# Comparing means
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Global function to compare means
compare_mean <- function(  data, formula, method = "t.test", paired = FALSE,
                           comparisons = NULL, ref.group = NULL,
                           p.adjust.method = "holm", detailed = FALSE, ...)
{

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)

  if(method %in% c("anova", "kruskal.test") & number.of.groups <= 2)
    stop("The number of groups <= 2; you should use t.test or wilcox.test")

  # Case of one sample test
  if(number.of.groups <= 2){
    res <- mean_test(data, formula, method = method, paired = paired, ...)
  }
  # Pairwise comparisons
  else if(number.of.groups > 2){

    if(method == "anova"){
      res <- anova_test(data, formula, ...) %>%
        select(.data$Effect, .data$F, .data$p) %>%
        set_colnames(c("term", "statistic", "p")) %>%
        add_column(method = "Anova", .after = "p") %>%
        add_column(.y. = outcome, .before = "term") %>%
        as_tibble()
    }
    else if(method == "kruskal.test")
      res <- kruskal_test(data, formula, ...)

    else if(is.null(ref.group))
      res <- mean_test_pairwise(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )

    else if(ref.group %in% c("all", ".all."))
      res <- mean_test_one_vs_all (
        data, formula, method = method,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )
    else
      res <- mean_test_pairwise(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )
  }
  if(!detailed) res <- remove_details(res, method = method)

  res

}



# Performs one or two samples mean comparisons
mean_test <- function(data, formula, method = "t.test", ref.group = NULL, detailed = FALSE, ...) {

  if (is_grouped_df(data)) {
    res <- data %>%
      doo(mean_test, formula, method = method, ...)
    return(res)
  }

  test.function <- match.fun(method)
  test.args <- list()
  grp1 <- grp2 <- NULL
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)

  # One sample mean comparison =========================
  if (.is_empty(group)) {
    grp1 <- "1"
    grp2 <- "null model"
    outcome.values <- data %>% pull(!!outcome)
    test.args <- list(x = outcome.values, ...)
  }
  # Two sample mean comparisons ========================
  else {
    # Convert group into factor if this is not already the case
    data <- data %>% .as_factor(group, ref.group = ref.group)
    group.levels <- data %>% get_levels(group)
    grp1 <- group.levels[1]
    grp2 <- group.levels[2]
    test.args <- list(formula = formula, data = data, ...)
  }

  statistic <- p <- NULL
  res <- suppressWarnings(do.call(test.function, test.args)) %>%
    as_tidy_stat() %>%
    add_column(
      .y. = outcome, group1 = grp1, group2 = grp2,
      .before = "statistic"
    )
  if(!detailed) res <- remove_details(res, method = method)
  res
}

# Pairwise mean comparisons
mean_test_pairwise <- function(data, formula, method = "t.test",
                               comparisons = NULL, ref.group = NULL,
                               p.adjust.method = "holm", detailed = FALSE, ...) {
  if (is_grouped_df(data)) {
    res <- data %>%
      doo(
        mean_test_pairwise, formula, method,
        comparisons, ref.group, p.adjust.method, ...
        )
    return(res)
  }

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  data <- data %>% .as_factor(group, ref.group = ref.group)
  group.levels <- data %>% get_levels(group)
  # All possible pairwise comparisons
  # if ref.group specified, only comparisons against reference will be kept
  if (is.null(comparisons)) {
    possible.pairs <- group.levels %>% .possible_pairs(ref.group = ref.group)
  } else {
    possible.pairs <- comparisons
  }
  # Perform comparisons
  p <- p.adj <- NULL
  res <- compare_pairs(data, formula, possible.pairs, method, ...) %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(
      p = signif(p, digits = 3),
      p.adj = signif(p.adj, digits = 3)
    )
if(!detailed) res <- remove_details(res, method = method)
}

# One vs all mean comparisons
mean_test_one_vs_all <- function(data, formula, method = "t.test", p.adjust.method = "holm", detailed = FALSE, ...) {

  if (is_grouped_df(data)) {
    results <- data %>%
      doo(mean_test_one_vs_all, formula, method, p.adjust.method, ...)
    return(results)
  }

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  data <- data %>% .as_factor(group)
  outcome.values <- data %>% pull(!!outcome)
  group.values <- data %>% pull(!!group)
  group.levels <- group.values %>% levels()

  # Create new data set containing the "all" group level
  all.data <- tibble(
    outcome = outcome.values,
    group = "all"
  )
  source.data <- tibble(
    outcome = outcome.values,
    group = as.character(group.values)
  )
  new.data <- all.data %>%
    bind_rows(source.data) %>%
    mutate(group = factor(group, levels = c("all", group.levels)))
  colnames(new.data) <- c(outcome, group)

  mean_test_pairwise(
    data = new.data, formula = formula,
    method = method, ref.group = "all",
    p.adjust.method = p.adjust.method,
    detailed = detailed, ...
  )
}


# compare_pair(ToothGrowth, len ~ dose, c("0.5", "1"))
compare_pair <- function(data, formula, pair, method = "t.test", ...){
  group <- get_formula_right_hand_side(formula)
  data %>%
    filter(!!sym(group) %in% pair) %>%
    droplevels() %>%
    mean_test(formula, method = method, ...)
}
# compare_pairs(ToothGrowth, len ~ dose, list(c("0.5", "1"), c("1", "2")))
compare_pairs <- function(data, formula, pairs, method = "t.test", ...){
  .f <- function(pair, data, formula, method, ...){
    compare_pair(data, formula, pair, method, ...)
  }
  pairs %>%
    map(.f, data, formula, method, ...) %>%
    bind_rows()
}



# Remove details from statistical test results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_details <- function(res, method){
  if(method == "anova"){
    # Remove details from ANOVA summary: such as intercept row, Sum Sq columns
    aov.table <- res$ANOVA
    aov.table = aov.table[, names(aov.table) %in% c('Effect','DFn','DFd','F','p','p<.05', 'ges', 'pes')]
    intercept.row <- grepl("Intercept", aov.table$Effect)
    res$ANOVA<- aov.table[!intercept.row, ]
  }
  else if(method %in% c("t.test", "wilcox.test", "kruskal.test") ){
    columns.to.keep <- intersect(
      c(".y.", "group1", "group2", "statistic", "df", "p", "p.signif", "p.adj", "p.adj.signif"),
      colnames(res)
    )
    res <- res[, columns.to.keep]
  }
  else{
    stop("Don't support the method : ", method)
  }
  res
}

