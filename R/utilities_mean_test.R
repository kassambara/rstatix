
# Comparing means
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Global function to compare means
compare_mean <- function(  data, formula, method = "t.test", paired = FALSE,
                           comparisons = NULL, ref.group = NULL,
                           p.adjust.method = "holm",  ...)
{

  # Formula variables
  formula.variables <- .extract_formula_variables(formula)
  outcome <- formula.variables$outcome
  group <- formula.variables$group

  if(.is_empty(group))
    number.of.groups <- 1  # Null model
  else
    number.of.groups <- data %>%
    pull(group) %>% unique() %>% length()

  if(method %in% c("anova", "kruskal.test") & number.of.groups <= 2)
    stop("The number of groups <= 2; you should use t.test or wilcox.test")

  # Case of one sample test
  if(number.of.groups <= 2){
    mean_test(data, formula, method = method, paired = paired, ...)
  }
  # Pairwise comparisons
  else if(number.of.groups > 2){

    if(method == "anova")
      anova_test(data, formula, ...)
    else if(method == "kruskal.test")
      kruskal_test(data, formula, ...)

    else if(is.null(ref.group))
      mean_test_pairwise(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, ...
      )

    else if(ref.group %in% c("all", ".all."))
      mean_test_one_vs_all (
        data, formula, method = method,
        p.adjust.method = p.adjust.method, ...
      )
    else
      mean_test_pairwise(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, ...
      )
  }

}



# Performs one or two samples mean comparisons
mean_test <- function(data, formula, method = "t.test", ref.group = NULL, ...) {
  if (is_grouped_df(data)) {
    . <- NULL
    res <- data %>%
      do(mean_test(data = ., formula, method = method, ...)) %>%
      ungroup()
    return(res)
  }

  test.function <- match.fun(method)
  test.args <- list()
  grp1 <- grp2 <- NULL

  # Formula variables
  formula.variables <- .extract_formula_variables(formula)
  outcome <- formula.variables$outcome
  group <- formula.variables$group

  # One sample mean comparison =========================
  if (.is_empty(group)) {
    grp1 <- "1"
    grp2 <- "null model"
    outcome.values <- data %>% pull(outcome)
    test.args <- list(x = outcome.values, ...)
  }

  # Two sample mean comparisons ========================
  else {
    # Convert group into factor if this is not already the case
    data <- data %>% .as_factor(group, ref.group = ref.group)
    group.levels <- data %>% pull(group) %>% levels()
    grp1 <- group.levels[1]
    grp2 <- group.levels[2]
    test.args <- list(formula = formula, data = data, ...)
  }

  statistic <- p <- NULL
  res <- suppressWarnings(do.call(test.function, test.args)) %>%
    as_tidy_stat() %>%
    select(statistic, p, method) %>%
    add_column(
      .y. = outcome, group1 = grp1, group2 = grp2,
      .before = "statistic"
    )

  res
}

# Pairwise mean comparisons
mean_test_pairwise <- function(data, formula, method = "t.test",
                               comparisons = NULL, ref.group = NULL,
                               p.adjust.method = "holm", ...) {
  if (is_grouped_df(data)) {
    . <- NULL
    res <- data %>%
      do(
        mean_test_pairwise(
          data = ., formula, method, comparisons,
          ref.group, p.adjust.method, ...
        )
      ) %>%
      ungroup()
    return(res)
  }

  # Formula variables
  formula.variables <- .extract_formula_variables(formula)
  outcome <- formula.variables$outcome
  group <- formula.variables$group

  # Convert group into factor if this is not already the case
  data <- data %>% .as_factor(group, ref.group = ref.group)
  group.levels <- data %>% pull(group) %>% levels()

  # All possible pairwise comparisons
  # if ref.group specified, only comparisons against reference will be kept
  if (is.null(comparisons)) {
    possible.pairs <- group.levels %>% .possible_pairs(ref.group = ref.group)
  } else {
    possible.pairs <- comparisons
  }

  # Perform comparisons
  p <- p.adj <- NULL
  .compare_pair <- function(pair, data, formula, ...) {
    data %>%
      filter(!!sym(group) %in% pair) %>%
      mutate_at(group, droplevels) %>%
      mean_test(formula, method = method, ...)
  }
  possible.pairs %>%
    map(.compare_pair, data, formula, ...) %>%
    bind_rows() %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p") %>%
    add_significance("p.adj") %>%
    mutate(
      p = signif(p, digits = 2),
      p.adj = signif(p.adj, digits = 2)
    )
}

# One vs all mean comparisons
mean_test_one_vs_all <- function(data, formula, method = "t.test", p.adjust.method = "holm", ...) {

  # Case of grouped data by dplyr::group_by
  if (is_grouped_df(data)) {
    . <- NULL
    results <- data %>%
      do(mean_test_one_vs_all(data = ., formula, method, p.adjust.method, ...)) %>%
      ungroup()
    return(results)
  }

  # Formula variables
  formula.variables <- .extract_formula_variables(formula)
  outcome <- formula.variables$outcome
  group <- formula.variables$group

  # Convert group into factor if this is not already the case
  # extract values
  data <- data %>% .as_factor(group)
  outcome.values <- data %>% pull(outcome)
  group.values <- data %>% pull(group)
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
    p.adjust.method = p.adjust.method, ...
  )
}
