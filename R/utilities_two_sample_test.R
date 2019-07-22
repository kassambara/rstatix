
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
    res <- two_sample_test(data, formula, method = method, paired = paired, ...)
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
      res <- pairwise_two_sample_test(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )

    else if(ref.group %in% c("all", ".all."))
      res <- two_sample_test_one_vs_all (
        data, formula, method = method,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )
    else
      res <- pairwise_two_sample_test(
        data, formula, method = method, paired = paired,
        comparisons = comparisons, ref.group = ref.group,
        p.adjust.method = p.adjust.method, detailed = detailed, ...
      )
  }
  if(!detailed) res <- remove_details(res, method = method)

  res

}



# Performs one or two samples mean comparisons
two_sample_test <- function(data, formula, method = "t.test", ref.group = NULL, detailed = FALSE, ...) {

  if (is_grouped_df(data)) {
    res <- data %>%
      doo(two_sample_test, formula, method = method,
          ref.group = ref.group, detailed = detailed, ...)
    return(res)
  }
  test.function <- method
  test.args <- list()
  grp1 <- grp2 <- NULL
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  # One sample mean comparison =========================
  if (.is_empty(group)) {
    grp1 <- "1"
    grp2 <- "null model"
    outcome.values <- data %>% pull(!!outcome)
    n <- length(outcome.values)
    test.args <- list(x = outcome.values, ...)
  }
  # Two sample mean comparisons ========================
  else {
    # Convert group into factor if this is not already the case
    data <- data %>% .as_factor(group, ref.group = ref.group)
    outcome.values <- data %>% pull(!!outcome)
    group.values <- data %>% pull(!!group)
    group.levels <- data %>% get_levels(group)
    grp1 <- group.levels[1]
    grp2 <- group.levels[2]
    x <- outcome.values[group.values == grp1]
    y <- outcome.values[group.values == grp2]
    n1 <- length(x)
    n2 <- length(y)
    test.args <- list(x = x, y = y, ...)
  }

  statistic <- p <- NULL
  res <- suppressWarnings(do.call(test.function, test.args)) %>%
    as_tidy_stat() %>%
    add_columns(
      .y. = outcome, group1 = grp1, group2 = grp2,
      .before = "statistic"
    )
  # Add n columns
  if(grp2 == "null model"){
    res <- res %>% add_columns(n = n, .before = "statistic")
  }
  else{
    res <- res %>% add_columns(n1 = n1, n2 = n2, .before = "statistic")
  }
  if(!detailed) res <- remove_details(res, method = method)
  res
}

# Pairwise mean comparisons
pairwise_two_sample_test <- function(data, formula, method = "t.test",
                               comparisons = NULL, ref.group = NULL,
                               p.adjust.method = "holm", detailed = FALSE, ...) {
  if (is_grouped_df(data)) {
    res <- data %>%
      doo(
        pairwise_two_sample_test, formula, method,
        comparisons, ref.group, p.adjust.method,
        detailed = detailed, ...
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
    comparisons <- group.levels %>% .possible_pairs(ref.group = ref.group)
  }
  res <- compare_pairs(data, formula, comparisons, method, detailed = detailed, ...) %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance() %>%
    p_round(digits = 3)
 if(!detailed) res <- remove_details(res, method = method)
 res
}

# One vs all mean comparisons -----------------------------------
two_sample_test_one_vs_all <- function(data, formula, method = "t.test", p.adjust.method = "holm", detailed = FALSE, ...) {

  if (is_grouped_df(data)) {
    results <- data %>%
      doo(two_sample_test_one_vs_all, formula, method, p.adjust.method,
          detailed = detailed, ...)
    return(results)
  }
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  new.data <- create_data_with_all_ref_group(data, outcome, group)
  pairwise_two_sample_test(
    data = new.data, formula = formula,
    method = method, ref.group = "all",
    p.adjust.method = p.adjust.method,
    detailed = detailed, ...
  )
}
# Create new data set containing the "all" group level
create_data_with_all_ref_group <- function(data, outcome, group){
  grouping.vars <- grouping.vars.data <- NULL
  if(is_grouped_df(data)){
    grouping.vars <- dplyr::group_vars(data)
    data <- dplyr::ungroup(data)
    grouping.vars.data <- data %>% select(!!!syms(grouping.vars))
  }
  data <- data %>% .as_factor(group)
  outcome.values <- data %>% pull(!!outcome)
  group.values <- data %>% pull(!!group)
  group.levels <- group.values %>% levels()
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
  if(!is.null(grouping.vars)){
    # repeat grouping.vars.data for "all" group
    new.data <- dplyr::bind_rows(grouping.vars.data, grouping.vars.data) %>%
      dplyr::bind_cols(new.data) %>%
      group_by(!!!syms(grouping.vars))
  }
  new.data
}


# compare_pair(ToothGrowth, len ~ dose, c("0.5", "1"))
compare_pair <- function(data, formula, pair, method = "t.test", ...){
  group <- get_formula_right_hand_side(formula)
  data %>%
    filter(!!sym(group) %in% pair) %>%
    droplevels() %>%
    two_sample_test(formula, method = method, ...)
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
  else if(method %in% c("t.test", "wilcox.test", "kruskal.test", "sign.test") ){
    columns.to.keep <- intersect(
      c(".y.", "group1", "group2", "n", "n1", "n2",  "statistic",
        "df", "p", "p.signif", "p.adj", "p.adj.signif"),
      colnames(res)
    )
    res <- res[, columns.to.keep]
  }
  else if(method %in% c("coin.wilcox.test", "cohens.d")){
    columns.to.remove <- c("p", "p.adj", "p.adj.signif", "p.signif",
                           "statistic", "method", "alternative", "df")
    columns.to.keep  <- setdiff(colnames(res), columns.to.remove)
    res <- res %>% select(!!!syms(columns.to.keep))
  }
  else{
    columns.to.remove <- c("n1", "n2", "n", "method", "alternative", "statistic", "df")
    columns.to.keep  <- setdiff(colnames(res), columns.to.remove)
    res <- res %>% select(!!!syms(columns.to.keep))
  }
  res
}

# Two samples tests--------------------------------------
# Check two samples test args
check_two_samples_test_args <- function(x, y = NULL, mu = 0, paired = FALSE, conf.level = 0.5){
  if (!missing(mu) & ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")
  if (!((length(conf.level) == 1L) & is.finite(conf.level) &
        (conf.level > 0) & (conf.level < 1)))
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.numeric(x))
    stop("'x' must be numeric")
  if (!is.null(y)) {
    if (!is.numeric(y))
      stop("'y' must be numeric")
    if (paired) {
      if (length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    }
  }
  else {
    if (paired)
      stop("'y' is missing for paired test")
  }
  if (length(x) < 1L)
    stop("not enough (finite) 'x' observations")
}



