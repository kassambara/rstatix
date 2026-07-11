
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
        select(all_of(c("Effect", "F", "p"))) %>%
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
two_sample_test <- function(data, formula, method = "t.test", ref.group = NULL, id = NULL, error.as.na = FALSE, detailed = FALSE, ...) {

  if (is_grouped_df(data)) {
    res <- data %>%
      doo(two_sample_test, formula, method = method,
          ref.group = ref.group, id = id, error.as.na = error.as.na, detailed = detailed, ...)
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
    n <- sum(!is.na(outcome.values))
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
    n1 <- sum(!is.na(x))
    n2 <- sum(!is.na(y))
    # Paired test with an explicit subject identifier (#136, #175, #192): align
    # the two groups by `id` so that x[i] and y[i] are the same subject, keeping
    # only subjects measured in BOTH groups (complete pairs, i.e. per-comparison
    # pairwise deletion). Only fires for a paired test with id supplied; the
    # default id = NULL path is unchanged (groups taken in row order as before).
    if(!is.null(id) && isTRUE(list(...)$paired)){
      x <- y <- NULL
      paired.data <- align_paired_by_id(data, outcome, group, id, grp1, grp2)
      x <- paired.data$x
      y <- paired.data$y
      n1 <- n2 <- nrow(paired.data)
    }
    test.args <- list(x = x, y = y, ...)
  }

  statistic <- p <- NULL
  # error.as.na (#208, #158): a comparison can fail because a group has too few
  # observations or the data are essentially constant. By default this stops with
  # an error (unchanged). When error.as.na = TRUE, catch that error, warn (naming
  # the comparison), and return an NA result row so the remaining comparisons /
  # groups are still computed.
  res.raw <- tryCatch(
    suppressWarnings(do.call(test.function, test.args)),
    error = function(e){
      # Only convert genuine "cannot be computed" failures on numeric data (too
      # few observations, essentially constant data) into an NA row. Structural
      # problems such as a non-numeric outcome must still surface as an error,
      # so we re-raise when the inputs are not numeric.
      inputs.numeric <- is.numeric(test.args$x) &&
        (is.null(test.args$y) || is.numeric(test.args$y))
      if(isTRUE(error.as.na) && inputs.numeric){
        comparison <- if(isTRUE(grp2 == "null model")) paste("one-sample:", grp1)
                      else paste(grp1, "vs", grp2)
        warning(
          "Could not compute the comparison (", comparison, "): ",
          conditionMessage(e), ". Returning NA for this comparison.",
          call. = FALSE
        )
        return(NULL)
      }
      stop(e)
    }
  )
  if(is.null(res.raw)){
    # Degenerate comparison turned into an NA row (error.as.na = TRUE). Match the
    # method's normal schema: t.test has a `df` column, wilcox.test does not, so
    # only add `df` for tests that report it (avoids injecting a phantom df column
    # into wilcox_test output).
    res <- tibble(statistic = NA_real_, p = NA_real_)
    if(identical(method, "t.test")){
      res <- tibble(statistic = NA_real_, df = NA_real_, p = NA_real_)
    }
    res <- res %>%
      add_columns(
        .y. = outcome, group1 = grp1, group2 = grp2,
        .before = "statistic"
      )
  }
  else {
  # #127: wilcox.test silently lowers the confidence interval's confidence level
  # when the requested one cannot be achieved (ties / zeroes), which can make the
  # CI contradict the p-value (e.g. p > 0.05 while the CI excludes 0). Surface
  # this clearly. Locale-independent: read the achieved level off the conf.int.
  ci <- res.raw$conf.int
  if(!is.null(ci)){
    achieved  <- attr(ci, "conf.level")
    requested <- test.args$conf.level
    if(is.null(requested)) requested <- 0.95
    if(!is.null(achieved) && isTRUE(achieved < requested)){
      warning(
        "The requested ", round(requested*100), "% confidence interval could ",
        "not be computed (likely due to ties or zeroes); the reported interval ",
        "is at ", round(achieved*100), "% confidence and may be inconsistent ",
        "with the p-value. Interpret the confidence interval with caution.",
        call. = FALSE
      )
    }
  }
  res <- res.raw %>%
    as_tidy_stat() %>%
    add_columns(
      .y. = outcome, group1 = grp1, group2 = grp2,
      .before = "statistic"
    )
  }
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
                               p.adjust.method = "holm", id = NULL, error.as.na = FALSE, detailed = FALSE, ...) {
  if (is_grouped_df(data)) {
    res <- data %>%
      doo(
        pairwise_two_sample_test, formula, method,
        comparisons, ref.group, p.adjust.method,
        id = id, error.as.na = error.as.na, detailed = detailed, ...
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
  res <- compare_pairs(data, formula, comparisons, method, id = id, error.as.na = error.as.na, detailed = detailed, ...) %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance()
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


# Align two groups of a paired test by a subject identifier, keeping only
# subjects present (and non-missing) in BOTH groups (complete pairs). Returns a
# data frame with columns x (grp1 values) and y (grp2 values), row-matched by id.
align_paired_by_id <- function(data, outcome, group, id, grp1, grp2){
  if(!(id %in% colnames(data))){
    stop("The id column '", id, "' was not found in the data.", call. = FALSE)
  }
  outcome.values <- data %>% pull(!!sym(outcome))
  group.values <- as.character(data %>% pull(!!sym(group)))
  id.values <- data %>% pull(!!sym(id))
  keep1 <- group.values == grp1
  keep2 <- group.values == grp2
  # Drop rows with a missing id up front: an unidentified subject (NA id) cannot
  # be matched, and would otherwise be cartesian-joined (dplyr matches NA keys by
  # default), inflating the pair count.
  d1 <- tibble(.id = id.values[keep1], x = outcome.values[keep1]) %>%
    filter(!is.na(.data$.id))
  d2 <- tibble(.id = id.values[keep2], y = outcome.values[keep2]) %>%
    filter(!is.na(.data$.id))
  # A proper paired design has at most one observation per subject and group.
  if(anyDuplicated(d1$.id) > 0 || anyDuplicated(d2$.id) > 0){
    stop(
      "Each id ('", id, "') must be unique within a group for a paired test, ",
      "but duplicated ids were found. Check the data or aggregate replicates ",
      "before testing.", call. = FALSE
    )
  }
  dplyr::inner_join(d1, d2, by = ".id", na_matches = "never") %>%
    filter(!is.na(.data$x), !is.na(.data$y)) %>%
    dplyr::arrange(.data$.id)
}

# compare_pair(ToothGrowth, len ~ dose, c("0.5", "1"))
compare_pair <- function(data, formula, pair, method = "t.test", id = NULL, error.as.na = FALSE, ...){
  group <- get_formula_right_hand_side(formula)
  data %>%
    filter(!!sym(group) %in% pair) %>%
    droplevels() %>%
    two_sample_test(formula, method = method, id = id, error.as.na = error.as.na, ...)
}
# compare_pairs(ToothGrowth, len ~ dose, list(c("0.5", "1"), c("1", "2")))
compare_pairs <- function(data, formula, pairs, method = "t.test", id = NULL, error.as.na = FALSE, ...){
  .f <- function(pair, data, formula, method, ...){
    compare_pair(data, formula, pair, method, id = id, error.as.na = error.as.na, ...)
  }
  pairs %>%
    map(.f, data, formula, method, ...) %>%
    bind_rows()
}



# Effect-size column on a pairwise/two-sample test result
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Left-join a per-metric effect-size column (and its magnitude, when present) onto
# a test result. `es` is the tibble returned by the matching effect-size function
# (cohens_d / cliff_delta), which dispatches through the SAME
# two_sample_test()/pairwise_two_sample_test() engine and therefore carries the
# same identifying columns (any grouping variables, .y., group1, group2, n/n1/n2)
# with the same group1/group2 orientation. Joining on those shared columns keeps
# the estimate on the row it belongs to regardless of row order, so the effect
# size can never be silently misaligned with its p-value. `metric.name` is the
# output column name (e.g. "cohens.d", "cliff.delta").
join_effect_size <- function(res, es, metric.name){
  value.cols <- intersect(c("effsize", "magnitude"), colnames(es))
  keys <- setdiff(colnames(es), value.cols)
  es <- es[, c(keys, value.cols), drop = FALSE]
  # The sibling effect-size functions carry a per-value name on `effsize` (e.g.
  # "Cohen's d"); drop it so the joined column is a plain numeric vector.
  es[["effsize"]] <- unname(es[["effsize"]])
  es <- dplyr::rename(es, !!metric.name := "effsize")
  keep_only_tbl_df_classes(dplyr::left_join(res, es, by = keys))
}

# Remove details from statistical test results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_details <- function(res, method){
  if(method == "anova"){
    # Remove details from ANOVA summary: such as intercept row, Sum Sq columns
    aov.table <- res$ANOVA
    aov.table = aov.table[, names(aov.table) %in% c('Effect','DFn','DFd','F','p','p<.05', 'ges', 'pes', 'conf.low', 'conf.high')]
    intercept.row <- grepl("Intercept", aov.table$Effect)
    res$ANOVA<- aov.table[!intercept.row, ]
  }
  else if(method %in% c("t.test", "wilcox.test", "kruskal.test", "sign.test", "ks.test") ){
    columns.to.keep <- intersect(
      c(".y.", "group1", "group2", "n", "n1", "n2",  "statistic",
        "df", "p", "p.signif", "p.adj", "p.adj.signif"),
      colnames(res)
    )
    res <- res[, columns.to.keep]
  }
  else if(method %in% c("coin.wilcox.test", "cohens.d", "cliff.delta", "rank.biserial")){
    columns.to.remove <- c("p", "p.adj", "p.adj.signif", "p.signif",
                           "statistic", "method", "alternative", "df")
    columns.to.keep  <- setdiff(colnames(res), columns.to.remove)
    res <- res %>% select(!!!syms(columns.to.keep))
  }
  else if(method %in% c("prop.test")){
    columns.to.keep <- intersect(
      c("n",  "group",  "statistic", "df", "p", "p.signif", "p.adj", "p.adj.signif"),
      colnames(res)
    )
    res <- res[, columns.to.keep]
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



