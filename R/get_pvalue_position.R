#' @include utilities.R
NULL
#'Autocompute P-value Positions For Plotting Significance
#'@description Compute p-value x and y positions for plotting significance
#'  levels.
#'@inheritParams t_test
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'@param fun summary statistics function. Possible values include: \code{"max",
#'  "mean", "mean_sd", "mean_se", "mean_ci", "median", "median_iqr",
#'  "median_mad"}.
#'
#'  \code{"max"} is suitable when you want to add, for example, p-value on box
#'  plots. The remaining functions are suitable for bar plots and line plots
#'  showing mean plots +/- error bars.
#'@param step.increase numeric vector with the increase in fraction of total
#'  height for every additional comparison to minimize overlap.
#'@param y.trans a function for transforming y axis scale. Value can be
#'  \code{log2}, \code{log10} and \code{sqrt}. Can be also any custom function
#'  that can take a numeric vector as input and retourne a numeric vector,
#'  example: \code{y.trans = function(x){log2(x+1)}}
#'@param test an object of class \code{rstatix_test} as returned by
#'  \code{\link{t_test}()}, \code{\link{wilcox_test}()},
#'  \code{\link{sign_test}()}, \code{\link{tukey_hsd}()},
#'  \code{\link{dunn_test}()}.
#'@param x variable on x axis.
#'@param dodge dodge width for grouped ggplot/test. Default is 0.8. Used only
#'  when \code{x} specified.
#'
#' @examples
#' # Data preparation
#' #::::::::::::::::::::::::::::::::::::
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' df$group <- factor(rep(c(1, 2), 30))
#' head(df)
#'
#' # Stat tests
#' #::::::::::::::::::::::::::::::::::::
#' stat.test <- df %>%
#'   t_test(len ~ dose)
#' stat.test
#'
#' # Add the test into box plots
#' #::::::::::::::::::::::::::::::::::::
#' stat.test <- stat.test %>%
#'   add_y_position()
#' \donttest{
#'  if(require("ggpubr")){
#'    ggboxplot(df, x = "dose", y = "len") +
#'      stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)
#'   }
#' }

#'@describeIn get_pvalue_position compute the p-value y positions
#'@export
get_y_position <- function(data, formula, fun = "max", ref.group = NULL, comparisons = NULL,
                           step.increase = 0.12, y.trans = NULL){
  # Estimate step increase
  # 1. Get groups y scale
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group))
    group <- NULL
  if(is_grouped_df(data))
    group <- c(group , dplyr::group_vars(data))
  yscale <- get_y_scale(data, outcome, group, fun)
  # 2. Step increase
  step.increase <- step.increase*(yscale$max - yscale$min)
  get_y_position_core(
    data = data, formula = formula, fun = fun, ref.group = ref.group,
    comparisons = comparisons, step.increase = step.increase, y.trans = y.trans
    )
}


get_y_position_core <- function(data, formula, fun = "max", ref.group = NULL, comparisons = NULL,
                            step.increase = 0.12, y.trans = NULL){
  if(is_grouped_df(data)){
    results <- data %>%
      doo(
        get_y_position_core, formula = formula, fun = fun,
        ref.group = ref.group, comparisons = comparisons,
        step.increase = step.increase, y.trans = y.trans
        )
    return(results)
  }

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  # Possible comparisons between groups
  if(is.null(comparisons)){
    if(.is_empty(group)){
      comparisons <- list(c("1", "null model"))
    }
    else{
      comparisons <- data %>%
        get_comparisons(!!group, !!ref.group)
    }
  }
  ncomparisons <- length(comparisons)
  group1 <- comparisons %>% get_group(1)
  group2 <- comparisons %>% get_group(2)
  k <- 1.08

  # Estimate y axis scale
  yscale <- get_y_scale(data, y = outcome, group = group, fun = fun)
  if(is.null(step.increase)) step.increase <- yscale$max/20
  # ystart <- k*yscale$max
  ystart <- yscale$max + step.increase
  yend <- ystart + (step.increase*ncomparisons)

  if(is.null(ref.group)) ref.group <- ""
  if(ref.group %in% c("all", ".all.")){
   # y.position <- yscale$y*k
    y.position <- yscale$y + step.increase
  }
  else{
    y.position <- seq(
      from = ystart, to = yend,
      length.out = ncomparisons
    )
  }
  if(!is.null(y.trans)) y.position <- y.trans(y.position)
  results <- tibble(group1, group2, y.position) %>%
    mutate(groups = combine_this(group1, group2))
  results
}



#' @describeIn get_pvalue_position add p-value y positions to an object of class \code{rstatix_test}
#' @export
add_y_position <- function(test, fun = "max", step.increase = 0.12,
                           data = NULL, formula = NULL,  ref.group = NULL, comparisons = NULL,
                           y.trans = NULL)
  {
  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  args  <- get_test_arguments(test)
  if(!is.null(args)){
    if(missing(data)) data <- args$data
    if(missing(formula)) formula <- args$formula
    if(missing(ref.group)) ref.group <- args$ref.group
    if(missing(comparisons)) comparisons <- args$comparisons
  }
  if(is.null(data) | is.null(formula)){
    stop("data and formula arguments should be specified.")
  }
  positions <- get_y_position(
    data = data, formula = formula, fun = fun,
    ref.group = ref.group, comparisons = comparisons,
    step.increase = step.increase, y.trans = y.trans
  )
  if(nrow(test) == nrow(positions)){
    test$y.position <- positions$y.position
    test$groups <- positions$groups
  }
  else{
    # this occurs when tests are grouped by two variables (for ex),
    # but y positions are grouped by one variable
    # merging positions and test data frame
    if("y.position" %in% colnames(test)){
      test <- test %>% select(-.data$y.position)
    }
    if("groups" %in% colnames(test)){
      test <- test %>% select(-.data$groups)
    }
    common.columns <- intersect(colnames(test), colnames(positions))
    test <- test %>% dplyr::left_join(positions, by = common.columns)
  }

  test %>%
    set_test_attributes(.attributes)
}

# Compute y scale depending on fun
get_y_scale <- function(data, y, group, fun = "max"){
  if(!.is_empty(group)){
    desc.stat <- data %>%
      group_by(!!!syms(group)) %>%
      get_summary_stats(!!y, type = fun)
  }
  else {
    desc.stat <- data %>%
      get_summary_stats(!!y, type = fun)
  }
  # Add error bars positions if any
  fun.splitted <- unlist(strsplit(fun, "_", fixed = TRUE))
  .center <- fun.splitted[1]
  .error <- ifelse(length(fun.splitted) == 2, fun.splitted[2], 0)
  .center <- desc.stat %>% pull(!!.center)
  if(.error != 0) .error <- desc.stat %>% pull(!!.error)
  y <- .center + .error
  ymax <- max(y, na.rm = TRUE)
  ymin <- min(y, na.rm = TRUE)
  list(y = y, max = ymax, min = ymin)
}


# Return group1 and group2 values from possible pairs
get_group <- function(possible.pairs, index){
  possible.pairs %>%
    map( function(x){as.character(x[index])}) %>%
    unlist()
}
# Combine vectors: c(a, b) & c(c, d) --> list(c(a, c), c(b, d))
# ... two or more character vectors
combine_this <- function(...){
  params <- list(...)
  purrr::pmap(params, c)
}


#' @describeIn get_pvalue_position compute and add p-value x positions.
#' @export
add_x_position <- function(test, x = NULL, dodge = 0.8){
  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  group1 <- set_diff(test$group1, c("all", ".all."), keep.dup = TRUE)
  group2 <- set_diff(test$group2, c("all", ".all."), keep.dup = TRUE)
  groups <- unique(c(group1, group2))
  group1.coords <- group1.ranks <-  match(group1, groups)
  group2.coords <- group2.ranks <- match(group2, groups)
  n <- length(groups)
  # Grouped test
  if(!is.null(x)){
    if(x %in% colnames(test)){
      test <- test %>%
        .as_factor(x) %>%
        mutate(x = as.numeric(!!sym(x))) # x levels order
      xpos <- test$x
      group1.coords <- (((dodge - dodge*n) / (2*n)) + ((group1.ranks - 1) * (dodge / n))) + xpos
      group2.coords <- (((dodge - dodge*n) / (2*n)) + ((group2.ranks - 1) * (dodge / n))) + xpos
    }
  }
  if(.is_empty(group1)) {
    # case when ref.group = "all"
    group1.coords <- group2.coords
  }
  test %>%
    mutate(xmin = group1.coords, xmax = group2.coords) %>%
    set_test_attributes(.attributes)
}

#' @describeIn get_pvalue_position compute and add both x and y positions.
#' @export
add_xy_position <- function(test, x = NULL,  dodge = 0.8, fun = "max", step.increase = 0.12, ...){
  test %>%
    add_y_position(fun = fun, step.increase = step.increase, ...) %>%
    add_x_position(x = x, dodge = dodge)
}


# Helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_group_level_xcoord <- function(xpos, groups, dodge = 0.8){
  n <- length(groups)
  xpos <- rep(xpos, n)
  group.rank <- match(groups, groups)
  (((dodge - dodge*n) / (2*n)) + ((group.rank - 1) * (dodge / n))) + xpos
}


asserttat_group_columns_exists <- function(data){
  groups.exist <- all(c("group1", "group2") %in% colnames(data))
  if(!groups.exist){
    stop("data should contain group1 and group2 columns")
  }
}


# Helper, not used
#_____________________________________________________________
add_pvalue_x_position <- function(test, x,  span = 0.2, hjust = 0){
  if(!inherits(test, "rstatix_test")){
    stop("An object of class `rstatix_test` is required")
  }
  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  args  <- get_test_arguments(test)
  grouping.vars <- get_test_grouping_vars(test)
  if(is.null(grouping.vars))
    stop("Grouped test required!")
  if(missing(x)) x <- grouping.vars[1]
  test <- test %>%
    .as_factor(x) %>%
    mutate(x = as.numeric(!!sym(x))) %>% # x levels order
    group_by(!!!syms(grouping.vars)) %>%
    doo(add_group_levels_position, span = span, hjust = hjust)
  test %>%
    set_test_attributes(.attributes) %>%
    add_class("grouped_test") # interpreted by ggpubr::stat_pvalue_manual
}
add_group_levels_position <- function(test, span = 0.2, hjust = 0){
  # x variable position and grouping variable groups
  pos <- unique(test$x)
  # Position of each group levels on the plot
  groups <- unique(c(test$group1, test$group2))
  levels.pos <- get_group_level_xpos(pos, groups, span = span)
  test <- test %>% mutate(
    xmin = levels.pos[test$group1] + hjust,
    xmax = levels.pos[test$group2] + hjust
  )
  test
}
# Position of each group levels on the plot
get_group_level_xpos <- function(pos, groups, span = 0.2){
  ngroup <- length(groups)
  if(is_even_number(ngroup))
    ngroup <- ngroup +1
  positions <- rep(pos, ngroup)
  ranks <- 1:length(positions)
  median.rank <- median(ranks)
  coords <- list()
  for(i in 1:ngroup){
    coords[[i]] <- (ranks[i] - median.rank)*span
  }
  coords <- unlist(coords) + pos
  if(is_even_number(ngroup))
    coords <- coords [-median.rank]
  names(coords) <- groups
  coords
}


