#' @include utilities.R
NULL
#'Autocompute P-value Positions For Plotting Significance
#'@description Compute p-value x and y positions for plotting significance
#'  levels. Many examples are provided at : \itemize{ \item
#'  \href{https://www.datanovia.com/en/blog/how-to-add-p-values-onto-a-grouped-ggplot-using-the-ggpubr-r-package/}{How
#'   to Add P-Values onto a Grouped GGPLOT using the GGPUBR R Package} \item
#'  \href{https://www.datanovia.com/en/blog/ggpubr-how-to-add-adjusted-p-values-to-a-multi-panel-ggplot/}{How
#'   to Add Adjusted P-values to a Multi-Panel GGPlot} \item
#'  \href{https://www.datanovia.com/en/blog/ggpubr-how-to-add-p-values-generated-elsewhere-to-a-ggplot/}{How
#'   to Add P-Values Generated Elsewhere to a GGPLOT} }
#'@inheritParams t_test
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'@param fun summary statistics functions used to compute automatically suitable
#'  y positions of p-value labels and brackets. Possible values include:
#'  \code{"max", "mean", "mean_sd", "mean_se", "mean_ci", "median",
#'  "median_iqr", "median_mad"}.
#'
#'  For example, if \code{fun = "max"}, the y positions are guessed as follow:
#'  \itemize{ \item 1. Compute the maximum of each group (groups.maximum) \item
#'  2. Use the highest groups maximum as the first bracket y position \item 3.
#'  Add successively a step increase for remaining bracket y positions. }
#'
#'  When the main plot is a boxplot, you need the option \code{fun = "max"}, to
#'  have the p-value bracket displayed at the maximum point of the group.
#'
#'  In some situations the main plot is a line plot or a barplot showing the
#'  \code{mean+/-error bars} of the groups, where error can be SE (standard
#'  error), SD (standard deviation) or CI (confidence interval). In this case,
#'  to correctly compute the bracket y position you need the option \code{fun =
#'  "mean_se"}, etc.
#'@param step.increase numeric vector with the increase in fraction of total
#'  height for every additional comparison to minimize overlap.
#'@param y.trans a function for transforming y axis scale. Value can be
#'  \code{log2}, \code{log10} and \code{sqrt}. Can be also any custom function
#'  that can take a numeric vector as input and returns a numeric vector,
#'  example: \code{y.trans = function(x){log2(x+1)}}
#'@param test an object of class \code{rstatix_test} as returned by
#'  \code{\link{t_test}()}, \code{\link{wilcox_test}()},
#'  \code{\link{sign_test}()}, \code{\link{tukey_hsd}()},
#'  \code{\link{dunn_test}()}.
#'@param x variable on x axis.
#'@param group group variable (legend variable).
#'@param dodge dodge width for grouped ggplot/test. Default is 0.8. Used only
#'  when \code{x} specified.
#'@param stack logical. If TRUE, computes y position for a stacked plot. Useful
#'  when dealing with stacked bar plots.
#'@param scales Should scales be fixed (\code{"fixed"}, the default), free
#'  (\code{"free"}), or free in one dimension (\code{"free_y"})?. This option is
#'  considered only when determining the y position. If the specified value is
#'  \code{"free"} or \code{"free_y"}, then the step increase of y positions will
#'  be calculated by plot panels. Note that, using \code{"free"} or
#'  \code{"free_y"} gives the same result. A global step increase is computed
#'  when \code{scales = "fixed"}.
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
                           step.increase = 0.12, y.trans = NULL, stack = FALSE,
                           scales = c("fixed", "free", "free_y")){
  # Estimate step increase
  # 1. Get groups y scale
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group))
    group <- NULL
  if(is_grouped_df(data))
    group <- c(group , dplyr::group_vars(data))
  yscale <- get_y_scale(data, outcome, group, fun, stack = stack)
  # 2. Step increase
  # If fixed scales, then a global step increase is computed,
  # otherwise step.increase is estimated by panel in get_y_position_core().
  scales <- match.arg(scales)
  if(scales == "fixed"){
    step.increase <- step.increase*(yscale$max - yscale$min)
  }
  get_y_position_core(
    data = data, formula = formula, fun = fun, ref.group = ref.group,
    comparisons = comparisons, step.increase = step.increase, y.trans = y.trans,
    stack = stack, scales = scales
    )
}


get_y_position_core <- function(data, formula, fun = "max", ref.group = NULL, comparisons = NULL,
                            step.increase = 0.12, y.trans = NULL, stack = FALSE, scales = "fixed"){
  if(is_grouped_df(data)){
    results <- data %>%
      doo(
        get_y_position_core, formula = formula, fun = fun,
        ref.group = ref.group, comparisons = comparisons,
        step.increase = step.increase, y.trans = y.trans,
        stack = stack, scales = scales
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
  yscale <- get_y_scale(data, y = outcome, group = group, fun = fun, stack = stack)
  if(is.null(step.increase)) step.increase <- yscale$max/20
  else if(scales %in% c("free", "free_y")){
    step.increase <- step.increase*(yscale$max - yscale$min)
  }
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
                           y.trans = NULL, stack = FALSE, scales = c("fixed", "free", "free_y"))
  {
  scales <- match.arg(scales)
  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  args  <- get_test_arguments(test)
  test <- keep_only_tbl_df_classes(test)
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
    step.increase = step.increase, y.trans = y.trans,
    stack = stack, scales = scales
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
get_y_scale <- function(data, y, group, fun = "max", stack = FALSE){
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
  if(stack){
    .center <- rep(sum(.center), length(.center))
  }
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
add_x_position <- function(test, x = NULL, group = NULL, dodge = 0.8){

  # Checking
  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  if(any(test$group1 %in% c("all", ".all."))) {
    # case when ref.group = "all"
    test$group1 <- test$group2
  }
  groups <- c(as.character(test$group1), as.character(test$group2)) %>%
    unique() %>%
    setdiff(c("all", ".all."))   # case when ref.group = "all"

  is_rstatix_test <- inherits(test, "rstatix_test")
  is.null.model <- ("null model" %in% test$group2) & all(test$group1 %in% 1)
  is.grouped.by.legend <- test %>% is_stat_test_grouped_by(group)
  is.grouped.by.x <- test %>% is_stat_test_grouped_by(x)
  is.basic <- !is.grouped.by.x & !is.grouped.by.legend

  # Data preparation
  if(is_rstatix_test) {
    data <- attr(test, "args")$data
    if(is.basic & is.null(x))
      x <- get_formula_right_hand_side(.attributes$args$formula)
    else if(is.grouped.by.x & is.null(group))
      group <- get_formula_right_hand_side(.attributes$args$formula)
  }
  else if(is.basic){
    data <- data.frame(x = groups, stringsAsFactors = FALSE)
    x <- "x"
  }
  else{
    if(is.grouped.by.x)
      data <- expand.grid(x = unique(test[[x]]), group = groups)
    else if(is.grouped.by.legend)
      data <- expand.grid(x = groups, group = test[[group]])
    colnames(data) <- c(x, group)
  }
  if(is.null.model) {
    data$group <- rep(1, nrow(data))
    group <- "group"
  }

  # Add xmin and x max
  if(is.basic){
    x_coords <- as_numeric_group(data[[x]])
    xmin_id <- as.character(test$group1)
    xmax_id <- as.character(test$group2)
  }
  else{
    x_coords <- get_grouped_x_position(data, x = x, group = group, dodge = dodge)
    if(is.grouped.by.legend){
      # Add x position to stat test when the test is grouped by the legend variable
      # Case when you group by legend and pairwise compare between x-axis groups
      xmin_id <- paste(test$group1, test[[group]], sep = "_")
      xmax_id <- paste(test$group2, test[[group]], sep = "_")
    }
    else if(is.grouped.by.x){
      # Add x position to stat test when the test is grouped by the x variable
      # Case when you pairwise compare legend groups at each x-axis position,
      # so the data is grouped by x position
      xmin_id <- paste(test[[x]], test$group1, sep = "_")
      xmax_id <- paste(test[[x]], test$group2, sep = "_")
      test$x <- unname(as_numeric_group(test[[x]]))
    }
  }
  test$xmin <- unname(x_coords[xmin_id])
  test$xmax <- unname(x_coords[xmax_id])
  if(is.null.model) test$xmax <- test$xmin
  test %>% set_test_attributes(.attributes)
}


# Compute grouped x positions or coordinates
# data is a dataframe containing the x and the group columns
get_grouped_x_position<- function(data, x, group, dodge = 0.8){
  data <- data.frame(x = data[[x]], group = data[[group]]) %>%
    dplyr::distinct(.data$x, .data$group) %>%
    dplyr::arrange(.data$x, .data$group)
  data$x.position <- as_numeric_group(data$x)
  # Add group.ranks and ngroups at x position
  data <- data %>%
    rstatix::df_nest_by(vars = "x") %>%
    mutate(
      data = map(.data$data, function(data){data$group.ranks = 1:nrow(data); data}),
      n = unlist(map(.data$data, nrow))
    ) %>%
    tidyr::unnest(cols = "data")
  # Compute x coords
  d <- data
  x_coords <-  (((dodge - dodge*d$n) / (2*d$n)) + ((d$group.ranks - 1) * (dodge / d$n))) + d$x.position
  names(x_coords) <- paste(d$x, d$group, sep = "_")
  x_coords
}

# Check if a stat test is grouped by a given variable
is_stat_test_grouped_by <- function(test, x = NULL){
  answer <- FALSE
  if(!is.null(x)){
    if(x %in% colnames(test)){
      answer <- TRUE
    }
  }
  answer
}

# Return a numeric named vector
# c("a", "b", "a") -> c(a = 1, b = 2, a = 1)
as_numeric_group <- function(x){
  grp <- x %>% as.factor() %>% as.numeric()
  names(grp) <- x
  grp
}




#' @describeIn get_pvalue_position compute and add both x and y positions.
#' @export
add_xy_position <- function(test, x = NULL,  group = NULL, dodge = 0.8, stack = FALSE,
                            fun = "max", step.increase = 0.12,
                            scales = c("fixed", "free", "free_y"), ...){
  if(missing(dodge)){
    if(stack) dodge <- 0
  }
  test %>%
    add_y_position(
      fun = fun, step.increase = step.increase,
      stack = stack, scales = scales, ...
      ) %>%
    add_x_position(x = x, group = group, dodge = dodge)
}


# Helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
asserttat_group_columns_exists <- function(data){
  groups.exist <- all(c("group1", "group2") %in% colnames(data))
  if(!groups.exist){
    stop("data should contain group1 and group2 columns")
  }
}

.is_grouped_test <- function(test, x = NULL){
  answer <- FALSE
  if(!is.null(x)){
    if(x %in% colnames(test)){
      answer <- TRUE
    }
  }
  answer
}

.contains_selected_comparisons <- function(test){
  answer <- FALSE
  if(is_rstatix_test(test)){
    comparisons <- attr(test, "args")$comparisons
    answer <- !is.null(comparisons)
  }
  answer
}


# To be removed
add_x_position0 <- function(test, x = NULL, dodge = 0.8){

  asserttat_group_columns_exists(test)
  .attributes <- get_test_attributes(test)
  group1 <- set_diff(test$group1, c("all", ".all."), keep.dup = TRUE)
  group2 <- set_diff(test$group2, c("all", ".all."), keep.dup = TRUE)
  groups <- unique(c(group1, group2))
  if(.contains_selected_comparisons(test)){
    # get all data groups
    group.var <- get_formula_right_hand_side(.attributes$args$formula)
    groups <- .attributes$args$data %>%
      convert_as_factor(vars = group.var) %>%
      pull(!!group.var) %>%
      levels()
  }
  group1.coords <- group1.ranks <-  match(group1, groups)
  group2.coords <- group2.ranks <- match(group2, groups)
  n <- length(groups)

  if(.is_grouped_test(test, x)){
    test <- test %>%
      .as_factor(x) %>%
      mutate(x = as.numeric(!!sym(x))) # x levels order
    xpos <- test$x
    group1.coords <- (((dodge - dodge*n) / (2*n)) + ((group1.ranks - 1) * (dodge / n))) + xpos
    group2.coords <- (((dodge - dodge*n) / (2*n)) + ((group2.ranks - 1) * (dodge / n))) + xpos
  }

  if(.is_empty(group1)) {
    # case when ref.group = "all"
    group1.coords <- group2.coords
  }
  test %>%
    mutate(xmin = group1.coords, xmax = group2.coords) %>%
    set_test_attributes(.attributes)
}

