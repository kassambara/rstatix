#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr extract
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_all
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr ungroup
#' @importFrom dplyr do
#' @importFrom dplyr filter
#' @importFrom dplyr tibble
#' @importFrom dplyr everything
#' @importFrom dplyr left_join
#' @importFrom purrr map map2
#' @importFrom broom tidy
#' @importFrom stats t.test
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang syms
#' @importFrom rlang !!!
#' @importFrom rlang quos
#' @importFrom rlang quo_name
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom tidyr nest


# Unnesting, adapt to tidyr 1.0.0
unnest <- function(data, cols = "data", ...){
  if(is_pkg_version_sup("tidyr", "0.8.3")){
   results <- tidyr::unnest(data, cols = cols, ...)
  }
  else {results <- tidyr::unnest(data, ...)}
  results
}

# Check if an installed package version is superior to a specified version
# Version, pkg: character vector
is_pkg_version_sup<- function(pkg, version){
  vv <- as.character(utils::packageVersion(pkg))
  cc <- utils::compareVersion(vv, version) > 0
  cc
}

# Rounding values --------------------------------------
# Round a vector, conditionnaly rounding
round_value <- function(x, digits = 0){
  sapply(
    x,
    function(x, digits){
      if(is.na(x))
        x
      else if(abs(x) > 10^-digits)
        round(x, digits)
      else
        signif(x, digits)
    },
    digits
  )
}
# Round a whole data frame or selected columns
round_column <- function(data, ...,  digits = 0){
  dot.vars <- get_existing_dot_vars(data, ...)
  if(.is_empty(dot.vars)){
    data %<>% dplyr::mutate_if(is.numeric, round_value, digits = digits)
  }
  data %<>% dplyr::mutate_at(dot.vars, round_value, digits = digits)
  data
}

# Extract or replace number from character string
extract_number <- function(x){
  as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}
replace_number <- function(x, replacement = ""){
  gsub("[0-9.]", replacement, as.character(x))
}

# Add columns into data frame
# If specified before or after columns does not exist, columns are appended at the end
add_columns <- function(.data, ..., .before = NULL, .after = NULL){
  if(is.character(.before)){
    if(!(.before %in% colnames(.data))){
      .before <- NULL
    }
  }
  if(is.character(.after)){
    if(!(.after %in% colnames(.data))){
      .after <- NULL
    }
  }
  tibble::add_column(.data, ..., .before = .before, .after = .after)
}








# Check if required package is installed
required_package <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      pkg, " package needed to be installed before using this function. ",
      "Type this in R: install.packages('", pkg, "')"
    )
  }
}


# Check if a given column name is in the data
assertthat_column_exists <-function(data, cols){
  .diff <- setdiff(cols, colnames(data))
  if(!.is_empty(.diff)){
    stop("Can't find the following variable(s) in the data: ",
         paste(col, collapse = ", "))
  }
}
# remove null elements from a list
remove_null_items <- function(.list){
  Filter(Negate(is.null), .list)
}

# Count a pattern in a string
str_count <- function(x, pattern){
  lengths(regmatches(x, gregexpr(pattern, x)))
}

# Check if all columns in a data frame are numeric
is_all_columns_numeric <- function(data){
  data %>%
    map(is.numeric) %>%
    unlist() %>%
    all()
}

is_lm <- function(object){
  inherits(object, "lm")
}

# Check odd and even numbers
is_odd_number <- function(x){
  x %% 2 != 0
}
is_even_number <- function(x){
  x %% 2 == 0
}


# Set diff that can keep duplicates
set_diff <- function(x, y, keep.dup = FALSE){
  if(!keep.dup)
    res <- setdiff(x, y)
  else{
    ins <- x %in% y
    res <- x[!ins]
  }
  res
}


# NSE
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Get the value of enquo variables. Usage:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# .args <- rlang::enquos(x = x, y = y, ...) %>%
#  map(~get_quo_vars(data, .))
get_quo_vars <- function (data, vars)
{
  if(rlang::quo_is_missing(vars)){
    return(NULL)
  }
  names(data) %>%
    tidyselect::vars_select(!!vars) %>%
    magrittr::set_names(NULL)
}
# .args <- rlang::enquos(x = x, y = y, ...) %>%
#   get_quo_vars_list(data, .)
get_quo_vars_list <- function(data, .enquos){
  . <- NULL
  res <- .enquos %>% map(~get_quo_vars(data, .))
  res <- map(res, set_empty_to_null )
  res
}
# pipe friendly alias of get_quo_vars_list
select_quo_variables <- function(.enquos, data){
  get_quo_vars_list(data, .enquos)
}


set_empty_to_null <- function(x){
  if(.is_empty(x)) x <- NULL
  x
}


# Extract variables used in a formula
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

get_formula_left_hand_side <- function(formula){
  deparse(formula[[2]])
}
get_formula_right_hand_side <- function(formula){
  attr(stats::terms(formula), "term.labels")
}
.extract_formula_variables <- function(formula){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  list(outcome = outcome, group = group)
}

# Get formula error term: len ~ dose*diet + Error(id/diet)
# retruns Error(id/diet)
get_formula_error_term <- function(formula){
  rhs <- get_formula_right_hand_side(formula)
  error.term <- rhs[grepl("^Error\\(", rhs)]
  error.term
}
# Get variables included in the error terms
get_formula_error_vars <- function(formula){
  error.term <- get_formula_error_term(formula)
  all.vars(parse(text = error.term))
}

is_error_term_in_formula <- function(formula){
  error.term <- get_formula_error_term(formula)
  length(error.term) > 0
}

# Grouping variables manipulation
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
guess_number_of_groups <- function(data, group){
  if(.is_empty(group)){
    number.of.groups <- 1  # Null model
  }
  else{
    number.of.groups <- data %>%
      pull(!!group) %>% unique() %>% length()
  }
  number.of.groups
}

get_levels <- function(data, group){
  data %>% pull(!!group) %>% levels()
}


# Convert a group column into a factor if this is not already the case
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# group.col column name containing groups
# ref.group: optional reference group
# ToothGrowth %>% .as_factor("dose", ref.group = "0.5") %>% pull("dose")
.as_factor <- function (data, group.col, ref.group = NULL){
  if(.is_empty(group.col))
    return(data)
  group.values <- data %>% pull(group.col)
  if(!is.factor(group.values))
    group.values <- as.factor(group.values)
  if(!is.null(ref.group)){
    if(ref.group != "")
      group.values <- stats::relevel(group.values, ref.group)
  }
  data %>% mutate(!!group.col := group.values)
}



# Guess p-value column name from a statistical test output
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# transform vector into regular expression
as_regexp <- function(x){
  . <- NULL
  gsub(".", "\\.", x, fixed = TRUE) %>%
    paste(collapse = "$|^") %>%
    paste("^", ., "$", sep = "")
}


# Generate all possible pairs of a factor levels
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# if ref.group is specified, then all possible pairs,
# against reference group, are generated
.possible_pairs <- function(group.levels, ref.group = NULL){
  # Ref group should be always the first group
  if(!is.null(ref.group))
    group.levels <- c(ref.group,  group.levels)
  group.levels <- unique(group.levels)
  # Generate possible pairs
  possible.pairs <- utils::combn(group.levels, 2) %>%
    as.data.frame()
  mate1 <- possible.pairs[1,]
  # select only comparisons against ref.group (if specified)
  if(!is.null(ref.group))
    possible.pairs <- possible.pairs %>%
    select(which(mate1 == ref.group))
  possible.pairs %>% as.list()
}



# Create a tidy statistical output
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Generic function to create a tidy statistical output
as_tidy_stat <- function(x, round.p = TRUE, digits = 3, stat.method = NULL){
  estimate <- estimate1 <- estimate2 <- p.value <-
    alternative <- p <- NULL
  res <- tidy(x)
  if(!is.null(stat.method)){
    res %<>% mutate(method = stat.method)
  }
  else if("method" %in% colnames(res)){
    stat.method <- get_stat_method(x)
    res %<>% mutate(method = stat.method)
  }
  if("p.value" %in% colnames(res)){
    res<- res %>% rename(p = p.value)
    if(round.p) res <- res %>% mutate(p = signif(p, digits))
  }
  if("parameter" %in% colnames(res)){
    res <- res %>% rename(df = .data$parameter)
  }
  res
}

get_stat_method <- function(x){
  if(inherits(x, c("aov", "anova"))){
    return("Anova")
  }
  available.methods <- c(
    "T-test", "Wilcoxon", "Kruskal-Wallis",
    "Pearson", "Spearman", "Kendall", "Sign-Test",
    "Cohen's d", "Chi-squared test"
  )
  used.method <- available.methods %>%
    map(grepl, x$method, ignore.case = TRUE) %>%
    unlist()
  if(sum(used.method) > 0){
    results <- available.methods %>% extract(used.method)
  if(length(results) >= 2)
    results <- paste(results, collapse = " ")
  }
  else
    results <- x$method
  results
}

# Check if en object is empty
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_empty <- function(x){
  length(x) == 0
}

# Check if is a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_list <- function(x){
  inherits(x, "list")
}

# Returns the levels of a factor variable
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.levels <- function(x){
  if(!is.factor(x)) x <- as.factor(x)
  levels(x)
}

# Add/remove items in a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
add_item <- function(.list, ...){
  pms <- list(...)
  for(pms.names in names(pms)){
    .list[[pms.names]] <- pms[[pms.names]]
  }
  .list
}
remove_item <- function(.list, items){
  for(item in items)
    .list[[item]] <- NULL
  .list
}
remove_null_items <- function(.list){
  Filter(Negate(is.null), .list)
}


# depreciated
.add_item <- function(.list, ...){
  add_item(.list, ...)
}



# First letter uppercase
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
to_uppercase_first_letter <- function(x) {
  if(is.null(x)) return(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Data conversion
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

as_matrix <- function(x){

  if(inherits(x, "tbl_df")){
    tibble_to_matrix(x)
  }
  else if(inherits(x, "matrix")){
    x
  }
  else if(is.data.frame(x)){
    if("rowname" %in% colnames(x)){
      x %>%
        tibble::remove_rownames() %>%
        tibble::column_to_rownames("rowname") %>%
        as_matrix()
    }
    else {
      as.matrix(x)
    }
  }
  else{
    as.matrix(x)
  }
}


# Convert a tbl to matrix
tibble_to_matrix <- function(x){
  x <-  as.data.frame(x)
  rownames(x) <- x[, 1]
  x <- x[, -1]
  as.matrix(x)
}

# Convert a matrix to standard data frame
matrix_to_dataframe <- function(x){
  x <- as.data.frame(x, stringsAsFactors = FALSE) %>%
    add_column(rowname = rownames(x), .before = 1)
  rownames(x) <- NULL
  x
}

# Convert a matrix to tibble
matrix_to_tibble <- function(x){
  as_tibble(x, rownames = "rowname")
}

# Replace empty space as na
replace_empty_by <- function(x, replacement = NA){
  x %>%
    keep_only_tbl_df_classes() %>%
    dplyr::mutate_all(
      function(x){x[x==""] <- replacement; x}
      )
}


# Correlation analysis
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Stop if not an object of class cor_mat
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
stop_ifnot_cormat <- function(x){
  if(!inherits(x, "cor_mat")){
    stop("An object of class cor_mat is required")
  }
}

# Subset a correlation matrix, return a tibble
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
subset_matrix <- function(x, vars, row.vars = vars,
                          col.vars = vars){

  if(inherits(x, c("tbl_df", "data.frame"))){
    . <- NULL
    x %>% as_matrix() %>%
      .[row.vars, col.vars, drop = FALSE] %>%
      as_tibble(rownames = "rowname")
  }
  else if(inherits(x, "matrix")){
    x[row.vars, col.vars, drop = FALSE] %>%
      as_tibble(rownames ="rowname")
  }
  else{
    stop("x should be a data frame or rownames")
  }
}


# Tidy Select
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Collect variables provided by users; get selected variables
get_selected_vars <- function(x, ..., vars = NULL){

  if(is_grouped_df(x))
    x <- x %>% dplyr::ungroup()
  dot.vars <- rlang::quos(...)

  if(length(vars) > 0){
    return(vars)
  }
  if (length(dot.vars) == 0) selected <- colnames(x)
  else selected <- tidyselect::vars_select(names(x), !!! dot.vars)
  selected %>% as.character()
}

# Return dot variables -----------------------
get_dot_vars <- function(...){
  rlang::quos(...) %>%
    map(rlang::quo_text) %>%
    unlist()
}
get_existing_dot_vars <- function(data, ...){
  tidyselect::vars_select(colnames(data), !!!rlang::quos(...))
}





# Select numeric columns
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
select_numeric_columns <- function(data){
  if(is_grouped_df(data))
    data <- data %>% dplyr::ungroup()
  data %>% dplyr::select_if(is.numeric)
}

# Add a class to an object
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
add_class <- function(x, .class){
  class(x) <- unique(c(.class, class(x)))
  x
}

prepend_class <- function(x, .class){
  current.class <- class(x)
  diff.class <- setdiff(class(x), .class)
  x <- structure(x, class = c(.class, diff.class))
  x
}

remove_class <- function(x, toremove){
  class(x) <- setdiff(class(x), toremove)
  x
}

keep_only_tbl_df_classes <- function(x){
  toremove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if(length(toremove) > 0){
    x <- remove_class(x, toremove)
  }
  x
}

# Add/set attributes
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
set_attrs <- function (x, ...)
{
  attrs <- list(...)
  attributes(x) <- c(attributes(x), attrs)
  x
}

# Correlation analysis
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Check classes
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.is_cor_mat <- function(x){
  inherits(x, "cor_mat")
}

.is_cor_test <- function(x){
  inherits(x, "cor_test")
}

# Convert a cor_mat_tri to numeric data
as_numeric_triangle <- function(x){
  rrowname <- x %>% pull(1)
  res <- x %>%
    replace_empty_by(NA) %>%
    select(-1) %>%
    mutate_all(as.numeric) %>%
    add_column(rowname = rrowname, .before = 1)
  res
}


# Create label for each row in a grouped data
#::::::::::::::::::::::::::::::::::::::::::::::::
# Labels are the combination of the levels of the grouping variables
# ex: dose:0.5,supp:VC
create_grouped_data_label <- function(data){
  if(!is_grouped_df(data)){
    stop("data should be a grouped data")
  }
  .nested <- nest(data)
  .vars <- dplyr::group_vars(data)
  .data <- .nested %>% select(!!!syms(.vars))
  for(.var in .vars){
    values <- .data %>% pull(!!.var)
    .data  <- .data %>%
      mutate(!!.var := paste0(.var, ":", values))
  }
  .results <- .data %>%
    purrr::pmap(paste, sep = ",") %>%
    unlist()
  .results
}

#::::::::::::::::::::::::::::::::::::::::::::::::
# Helper functions to process rstatix test results
#::::::::::::::::::::::::::::::::::::::::::::::::
is_rstatix_test <- function(x){
  inherits(x, "rstatix_test")
}
# get rstatix test arguments
get_test_arguments <- function(test){
  attr(test, "args")
}

# get test grouping variables
# exist when tests are performed on grouped data
get_test_grouping_vars <- function(test){
  args <- get_test_arguments(test)
  grouping.vars <- dplyr::group_vars(args$data)
  if(.is_empty(grouping.vars))
    grouping.vars <- NULL
  grouping.vars
}
# Get  and set the test attributes: class and attr
# used to propagate attributes
get_test_attributes <- function(test){
  .attributes <- attributes(test)
  .attributes$names <- .attributes$row.names <- NULL
  .attributes
}
set_test_attributes <- function(test, .attributes){
  class(test) <- .attributes$class
  .attributes$class <- NULL
  for (attr.name in names(.attributes)){
    attr(test, attr.name ) <- .attributes[[attr.name]]
  }
  test
}

get_group_size <- function(data, group){
  result <- data %>%
    group_by(!!sym(group)) %>%
    dplyr::count()
  n <- result$n
  group.levels <- result %>% pull(1)
  names(n) <- group.levels
  n
}

stop_ifnot_class <- function(x, .class){
  object.name <- deparse(substitute(x))
  if(!inherits(x, .class)){
    stop(object.name, " should be an object of class: ",
         paste(.class, collapse = ", "))
  }
}

# Allowed pairwise comparison tests
get_pairwise_comparison_methods <- function(){
  c(
    t_test = "T test",
    wilcox_test = "Wilcoxon test",
    sign_test = "Sign test",
    dunn_test = "Dunn test",
    emmeans_test = "Emmeans test",
    tukey_hsd = "Tukey HSD",
    games_howell_test = "Games Howell",
    prop_test = "Z-Prop test",
    fisher_test = "Fisher's exact test",
    chisq_test = "Chi-square test",
    exact_binom_test = "Exact binomial test",
    mcnemar_test = "McNemar test"
  )
}

# Bootstrap confidence intervals -------------------------
get_boot_ci <- function(data, stat.func, conf.level = 0.95, type = "perc", nboot = 500){
  required_package("boot")
  Boot = boot::boot(data, stat.func, R = nboot)
  BCI = boot::boot.ci(Boot, conf = conf.level, type = type, parallel = "multicore")
  type <- switch(
    type, norm = "normal", perc = "percent",
    basic = "basic", bca = "bca", stud = "student", type
  )
  CI <- as.vector(BCI[[type]]) %>%
    utils::tail(2) %>% round_value(digits = 2)
  CI
}



get_complete_cases <- function(data){
  data %>%
    filter(complete.cases(data))
}


# transform squared matrix into tidy data frame
tidy_squared_matrix <- function(data, value = "value"){
  data %>%
    as_tibble(rownames = "group2") %>%
    gather(key = "group1", value = !!value, -.data$group2) %>%
    stats::na.omit() %>% as_tibble() %>%
    select(.data$group1, everything())
}


# Binomial proportion confidence interval
get_prop_conf_int <- function(x, n, p = 0.5, conf.level = 0.95,
                              alternative = "two.sided"){
  .get_conf <- function(x, n, p, alternative, conf.level){
    res <- stats::binom.test(x, n, p, alternative, conf.level)$conf.int
    tibble(conf.low = res[1], conf.high = res[2])
  }
  results <- list(x = x, n = n, p = p) %>%
    purrr::pmap(
      .get_conf, conf.level = conf.level,
      alternative = alternative
    ) %>%
    dplyr::bind_rows()
}
