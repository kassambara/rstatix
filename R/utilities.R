#' @importFrom magrittr %>%
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
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom stats t.test
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang syms
#' @importFrom rlang !!!
#' @importFrom rlang set_attrs
#' @importFrom rlang quos
#' @importFrom rlang quo_name
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr unnest


# Conditionnaly rounding
roundif <- function(x, digits = 3){
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
  group.values <- data %>% pull(group.col)
  if(!is.factor(group.values))
    group.values <- as.factor(group.values)
  if(!is.null(ref.group))
    group.values <- stats::relevel(group.values, ref.group)
  data %>% mutate(!!group.col := group.values)
}



# Guess p-value column name from a statistical test output
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.guess_pvalue_column <- function(data){
  matches <- dplyr::matches
  common.p.cols <- "^p$|^pval$|^pvalue$|^p\\.val$|^p\\.value$"
  p.col <- data %>%
    select(matches(common.p.cols)) %>%
    colnames()
  if(.is_empty(p.col))
    stop("Can't guess the p value column from the input data. Specify the p.col argument")
  p.col
}

# Generate all possible pairs of a factor levels
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# if ref.group is specified, then all possible pairs,
# against reference group, are generated
.possible_pairs <- function(group.levels, ref.group = NULL){

  # Ref group should be always the first group
  if(!is.null(ref.group))
    group.levels <- c(ref.group,  group.levels) %>% unique()
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
as_tidy_stat <- function(x, digits = 3){

  stat.method <- get_stat_method(x)

  estimate <- estimate1 <- estimate2 <- p.value <-
    alternative <- NULL
  res <- x %>%
    tidy() %>%
    mutate(
      p.value = signif(p.value, digits),
      method = stat.method
    ) %>%
    rename(p = p.value)
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
    "Pearson", "Spearman", "Kendall"
  )
  used.method <- available.methods %>%
    map(grepl, x$method, ignore.case = TRUE) %>%
    unlist()
  available.methods %>% extract(used.method)
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

# Additems in a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
add_item <- function(.list, ...){
  pms <- list(...)
  for(pms.names in names(pms)){
    .list[[pms.names]] <- pms[[pms.names]]
  }
  .list
}
# depreciated
.add_item <- function(.list, ...){
  add_item(.list, ...)
}



# First letter uppercase
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
to_uppercase_first_letter <- function(x) {
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
  x %>% dplyr::mutate_all(
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

# Return dot variables
get_dot_vars <- function(...){
  rlang::quos(...) %>%
    map(rlang::quo_text) %>%
    unlist()
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
  for(.cl in .class){
    if(!inherits(x, .cl))
      x <- structure(x, class = c(class(x), .cl))
  }
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




