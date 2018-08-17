#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr ungroup
#' @importFrom dplyr do
#' @importFrom dplyr filter
#' @importFrom dplyr tibble
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom stats t.test
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread



# Extract variables used in a formula
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.extract_formula_variables <- function(formula){
  outcome <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")
  list(outcome = outcome, group = group)
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
.as_tidy_stat <- function(x){
  p.value <- NULL
  x %>%
    tidy() %>%
    as_data_frame() %>%
    rename(p = p.value)
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
.add_item <- function(.list, ...){
  pms <- list(...)
  for(pms.names in names(pms)){
    .list[[pms.names]] <- pms[[pms.names]]
  }
  .list
}


# First letter uppercase
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
to_uppercase_first_letter <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Data conversion
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Convert a tbl to matrix
.tibble_to_matrix <- function(x){
  x <-  as.data.frame(x)
  rownames(x) <- x[, 1]
  x <- x[, -1]
  as.matrix(x)
}

# Convert a matrix to standard data frame
.matrix_to_dataframe <- function(x){
  x <- as.data.frame(x) %>%
    add_column(name = rownames(x), .before = 1)
  rownames(x) <- NULL
  x
}

# Convert a matrix to tibble
.matrix_to_tibble <- function(x){
  .matrix_to_dataframe(x) %>%
    as_tibble()
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

# Spread a result from cor_test function
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# x: cor_test results;
# value: column containing the value to spread
spread_cor_test <- function(x, value = "cor"){

  # if(!.is_cor_test(x)){
  #   stop("x should be an object of class cor_test")
  # }

  var1 <- var2 <- cor <- p <- NULL
  vars <- x %>% pull(var2) %>% unique()

  res <- x %>%
    select(var1, var2, !!value) %>%
    spread(key = "var1", value = value) %>%
    .respect_variables_order(vars)
  colnames(res)[1] <- "name"

  res
}

# Reorder a correlation matrix according
# to the order of variables in vars
.respect_variables_order <- function(x, vars){
  subset_cor_mat(x, vars)
}








