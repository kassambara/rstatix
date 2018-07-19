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







