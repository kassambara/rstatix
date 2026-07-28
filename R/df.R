#' @include utilities.R
NULL

#' Select Columns in a Data Frame
#'
#' @description A wrapper around the \code{\link[dplyr]{select}()} function for
#'   selection data frame columns. Supports standard and non standard
#'   evaluations. Usefull to easily program with \code{dplyr}
#' @param data a data frame
#' @param vars a character vector containing the variable names of interest.
#' @param ... One or more unquoted expressions (or variable names) separated by
#'   commas. Used to select a variable of interest.
#'
#' @return a data frame
#' @examples
#' df <- head(ToothGrowth)
#' df
#'
#' # Select column using standard evaluation
#' df %>% df_select(vars = c("dose", "len"))
#'
#' # Select column using non-standard evaluation
#' df %>% df_select(dose, len)
#' @rdname df_select
#' @export
df_select <- function(data, ...,  vars = NULL){
  if(is.null(vars)){
    results <- data %>% select(...)
  }
  else{
    results <- data %>% select(!!!syms(vars))
  }
  results
}

#' Arrange Rows by Column Values
#'
#' @description Order the rows of a data frame by values of specified columns.
#'   Wrapper arround the \code{\link[dplyr]{arrange}()} function. Supports
#'   standard and non standard evaluation.
#' @param data a data frame
#' @param vars a character vector containing the variable names of interest.
#' @param ... One or more unquoted expressions (or variable names) separated by
#'   commas. Used to select a variable of interest. Use
#'   \code{\link[dplyr]{desc}()} to sort a variable in descending order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#'
#' @return a data frame
#' @examples
#' df <- head(ToothGrowth)
#' df
#'
#' # Select column using standard evaluation
#' df %>% df_arrange(vars = c("dose", "len"))
#'
#' # Select column using non-standard evaluation
#' df %>% df_arrange(dose, desc(len))
#' @rdname df_arrange
#' @export
df_arrange <- function(data, ..., vars = NULL, .by_group = FALSE ){
  if(is.null(vars)){
    results <- data %>%
      dplyr::arrange(..., .by_group = .by_group)
  }
  else{
    results <- data %>%
      dplyr::arrange(!!!syms(vars), .by_group = .by_group)
  }
  results
}


#' Group a Data Frame by One or more Variables
#'
#' @description Group a data frame by one or more variables. Supports standard
#'   and non standard evaluation.
#' @inheritParams df_select
#' @examples
#'
#' # Non standard evaluation
#' by_dose <- head(ToothGrowth) %>%
#'    df_group_by(dose)
#' by_dose
#'
#' # Standard evaluation
#' head(ToothGrowth) %>%
#'    df_group_by(vars = c("dose", "supp"))
#' @rdname df_group_by
#' @export
df_group_by <- function(data, ..., vars = NULL){
  if(is.null(vars)){
    results <- data %>% group_by(...)
  }
  else{
    results <- data %>% group_by(!!!syms(vars))
  }
  results
}


#' Nest a Tibble By Groups
#'
#' @description Nest a tibble data frame using grouping specification. Supports standard and non standard evaluation.
#' @param data a data frame
#' @param ... One or more unquoted expressions (or variable names) separated by
#'   commas. Used as grouping variables.
#' @param vars a character vector containing the grouping variables of interest.
#'
#' @return A tbl with one row per unique combination of the grouping variables.
#'   The first columns are the grouping variables, followed by a list column of
#'   tibbles with matching rows of the remaining columns.
#' @examples
#'
#' # Non standard evaluation
#' ToothGrowth %>%
#'  df_nest_by(dose, supp)
#'
#' # Standard evaluation
#' ToothGrowth %>%
#'  df_nest_by(vars = c("dose", "supp"))
#'
#' @rdname df_nest_by
#' @export
df_nest_by <- function(data, ..., vars = NULL){
  data %>%
    df_group_by(..., vars = vars) %>%
    nest() %>%
    ungroup()
}


#' Split a Data Frame into Subset
#'
#' @description Split a data frame by groups into subsets or data panel. Very
#'   similar to the function \code{\link{df_nest_by}()}. The only difference is
#'   that, it adds label to each data subset. Labels are the combination of the
#'   grouping variable levels. The column holding labels are named "label".
#' @inheritParams df_nest_by
#' @inheritParams df_label_both
#' @param labeller A function that takes a data frame, the grouping variables,
#'   label_col and label_sep arguments, and add labels into the data frame.
#'   Example of possible values are: \code{\link{df_label_both}()} and
#'   \code{\link{df_label_value}()}.
#'
#' @return A tbl with one row per unique combination of the grouping variables.
#'   The first columns are the grouping variables, followed by a list column of
#'   tibbles with matching rows of the remaining columns, and a column named
#'   label, containing labels.
#' @examples
#'
#' # Split a data frame
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' # Create a grouped data
#' res <- ToothGrowth %>%
#'   df_split_by(dose, supp)
#' res
#'
#' # Show subsets
#' res$data
#'
#' # Add panel/subset labels
#' res <- ToothGrowth %>%
#'   df_split_by(dose, supp)
#' res
#' @rdname df_split_by
#' @export
df_split_by <- function(data, ..., vars = NULL, label_col = "label",
                        labeller = df_label_both, sep = c(", ", ":")){
  groups <- df_get_var_names(data, ..., vars = vars)
  data %>%
    df_nest_by(vars = groups) %>%
    labeller(vars = groups, label_col = label_col, sep = sep) %>%
    mutate(data = map2(.data$data, .data[[label_col]], add_panel_name, col = label_col))
}

#' Functions to Label Data Frames by Grouping Variables
#'
#' @description Functions to label data frame rows by one or multiple grouping
#'   variables.
#'
#' @inheritParams df_nest_by
#' @param label_col column to hold the label of the data subsets. Default column
#'   name is "label".
#' @param sep String separating labelling variables and values. Should be of
#'   length 2 in the function \code{df_label_both()}. 1) One sep is used to
#'   separate groups, for example ','; 2) The other sep between group name and
#'   levels; for example ':'.
#' @return a modified data frame with a column containing row labels.
#' @examples
#' # Data preparation
#' df <- head(ToothGrowth)
#'
#' # Labelling: Non standard evaluation
#' df %>%
#'   df_label_both(dose, supp)
#'
#' # Standard evaluation
#' df %>%
#'   df_label_both(dose, supp)
#'
#' # Nesting the data then label each subset by groups
#' ToothGrowth %>%
#'   df_nest_by(dose, supp) %>%
#'   df_label_both(supp, dose)
#'
#' @describeIn df_label_value Displays both the variable name and the factor value.
#' @export
df_label_both <- function(data, ..., vars = NULL, label_col = "label", sep = c(", ", ":")){
  vars <- df_get_var_names(data, ..., vars = vars)
  if(length(sep) < 2){
    warning(
      "Argument sep sould be of length 2, otherwise it will be ignored; example: sep = c(', ', ':', )\n",
      "  2. One sep is used to separate groups, for example ','\n",
      "  1. The other sep between group name and levels; for example ':'",
      call. = FALSE
    )
    sep <- c(":", ", ")
  }
  assert_labelling_vars(vars)
  # ungroup first: select() on a grouped_df adds the grouping columns back,
  # which would hand concat_groupname_to_levels() more columns than variable
  # names, and the grouping is irrelevant to a row's own label anyway
  groups <- data %>% dplyr::ungroup() %>% df_select(vars = vars)
  label <- groups %>%
    concat_groupname_to_levels(vars, sep = sep[2]) %>%
    unite_factors_in_place(
      col = label_col, vars = vars, sep = sep[1], order_by = groups
    )
  data %>% mutate(!!label_col := !!label)
}


#' @describeIn df_label_value Displays only the value of a factor.
#' @export
df_label_value <- function(data, ..., vars = NULL, label_col = "label", sep = ", "){
  vars <- df_get_var_names(data, ..., vars = vars)
  assert_labelling_vars(vars)
  label <- data %>%
    dplyr::ungroup() %>%
    df_select(vars = vars) %>%
    unite_factors_in_place(col = label_col, vars = vars, sep = sep[1])
  data %>% mutate(!!label_col := !!label)
}


# Add panel label to a data
# Labels are the combination of the grouping variable labels
add_panel_label <- function(data, groups, col = "label") {
  assert_labelling_vars(groups)
  group.data <- data %>% dplyr::ungroup() %>% df_select(vars = groups)
  label <- group.data %>%
    concat_groupname_to_levels(groups, sep = ":") %>%
    unite_factors_in_place(
      col = col, vars = groups, sep = ", ", order_by = group.data
    )
  data %>% mutate(!!col := !!label)
}
# A label has to be built from at least one grouping variable. Without one the
# label is zero-length, which df_label_both() reported as a dplyr recycling
# error while df_label_value() quietly returned empty strings.
assert_labelling_vars <- function(vars){
  if(length(vars) == 0){
    stop("Specify at least one grouping variable to build the label from.", call. = FALSE)
  }
}
# Unite grouping columns into a labelling factor, one label per input row.
#
# Same output as pulling the united column out of df_unite_factors(), except
# that the labels stay aligned with the rows they were built from:
# df_unite_factors() sorts before uniting, so its column is a permutation of
# the input rows and mutating it back onto the unsorted data mislabels every
# row whose sorted position differs (#324).
#
# The factor levels follow the sorted row order, so they still drive panel
# ordering the way df_unite_factors() does. order_by supplies the columns that
# sort is taken on, for callers that label a transformed copy of the grouping
# variables: df_label_both() pastes the variable name onto the values first,
# and sorting those strings ordered a numeric variable lexicographically and
# ignored a factor's levels (#326). Sorting is left to dplyr::arrange() rather
# than order(), so character collation is unchanged.
unite_factors_in_place <- function(data, col, vars, sep = "_", order_by = NULL){
  if(is.null(order_by)) order_by <- data
  # Both frames must be ungrouped before the dplyr verbs below. row_number()
  # counts within groups, so on a grouped_df the row order would be a per-group
  # index instead of a permutation of the rows; unique() would then see only the
  # first group's labels and factor() would silently turn every other row's
  # label into NA.
  data <- dplyr::ungroup(data)
  order_by <- dplyr::ungroup(order_by)
  labels <- data %>%
    df_unite(col = col, vars = vars, sep = sep) %>%
    pull(!!col)
  row_id <- ".rstatix_row"
  while(row_id %in% vars) row_id <- paste0(row_id, ".")
  row_order <- order_by %>%
    dplyr::mutate(!!row_id := dplyr::row_number()) %>%
    dplyr::arrange(!!!syms(vars)) %>%
    pull(!!row_id)
  factor(labels, levels = unique(labels[row_order]))
}
# Add a column containing panel name
add_panel_name <- function(data, panel, col = "label") {
  data %>% mutate(!!col := !!panel)
}
concat_groupname_to_levels <- function(group.data, groups, sep = ":"){
  purrr::map2(
    group.data, groups,
    function(x, name) {paste(name, x, sep = sep)}
  ) %>%
    as_tibble()
}



#' Unite Multiple Columns into One
#'
#' @description Paste together multiple columns into one. Wrapper arround
#'   \code{\link[tidyr]{unite}()} that supports standard and non standard
#'   evaluation.
#' @inheritParams tidyr::unite
#' @param data a data frame
#' @param col the name of the new column as a string or a symbol.
#' @param ... a selection of columns. One or more unquoted expressions (or variable names) separated by
#'   commas.
#' @param vars a character vector containing the column names of interest.
#' @examples
#' # Non standard evaluation
#' head(ToothGrowth) %>%
#'  df_unite(col = "dose_supp", dose, supp)
#'
#' # Standard evaluation
#' head(ToothGrowth) %>%
#'  df_unite(col = "dose_supp", vars = c("dose", "supp"))
#' @describeIn df_unite Unite multiple columns into one.
#' @export
df_unite <- function(data, col, ..., vars = NULL, sep = "_", remove = TRUE, na.rm = FALSE){
  if(is.null(vars)){
    results <- data %>%
      tidyr::unite(
        col = !!col, ..., sep = sep,
        remove = remove, na.rm = na.rm
      )
  }
  else{
    results <- data %>%
      tidyr::unite(
        col = !!col, !!!syms(vars), sep = sep,
        remove = remove, na.rm = na.rm
      )
  }
  results
}

#' @export
#' @describeIn df_unite Unite factor columns. First, order factors levels then
#'   merge them into one column. The output column is a factor.
df_unite_factors <- function(data, col, ..., vars = NULL, sep = "_", remove = TRUE, na.rm = FALSE){
  vars <- df_get_var_names(data, ..., vars = vars)
  data %>%
    dplyr::arrange(!!!syms(vars)) %>%
    df_unite(col = col, vars = vars, sep = sep, remove = remove, na.rm = na.rm) %>%
    dplyr::mutate_at(dplyr::vars(all_of(col)), function(x){factor(x, levels = unique(x))})
}



#' Get User Specified Variable Names
#'
#' @description Returns user specified variable names. Supports standard and non standard evaluation.
#' @inheritParams df_select
#' @return a character vector
#' @examples
#'
#' # Non standard evaluation
#' ToothGrowth %>%
#'  df_get_var_names(dose, len)
#'
#' # Standard evaluation
#' ToothGrowth %>%
#'  df_get_var_names(vars = c("len", "dose"))
#' @rdname df_get_var_names
#' @export
df_get_var_names <- function(data, ..., vars = NULL){
  dot_vars <- tidyselect::vars_select(colnames(data), !!!rlang::quos(...))
  unique(c(vars, dot_vars))
}
