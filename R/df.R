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
  vars <- df_get_var_names(data, ..., vars = vars)
  data %>% select(!!!syms(vars))
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
  groups <- df_get_var_names(data, ..., vars = vars)
  data %>%
    group_by(!!!syms(groups)) %>%
    nest() %>%
    ungroup()
}


#' Split a Data Frame into Subset
#'
#' @description Split a data frame by groups into subsets or data panel. Very
#'   similar to the function \code{\link{df_nest_by}()}. The only difference is
#'   that, it adds label to each data subset. Labels are the combination of the
#'   grouping variable levels. The column holding labels are named "panel".
#' @inheritParams df_nest_by
#' @param label_col column to hold the label of the data subsets. Default column
#'   name is "panel".
#'
#' @return A tbl with one row per unique combination of the grouping variables.
#'   The first columns are the grouping variables, followed by a list column of
#'   tibbles with matching rows of the remaining columns, and a column named
#'   panel, containing labels.
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
df_split_by <- function(data, ..., vars = NULL, label_col = "panel"){
  groups <- df_get_var_names(data, ..., vars = vars)
  data %>%
    df_nest_by(vars = groups) %>%
    add_panel_label(groups, col = label_col) %>%
    mutate(data = map2(.data$data, .data$panel, add_panel_name, col = label_col))
}

# Add panel label to a data
# Labels are the combination of the grouping variable labels
add_panel_label <- function(data, groups, col = "panel") {
  label <- data %>%
    df_select(vars = groups) %>%
    concat_groupname_to_levels(groups, sep = ":") %>%
    df_unite_factors(col = col, vars = groups, sep = ", ") %>%
    pull(!!col)
  data %>% mutate(!!col := label)
}
# Add a column containing panel name
add_panel_name <- function(data, panel, col = "panel") {
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
  vars <- df_get_var_names(data, ..., vars = vars)
  data %>%
    tidyr::unite(
      col = !!col, !!!syms(vars), sep = sep,
      remove = remove, na.rm = FALSE
    )
}

#' @export
#' @describeIn df_unite Unite factor columns. First, order factors levels then
#'   merge them into one column. The output column is a factor.
df_unite_factors <- function(data, col, ..., vars = NULL, sep = "_", remove = TRUE, na.rm = FALSE){
  vars <- df_get_var_names(data, ..., vars = vars)
  data %>%
    dplyr::arrange(!!!syms(vars)) %>%
    df_unite(col = col, vars = vars, sep = sep) %>%
    dplyr::mutate_at(col, function(x){factor(x, levels = unique(x))})
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
