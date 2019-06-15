#' @include utilities.R
NULL
#'Create a List of Possible Comparisons Between Groups
#'@description Create a list of possible pairwise comparisons between groups. If
#'  a reference group is specified, only comparisons against reference will be
#'  kept.
#'@param data a data frame
#'@param variable the grouping variable name. Can be unquoted.
#'@param ref.group a character string specifying the reference group. Can be
#'  unquoted. If numeric, then it should be quoted. If specified, for a
#'  given grouping variable, each of the group levels will be compared to the
#'  reference group (i.e. control group).
#'
#'  If \code{ref.group = "all"}, pairwise comparisons are performed between each
#'  grouping variable levels against all (i.e. basemean).
#'@return a list of all possible pairwise comparisons.
#'@examples
#' # All possible pairwise comparisons
#' ToothGrowth %>%
#'   get_comparisons("dose")
#'
#' # Comparisons against reference groups
#' ToothGrowth %>%
#'   get_comparisons("dose", ref.group = "0.5")
#'
#' # Comparisons against  all (basemean)
#' ToothGrowth %>%
#'   get_comparisons("dose", ref.group = "all")
#'@export
get_comparisons <- function(data, variable, ref.group = NULL){
  group <- rlang::enquo(variable) %>% rlang::as_name()
  ref.group <- rlang::enquo(ref.group)
  if(rlang::quo_is_null(ref.group)) ref.group <- NULL
  else ref.group <- rlang::as_name(ref.group)
  group.levels <- data %>%
    .as_factor(group) %>%
    get_levels(group)
  asserttat_ref_group_iscorrect(group.levels, ref.group)
  comparisons <- c(ref.group, group.levels) %>%
    unique() %>%
    .possible_pairs(ref.group = ref.group) %>%
    map(as.character)
  comparisons
}

asserttat_ref_group_iscorrect <- function(.levels, .ref){
  if(!is.null(.ref)){
    .diff <- setdiff(.ref, c("all", ".all.", .levels))
    if(!.is_empty(.diff))
      stop("ref.group is incorrect")
  }
}
