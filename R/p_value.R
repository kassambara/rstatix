#' @include utilities.R
NULL
#'Rounding and Formatting p-values
#'
#'@description Round and format p-values. Can also mark significant p-values with stars.
#'@param x a numeric vector of p-values or a data frame containing a p value
#'  column. If data frame, the p-value column(s) will be automatically detected.
#'  Known p-value column names can be obtained using the functions
#'  \code{p_names()} and \code{p_adj_names()}
#'@param digits the number of significant digits to be used.
#'@param accuracy number to round to, that is the threshold value above wich the
#'  function will replace the pvalue by "<0.0xxx".
#'@param decimal.mark the character to be used to indicate the numeric decimal
#'  point.
#'@param leading.zero logical. If FALSE, remove the leading zero.
#'@param trailing.zero logical. If FALSE (default), remove the training extra
#'  zero.
#'@param space logical. If TRUE (default) use space as separator between
#'  different elements and symbols.
#'@param cutpoints numeric vector used for intervals
#'@param symbols character vector, one shorter than cutpoints, used as
#'  significance symbols.
#'@param add.p logical value. If TRUE, add "p=" before the value.
#'@param ... column names to manipulate in the case where \code{x} is a data
#'  frame. P value columns are automatically detected if not specified.
#'@param new.col logical, used only when \code{x} is a data frame. If TRUE, add
#'  a new column to hold the results. The new column name is created by adding,
#'  to the p column, the suffix "format" (for \code{p_format()}), "signif" (for
#'  \code{p_mak_significant()}).
#'@return a vector or a data frame containing the rounded/formatted p-values.
#' @examples
#'
#' # Round and format a vector of p-values
#' # ::::::::::::::::::::::::::::::::::::::::::::
#' # Format
#' p <- c(0.5678, 0.127, 0.045, 0.011, 0.009, 0.00002, NA)
#' p_format(p)
#'
#'# Specify the accuracy
#' p_format(p, accuracy = 0.01)
#'
#' # Add p and remove the leading zero
#' p_format(p, add.p = TRUE, leading.zero = FALSE)
#'
#' # Remove space before and after "=" or "<".
#' p_format(p, add.p = TRUE, leading.zero = FALSE, space = FALSE)
#'
#' # Mark significant p-values
#' # ::::::::::::::::::::::::::::::::::::::::::::
#' p_mark_significant(p)
#'
#' # Round, the mark significant
#' p %>% p_round(digits = 2) %>% p_mark_significant()
#'
#' # Format, then mark significant
#' p %>% p_format(digits = 2) %>% p_mark_significant()
#'
#' # Perform stat test, format p and mark significant
#' # ::::::::::::::::::::::::::::::::::::::::::::
#' ToothGrowth %>%
#'   group_by(dose) %>%
#'   t_test(len ~ supp) %>%
#'   p_format(digits = 2, leading.zero = FALSE) %>%
#'   p_mark_significant()
#'
#'@describeIn p_value round p-values
#'@export
p_round <- function(x, ..., digits = 3){
  if(is.numeric(x)){
    round_value(x, digits = digits)
  }
  else if(is.data.frame(x)){
    p_round_at(x, ..., digits = digits)
  }
  else{
    stop("x should be a numeric vector or a data frame")
  }
}

#' @describeIn p_value format p-values. Add a symbol "<" for small p-values.
#' @export
p_format <- function(x, ..., new.col = FALSE, digits = 2, accuracy = 0.0001, decimal.mark = ".",
                     leading.zero = TRUE, trailing.zero = FALSE,
                     add.p = FALSE, space = FALSE){
  if(is.data.frame(x)){
    .attributes <- attributes(x)
    res <- x %>%
      keep_only_tbl_df_classes() %>%
      p_format_at(
      ..., new.col = new.col, digits = digits, accuracy = accuracy,
      decimal.mark = decimal.mark, leading.zero = leading.zero,
      trailing.zero = trailing.zero, add.p = add.p, space = space
      )
    .attributes$names <- colnames(res)
    attributes(res) <- .attributes
    return(res)
  }
  res <- format.pval(
    pv = x,
    digits = digits, eps = accuracy,
    # nsmall = how much tails 0 to keep if digits of
    # original value < to digits defined
    nsmall = 0
  )
  res <- gsub(pattern = " ", replacement = "", res, fixed = TRUE)
  res <- gsub("<1e-04", "<0.0001", res)
  if(!leading.zero)
    res <- remove_leading_zero(res)
  if(!trailing.zero)
    res <- remove_trailing_zero(res)
  if(!missing(decimal.mark))
    res <- gsub("\\.", decimal.mark, res)
  if(add.p){
    contain.inf.symbol <- grepl("<", res)
    res2 <- paste0("p", "=", res)
    if(sum(contain.inf.symbol) > 0){
      # no need to add =
      res2[contain.inf.symbol] <- paste0("p", res[contain.inf.symbol])
    }
    res <- res2
  }
  if(space){
    if(add.p)
      res <- gsub(pattern = "(=|<)", replacement = " \\1 ", x = res)
    else
      res <- gsub(pattern = "(=|<)", replacement = "\\1 ", x = res)
  }
  res
}

#' @describeIn p_value mark p-values with significance levels
#' @export
p_mark_significant <- function(x,  ..., new.col = FALSE, cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1),
                               symbols = c("****", "***", "**", "*", "")){
  if(is.data.frame(x)){
    .attributes <- attributes(x)
    res <- x %>%
      keep_only_tbl_df_classes() %>%
      p_mark_significant_at(
      ..., new.col = new.col, cutpoints = cutpoints,
      symbols = symbols
    )
    attributes(res) <- .attributes
    return(res)
  }
  contains.leading.zero <- TRUE
  is.char.x <- is.character(x)
  if(is.char.x){
    contains.leading.zero <- p_contains_leading_zero(x)
    leading.character <- replace_number(x, "")
    leading.character <- gsub("NA", "", leading.character)
    x <- extract_number(x)
  }
  x <- tibble(p = x) %>%
    add_significance("p", cutpoints = cutpoints, symbols = symbols) %>%
    mutate(.signif = paste0(.data$p, .data$p.signif)) %>%
    pull(".signif")
  if(!contains.leading.zero)
    x <- remove_leading_zero(x)
  if(is.char.x)
    x <- paste(leading.character, x, sep = "")
  x <- gsub("NA?", "NA", x, fixed = TRUE)
  x <- gsub("<1e-04", "<0.0001", x, fixed = TRUE)
  x
}
#' @describeIn p_value detects and returns p-value column names in a data frame.
#' @param data a data frame
#' @param type the type of p-value to detect. Can be one of \code{c("all", "p", "p.adj")}.
#' @export
p_detect <- function(data, type = c("all", "p", "p.adj")){
  type <- match.arg(type)
  p.cols <- switch (type,
                    all = c(p_adj_names(), p_names()),
                    p = p_names(), p.adj = p_adj_names()
  )
  existing.p.cols <- intersect(p.cols, colnames(data))
  if(.is_empty(existing.p.cols)) existing.p.cols <- NULL
  existing.p.cols
}

#' @describeIn p_value returns known p-value column names
#' @export
p_names <- function(){
  c("p", "pval", "pvalue", "p.val", "p.value")
}

#' @describeIn p_value returns known adjust p-value column names
#' @export
p_adj_names <- function(){
  p_names() %>% paste0(".adj")
}


# Rounding specified columns
p_round_at <- function(data, ..., digits = 3){
  p.cols <- p_select(data, ...)
  if(!is.null(p.cols)){
    data %<>% dplyr::mutate_at(rlang::quos(p.cols), round_value, digits = digits)
  }
  data
}
# Formatting (specified) p value columns
p_format_at <- function(data, ..., new.col = FALSE, digits = 2, accuracy = 0.0001, decimal.mark = ".",
                         leading.zero = TRUE, trailing.zero = FALSE,
                         add.p = FALSE, space = TRUE){
  mutate_func <- dplyr::mutate_at
  if(new.col) mutate_func <- dplyr::transmute_at
  results <- data
  p.cols <- p_select(data, ...)
  if(!is.null(p.cols)){
    results <- results %>% mutate_func(
      p.cols, p_format, digits = digits, accuracy = accuracy,
      decimal.mark = decimal.mark, leading.zero = leading.zero,
      trailing.zero = trailing.zero, add.p = add.p, space = space
    )
    if(new.col){
      colnames(results) <- paste0(p.cols, ".format")
      results <- dplyr::bind_cols(data, results)
    }
  }
  results
}
# Mark significant at a specified column
p_mark_significant_at <- function(data, ..., new.col = FALSE, cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1),
                               symbols = c("****", "***", "**", "*", "")){
  mutate_func <- dplyr::mutate_at
  if(new.col) mutate_func <- dplyr::transmute_at
  results <- data
  p.cols <- p_select(data, ...)
  if(!is.null(p.cols)){
    results %<>% mutate_func(
      p.cols, p_mark_significant, cutpoints = cutpoints,
      symbols = symbols
    )
    if(new.col){
      colnames(results) <- paste0(p.cols, ".signif")
      results <- dplyr::bind_cols(data, results)
    }
  }
  results
}

# Manipulating leading zero-----------------------------
# Check if formatted p-values contain leading zero
p_contains_leading_zero <- function(p){
  any(grepl(pattern = "0.", p, fixed = TRUE))
}
remove_leading_zero <- function(x){
  sapply(x, function(x){ sub("^([-|<|=|>]?)0[.]", "\\1.", x)}) %>%
    as.character()
}
remove_trailing_zero <- function(x){
  gsub("0+$", "", x)
}

# Select p-value columns: p and p.adj -----------------------
p_select <- function(data, ...){
  p.col <- get_existing_dot_vars(data, ...)
  if(is.null(p.col) | .is_empty(p.col)){
    p.col <- p_detect(data, type = "all")
  }
  p.col
}
