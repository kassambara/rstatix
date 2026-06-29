#' @include utilities.R
NULL
#'Compact Letter Display of All-Pairwise Comparisons
#'
#'@description Adds the \strong{compact letter display} (CLD) to a pairwise
#'  comparison result. Groups that do \emph{not} share a letter are
#'  significantly different. This is a convenient way to annotate plots
#'  (e.g. one letter per box/bar) after an all-pairwise post-hoc test such as
#'  \code{\link{tukey_hsd}()}, \code{\link{dunn_test}()},
#'  \code{\link{games_howell_test}()}, \code{\link{conover_test}()},
#'  \code{\link{wilcox_test}()} or \code{\link{t_test}()}.
#'
#'  The letters are computed with the insert-and-absorb algorithm (Piepho, 2004)
#'  using base R only, so no additional package is required (the results match
#'  \code{multcompView::multcompLetters()}).
#'
#'@param test an all-pairwise comparison result returned by an \code{rstatix}
#'  function (e.g. \code{tukey_hsd()}, \code{dunn_test()}, a pairwise
#'  \code{t_test()}/\code{wilcox_test()}, ...). Must contain the \code{group1}
#'  and \code{group2} columns and a p-value column.
#'@param p.col character. The p-value column to threshold. If \code{NULL}
#'  (default), \code{"p.adj"} is used when present, otherwise \code{"p"}.
#'@param threshold the significance threshold (default 0.05). Comparisons with a
#'  p-value below \code{threshold} are treated as significant; comparisons with a
#'  missing (\code{NA}) p-value are treated as non-significant.
#'@param reversed logical. If \code{TRUE}, reverses the order in which the
#'  letters are assigned (so that, with groups ordered by increasing level, the
#'  later groups receive the earlier letters). Default is \code{FALSE}.
#'@param ... not used.
#'@return a tibble with one row per group and the following columns: any grouping
#'  variables (for a grouped test), \code{.y.} (the outcome variable, when
#'  present), \code{group} (the group level) and \code{cld} (the compact letter
#'  display). Groups sharing a letter are not significantly different.
#'@references Piepho, H.-P. (2004) An Algorithm for a Letter-Based Representation
#'  of All-Pairwise Comparisons. Journal of Computational and Graphical
#'  Statistics, 13(2), 456-466.
#'@seealso \code{\link{tukey_hsd}}, \code{\link{dunn_test}},
#'  \code{\link{games_howell_test}}, \code{\link{add_significance}}
#' @examples
#' # Tukey HSD post-hoc, then compact letter display
#' res <- ToothGrowth %>%
#'   mutate(dose = factor(dose)) %>%
#'   tukey_hsd(len ~ dose)
#' res %>% add_cld()
#'
#' # Works on rank-based post-hocs too
#' ToothGrowth %>% dunn_test(len ~ dose) %>% add_cld()
#'
#' # Grouped pairwise test -> one CLD per group
#' ToothGrowth %>%
#'   mutate(dose = factor(dose)) %>%
#'   group_by(supp) %>%
#'   tukey_hsd(len ~ dose) %>%
#'   add_cld()
#'@name add_cld
#'@export
add_cld <- function(test, p.col = NULL, threshold = 0.05, reversed = FALSE, ...){
  if(!all(c("group1", "group2") %in% colnames(test))){
    stop("add_cld() requires an all-pairwise comparison result with 'group1' ",
         "and 'group2' columns.", call. = FALSE)
  }
  if(is.null(p.col)){
    p.col <- intersect(c("p.adj", "p"), colnames(test))[1]
  }
  if(is.na(p.col) || !(p.col %in% colnames(test))){
    stop("add_cld(): no p-value column found. Specify one with `p.col`.",
         call. = FALSE)
  }
  data <- keep_only_tbl_df_classes(test)
  # All columns before 'group1' identify the comparison context: any group_by()
  # grouping variables plus the outcome/term identifier ('.y.' for most tests,
  # 'term' for tukey_hsd()). Splitting by all of them computes one compact letter
  # display per group_by() group and keeps those columns in the output. (Within a
  # single test the outcome/term is constant, so including it is harmless.)
  g1.index <- match("group1", colnames(data))
  split.vars <- if(g1.index > 1) colnames(data)[seq_len(g1.index - 1)] else character(0)
  if(length(split.vars) == 0){
    results <- .add_cld_core(data, p.col, threshold, reversed)
  }
  else{
    results <- data %>%
      group_by(!!!syms(split.vars)) %>%
      doo(.add_cld_core, p.col = p.col, threshold = threshold, reversed = reversed)
  }
  results %>% add_class("rstatix_test")
}

# Compute the compact letter display for a single ungrouped pairwise table.
.add_cld_core <- function(data, p.col, threshold = 0.05, reversed = FALSE){
  raw1 <- data$group1; raw2 <- data$group2
  group1 <- as.character(raw1)
  group2 <- as.character(raw2)
  # Display/letter order is deterministic: follow the factor levels when the
  # group columns are factors, otherwise the order of first appearance (group1
  # then group2). rstatix test outputs are already level-ordered.
  if(is.factor(raw1) || is.factor(raw2)){
    levs <- union(levels(as.factor(raw1)), levels(as.factor(raw2)))
    groups <- levs[levs %in% c(group1, group2)]
  }
  else {
    groups <- unique(c(group1, group2))
  }
  # The compact letter display is only meaningful for an ALL-pairwise input.
  # A reduced set (e.g. a ref.group result with only k - 1 comparisons) would
  # treat the missing pairs as non-significant and produce a misleading display.
  # Count DISTINCT unordered pairs (not rows) so a duplicated comparison can't
  # mask a genuinely missing one.
  n.expected <- choose(length(groups), 2)
  n.present <- length(unique(paste(pmin(group1, group2), pmax(group1, group2))))
  if(n.present < n.expected){
    warning(
      "add_cld(): the input has ", n.present, " of the ", n.expected,
      " pairwise comparisons needed for a complete display of ", length(groups),
      " groups. Missing comparisons are treated as non-significant, which can ",
      "make the compact letter display misleading (e.g. for a `ref.group` result).",
      call. = FALSE
    )
  }
  pvals <- data[[p.col]]
  is.sig <- !is.na(pvals) & pvals < threshold
  sig.pairs <- mapply(
    function(a, b) c(a, b), group1[is.sig], group2[is.sig],
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  letters.map <- .cld_letters(groups, sig.pairs, reversed = reversed)
  tibble(group = groups, cld = unname(letters.map[groups]))
}

# Insert-and-absorb letter assignment (Piepho, 2004). `groups` is a character
# vector (defines the display order); `sig.pairs` a list of c(a, b) significant
# pairs. Returns a named character vector of letters, one per group. Matches
# multcompView::multcompLetters().
.cld_letters <- function(groups, sig.pairs, reversed = FALSE){
  if(length(groups) == 0) return(stats::setNames(character(0), character(0)))
  # Each "column" is a set of groups that are all mutually non-significant.
  columns <- list(groups)
  for(pair in sig.pairs){
    a <- pair[1]; b <- pair[2]
    new.columns <- list()
    for(col in columns){
      if(a %in% col && b %in% col){
        # split: remove a from one copy, b from the other
        new.columns[[length(new.columns) + 1]] <- setdiff(col, a)
        new.columns[[length(new.columns) + 1]] <- setdiff(col, b)
      }
      else {
        new.columns[[length(new.columns) + 1]] <- col
      }
    }
    # drop empty columns and exact duplicates
    new.columns <- Filter(function(x) length(x) > 0, new.columns)
    new.columns <- new.columns[!duplicated(
      lapply(new.columns, function(x) paste(sort(x), collapse = "\\u0001"))
    )]
    # absorb: drop any column whose groups are a strict subset of another column
    keep <- rep(TRUE, length(new.columns))
    for(i in seq_along(new.columns)){
      for(j in seq_along(new.columns)){
        if(i != j && keep[i] && keep[j] &&
           all(new.columns[[i]] %in% new.columns[[j]]) &&
           length(new.columns[[i]]) < length(new.columns[[j]])){
          keep[i] <- FALSE
        }
      }
    }
    columns <- new.columns[keep]
  }
  # order columns by the first-appearing group so letters are stable and read
  # left-to-right with the group order
  first.pos <- sapply(columns, function(col) min(match(col, groups)))
  columns <- columns[order(first.pos)]
  if(reversed) columns <- rev(columns)
  # Letter labels MUST stay single characters so the concatenated cld string
  # remains tokenizable (e.g. "ab" = letters a and b). Extend past z into A..Z
  # for the rare case of > 26 columns (matching multcompView's behavior).
  n.col <- length(columns)
  alphabet <- c(letters, LETTERS)
  if(n.col > length(alphabet)){
    stop("add_cld() supports at most ", length(alphabet), " letter groups; ",
         "this comparison needs ", n.col, ".", call. = FALSE)
  }
  labels <- alphabet[seq_len(n.col)]
  letters.map <- stats::setNames(rep("", length(groups)), groups)
  for(ci in seq_along(columns)){
    this.letter <- labels[ci]
    for(g in columns[[ci]]){
      letters.map[g] <- paste0(letters.map[g], this.letter)
    }
  }
  letters.map
}
