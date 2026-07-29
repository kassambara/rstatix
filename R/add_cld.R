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
#'  p-value below \code{threshold} are treated as significant. A comparison whose
#'  p-value is missing (\code{NA}), or which the input does not contain at all,
#'  has no established significance: the groups it involves get no letter rather
#'  than being displayed as not significantly different.
#'@param reversed logical. If \code{TRUE}, reverses the order in which the
#'  letters are assigned (so that, with groups ordered by increasing level, the
#'  later groups receive the earlier letters). Default is \code{FALSE}.
#'@param ... not used.
#'@return a tibble with one row per group and the following columns: any grouping
#'  variables (for a grouped test), \code{.y.} (the outcome variable, when
#'  present), \code{group} (the group level) and \code{cld} (the compact letter
#'  display). Groups sharing a letter are not significantly different;
#'  \code{cld} is \code{NA} for a group involved in a comparison whose
#'  significance was not established, and a warning names those comparisons. A
#'  group keeps its letter only when every one of its comparisons is present and
#'  carries a p-value, so one group whose comparisons cannot be estimated can
#'  leave the whole display empty.
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
  pvals <- data[[p.col]]
  # A comparison is KNOWN only if some row carries a usable p-value for it. That
  # covers both ways it can be unknown: a pair the input never contains (a
  # ref.group result has only k - 1 of them), and a pair whose p-value is NA
  # because the test could not estimate it -- games_howell_test() returns NA
  # when the pair's Welch standard error is zero or undefined, which needs BOTH
  # groups constant, or one of them a single observation (a comparison against a
  # single constant group is perfectly estimable). Either way its significance
  # was not established, and lettering the two groups as equal would assert a
  # comparison that was never made (#323).
  #
  # Groups touched by an unknown comparison therefore get no letter. Because a
  # group is dropped as soon as ANY of its pairs is unknown, every pair among
  # the groups that remain is known, so the letters still shown rest only on
  # comparisons that were actually made. The rule is deliberately symmetric
  # rather than minimal: one group whose comparisons are all unestimable -- a
  # single-observation group, say -- removes every group it is compared against,
  # so the whole display can come back empty even though the other groups were
  # compared successfully with each other. Pair identity is carried as an
  # integer code rather than a pasted string, so a group name containing the
  # separator cannot collide with another pair.
  n.g <- length(groups)
  i1 <- match(group1, groups)
  i2 <- match(group2, groups)
  pair.code <- function(a, b) pmin(a, b) * (n.g + 1L) + pmax(a, b)
  known.codes <- unique(pair.code(i1, i2)[!is.na(pvals)])
  all.codes <- if(n.g > 1){
    idx <- utils::combn(seq_len(n.g), 2)
    pair.code(idx[1, ], idx[2, ])
  } else integer(0)
  unknown.codes <- setdiff(all.codes, known.codes)

  retained <- seq_len(n.g)
  if(length(unknown.codes) > 0){
    unknown.a <- unknown.codes %/% (n.g + 1L)
    unknown.b <- unknown.codes %% (n.g + 1L)
    retained <- setdiff(retained, unique(c(unknown.a, unknown.b)))
    pair.labels <- paste(groups[unknown.a], groups[unknown.b], sep = " - ")
    warning(
      "add_cld(): the significance of ", length(unknown.codes), " comparison",
      if(length(unknown.codes) > 1) "s" else "", " was not established (",
      paste(utils::head(pair.labels, 5), collapse = ", "),
      if(length(pair.labels) > 5) ", ..." else "",
      "): the input does not contain the comparison, or its p-value is NA. ",
      "The groups involved get no letter, because treating such a pair as ",
      "non-significant would display a comparison that was never made.",
      call. = FALSE
    )
  }

  is.sig <- !is.na(pvals) & pvals < threshold & i1 %in% retained & i2 %in% retained
  sig.pairs <- mapply(
    function(a, b) c(a, b), group1[is.sig], group2[is.sig],
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  kept <- groups[retained]
  letters.map <- .cld_letters(kept, sig.pairs, reversed = reversed)
  cld <- rep(NA_character_, n.g)
  cld[retained] <- unname(letters.map[kept])
  tibble(group = groups, cld = cld)
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
