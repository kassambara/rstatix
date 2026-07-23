#' @include utilities.R
NULL
#'Chi-squared Test for Count Data
#'@description Performs chi-squared tests, including goodness-of-fit,
#'  homogeneity and independence tests.
#'
#'  \code{chisq_test()} also accepts a pipe-friendly data-frame interface for the
#'  test of independence between two categorical variables: pass a data frame as
#'  \code{x} and the two columns either positionally
#'  (\code{data \%>\% chisq_test(var1, var2)}) or via \code{vars}
#'  (\code{data \%>\% chisq_test(vars = c("var1", "var2"))}). The contingency
#'  table is built internally. Note that in the positional form the second column
#'  occupies the \code{correct} argument slot, so use the \code{vars} form (or the
#'  table interface) if you need to set \code{correct}/\code{simulate.p.value}.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/categorical/chi-square-test-of-independence-in-r}{Chi-Square Test of Independence in R}
#'  for a worked walkthrough.
#'@inheritParams  stats::chisq.test
#'@param vars optional character vector of length two giving the names of two
#'  columns in the data frame \code{x} to cross-tabulate for a test of
#'  independence. An alternative to passing the two columns positionally.
#'@param res.chisq an object of class \code{chisq_test}.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param ... other arguments passed to the function \code{{chisq_test}()}.
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{n}: the number of participants. \item \code{group, group1, group2}:
#'  the categories or groups being compared. \item \code{statistic}: the value
#'  of Pearson's chi-squared test statistic. \item \code{df}: the degrees of
#'  freedom of the approximate chi-squared distribution of the test statistic.
#'  NA if the p-value is computed by Monte Carlo simulation. \item \code{p}:
#'  p-value. \item \code{p.adj}: the adjusted p-value. \item \code{method}: the
#'  used statistical test. \item \code{p.signif, p.adj.signif}: the significance
#'  level of p-values and adjusted p-values, respectively. \item
#'  \code{observed}: observed counts. \item
#'  \code{expected}: the expected counts under the null hypothesis.
#'  }
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'
#' @examples
#' # Chi-square goodness of fit test
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' tulip <- c(red = 81, yellow = 50, white = 27)
#' # Q1: Are the colors equally common?
#' chisq_test(tulip)
#' pairwise_chisq_gof_test(tulip)
#' # Q2: comparing observed to expected proportions
#' chisq_test(tulip, p = c(1/2, 1/3, 1/6))
#' pairwise_chisq_test_against_p(tulip, p = c(0.5, 0.33, 0.17))
#'
#' # Homogeneity of proportions between groups
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: Titanic
#' xtab <- as.table(rbind(
#'   c(203, 118, 178, 212),
#'   c(122, 167, 528, 673)
#' ))
#' dimnames(xtab) <- list(
#'   Survived = c("Yes", "No"),
#'   Class = c("1st", "2nd", "3rd", "Crew")
#' )
#' xtab
#' # Chi-square test
#' chisq_test(xtab)
#' # Compare the proportion of survived between groups
#' pairwise_prop_test(xtab)
#'
#' # Test of independence using the data-frame interface
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' df <- data.frame(
#'   gender = rep(c("M", "F"), each = 100),
#'   smoker = rep(c("yes", "no", "yes", "no"), times = c(30, 70, 60, 40))
#' )
#' # Positional columns
#' df %>% chisq_test(gender, smoker)
#' # Equivalent, using vars (keeps `correct` settable)
#' df %>% chisq_test(vars = c("gender", "smoker"), correct = FALSE)
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/categorical/chi-square-test-of-independence-in-r}{Chi-Square Test of Independence in R}, \href{https://www.datanovia.com/learn/biostatistics/categorical/chi-square-goodness-of-fit-test-in-r}{Chi-Square Goodness-of-Fit Test in R}.


#' @describeIn chisq_test performs chi-square tests including goodness-of-fit,
#'   homogeneity and independence tests.
#' @export
chisq_test <- function(x, y = NULL, correct = TRUE,
                       p = rep(1/length(x), length(x)), rescale.p = FALSE,
                       simulate.p.value = FALSE, B = 2000, vars = NULL){
  # Data-frame / two-column interface (#43): build the contingency table from two
  # columns and run the test of independence. This is dispatched BEFORE the
  # `args <- as.list(environment())` line below, because forcing `correct` (which
  # holds the bare column name in the positional form) is exactly what errored
  # before. The branch fires only for data frames with column references - inputs
  # that previously threw an error - so the table/vector interface is untouched.
  if(is.data.frame(x)){
    y_quo <- rlang::enquo(y)
    correct_quo <- rlang::enquo(correct)
    p_quo <- rlang::enquo(p)
    selected.vars <- NULL
    if(!is.null(vars)){
      selected.vars <- as.character(vars)
    } else if(!rlang::quo_is_null(y_quo)){
      # positional form: chisq_test(data, var1, var2) -> var1 in `y`. The second
      # column lands in the `correct` slot; but if `correct =` is passed by name,
      # it lands in the next free positional slot `p` instead. A slot is treated
      # as a column only when it holds a name/string naming a column of `x`.
      var1 <- get_quo_vars(x, y_quo)
      var2 <- NULL
      for(slot in c("correct", "p")){
        cand <- switch(slot, correct = correct_quo, p = p_quo)
        cexpr <- rlang::quo_get_expr(cand)
        if(is.symbol(cexpr) || is.character(cexpr)){
          v <- tryCatch(get_quo_vars(x, cand), error = function(e) NULL)
          if(length(v) && all(v %in% names(x))){
            var2 <- v
            # if the column occupied the `correct` slot, restore the default
            if(slot == "correct") correct <- TRUE
            break
          }
        }
      }
      selected.vars <- c(var1, var2)
    }
    if(!is.null(selected.vars)){
      if(length(selected.vars) != 2 || !all(selected.vars %in% names(x))){
        stop(
          "chisq_test(): the data-frame interface needs exactly two columns of ",
          "`x`, e.g. `data %>% chisq_test(var1, var2)` or ",
          "`data %>% chisq_test(vars = c(\"var1\", \"var2\"))`. If you are also ",
          "passing other arguments by name (e.g. `correct`, `p`), use the ",
          "`vars =` form.",
          call. = FALSE
        )
      }
      args <- list(
        x = x, y = NULL, correct = correct, rescale.p = rescale.p,
        simulate.p.value = simulate.p.value, B = B, vars = selected.vars,
        method = "chisq_test"
      )
      # Grouped data: run the test independently per group (consistent with the
      # other rstatix tests) instead of pooling all rows.
      if(is_grouped_df(x)){
        results <- x %>%
          doo(
            chisq_test, vars = selected.vars, correct = correct,
            simulate.p.value = simulate.p.value, B = B
          )
        return(
          results %>%
            set_attrs(args = args) %>%
            add_class(c("rstatix_test", "chisq_test"))
        )
      }
      xtab <- table(x[[selected.vars[1]]], x[[selected.vars[2]]])
      n <- sum(xtab)
      res.chisq <- stats::chisq.test(
        xtab, correct = correct,
        simulate.p.value = simulate.p.value, B = B
      )
      return(
        as_tidy_stat(res.chisq, stat.method = "Chi-square test") %>%
          add_significance("p") %>%
          add_columns(n = n, .before = 1) %>%
          set_attrs(args = args, test = res.chisq) %>%
          add_class(c("rstatix_test", "chisq_test"))
      )
    }
  }
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(inherits(x, c("matrix", "table"))) n <- sum(x)
  else n <- length(x)
  res.chisq <- stats::chisq.test(
    x, y,  correct = correct, p = p, rescale.p = rescale.p,
    simulate.p.value = simulate.p.value, B = B
    )
  as_tidy_stat(res.chisq, stat.method = "Chi-square test") %>%
    add_significance("p") %>%
    add_columns(n = n, .before = 1) %>%
    set_attrs(args = args, test = res.chisq) %>%
    add_class(c("rstatix_test", "chisq_test"))
}

#' @describeIn chisq_test perform pairwise comparisons between groups following a global
#'   chi-square goodness of fit test.
#' @export
pairwise_chisq_gof_test <- function(x, p.adjust.method = "holm", ...){
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  compare_pair <- function(levs, x, ...){
    levs <- as.character(levs)
    suppressWarnings(chisq_test(x[levs],  ...)) %>%
      add_columns(group1 = levs[1], group2 = levs[2], .before = "statistic")
  }
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  comparisons <- names(x) %>%
    .possible_pairs()
  results <- comparisons %>%
    map(compare_pair, x, ...) %>%
    map(keep_only_tbl_df_classes) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    select(-any_of(c("p.signif", "method")))
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "chisq_test"))
}

#' @describeIn chisq_test perform pairwise comparisons after a global
#'   chi-squared test for given probabilities. For each group, the observed and
#'   the expected proportions are shown. Each group is compared to the sum of
#'   all others.
#' @export
pairwise_chisq_test_against_p <- function(x, p = rep(1/length(x), length(x)), p.adjust.method = "holm", ...){
  args <- as.list(environment()) %>%
    add_item(method = "chisq_test")
  if (sum(p) != 1) {
    stop(
      "Make sure that the `p` argument is correctly specified.",
      "sum of probabilities must be 1."
      )
  }
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  results <- list()
  for (i in 1:length(x)) {
    res.chisq <- suppressWarnings(chisq_test(c(x[i], sum(x) - x[i]), p = c(p[i], 1 - p[i]), ...))
    res.desc <- chisq_descriptives(res.chisq)
    res.chisq <- res.chisq %>%
      add_columns(observed = res.desc$observed[1], expected = res.desc$expected[1], .before = 1)
    results[[i]] <- res.chisq
  }
  results <- results %>%
    map(keep_only_tbl_df_classes) %>%
    bind_rows() %>%
    add_columns(group = names(x), .before = 1) %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    select(-any_of(c("p.signif", "method")))
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "chisq_test"))
}


# The descriptive accessors below need the stored chisq.test object
# (attr "test"). A grouped chisq_test() result holds one test per group, so no
# single test object is stored; fail with a clear message instead of silently
# returning empty output.
assert_chisq_has_test <- function(res.chisq){
  if(is.null(attr(res.chisq, "test"))){
    stop(
      "No chi-square test object is stored on this result, so descriptive ",
      "statistics are unavailable. These are only available for a single ",
      "(ungrouped) `chisq_test()` result.",
      call. = FALSE
    )
  }
  invisible(res.chisq)
}

#' @describeIn chisq_test returns the descriptive statistics of the chi-square
#'   test. These include, observed and expected frequencies, proportions,
#'   residuals and standardized residuals. Only available for a single
#'   (ungrouped) \code{chisq_test()} result.
#' @export
chisq_descriptives <- function(res.chisq){
  assert_chisq_has_test(res.chisq)
  res <- attr(res.chisq, "test") %>% augment()
  colnames(res) <- gsub(pattern = "^\\.", replacement = "", colnames(res))
  res
}

#' @describeIn chisq_test returns the expected counts from the chi-square test result.
#' @export
expected_freq <- function(res.chisq){
  assert_chisq_has_test(res.chisq)
  attr(res.chisq, "test")$expected
}

#' @describeIn chisq_test returns the observed counts from the chi-square test result.
#' @export
observed_freq <- function(res.chisq){
  assert_chisq_has_test(res.chisq)
  attr(res.chisq, "test")$observed
}

#' @describeIn chisq_test returns the Pearson residuals, \code{(observed - expected) / sqrt(expected)}.
#' @export
pearson_residuals <- function(res.chisq){
  assert_chisq_has_test(res.chisq)
  attr(res.chisq, "test")$residuals
}

#' @describeIn chisq_test returns the standardized residuals
#' @export
std_residuals <- function(res.chisq){
  assert_chisq_has_test(res.chisq)
  attr(res.chisq, "test")$stdres
}



