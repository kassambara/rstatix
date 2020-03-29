#' @include utilities.R
NULL
#'McNemar's Chi-squared Test for Count Data
#'@description Performs McNemar chi-squared test to compare paired proportions.
#'
#'  Wrappers around the R base function \code{\link[stats]{mcnemar.test}()}, but
#'  provide pairwise comparisons between multiple groups
#'@inheritParams stats::mcnemar.test
#'@param data a data frame containing the variables in the formula.
#'@param formula a formula of the form \code{a ~ b | c}, where \code{a} is the
#'  outcome variable name; b is the within-subjects factor variables; and c
#'  (factor) is the column name containing individuals/subjects identifier.
#'  Should be unique per individual.
#'@param type type of statistical tests used for pairwise comparisons. Allowed
#'  values are one of \code{c("mcnemar", "exact")}.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'
#'
#'@return return a data frame with the following columns: \itemize{
#' \item \code{n}: the number of participants.
#' \item \code{statistic}: the value of McNemar's statistic. \item \code{df} the
#'  degrees of freedom of the approximate chi-squared distribution of the test
#'  statistic. \item \code{p}: p-value. \item \code{p.adj}: the adjusted
#'  p-value. \item \code{method}: the used statistical test. \item
#'  \code{p.signif}: the significance level of p-values.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#' @examples
#'
#' # Comparing two paired proportions
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: frequencies of smokers before and after interventions
#' xtab <- as.table(
#'   rbind(c(25, 6), c(21,10))
#' )
#' dimnames(xtab) <- list(
#'   before = c("non.smoker", "smoker"),
#'   after = c("non.smoker", "smoker")
#' )
#' xtab
#'
#' # Compare the proportion of smokers
#' mcnemar_test(xtab)
#'
#' # Comparing multiple related proportions
#' # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Generate a demo data
#' mydata <- data.frame(
#'   outcome = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
#'   treatment = gl(3,1,30,labels=LETTERS[1:3]),
#'   participant = gl(10,3,labels=letters[1:10])
#' )
#' mydata$outcome <- factor(
#'   mydata$outcome, levels = c(1, 0),
#'   labels = c("success", "failure")
#'   )
#' # Cross-tabulation
#' xtabs(~outcome + treatment, mydata)
#'
#' # Compare the proportion of success between treatments
#' cochran_qtest(mydata, outcome ~ treatment|participant)
#'
#' # pairwise comparisons between groups
#' pairwise_mcnemar_test(mydata, outcome ~ treatment|participant)
#'
#'@describeIn mcnemar_test performs McNemar's chi-squared test for comparing two
#'  paired proportions
#'@export
mcnemar_test <- function(x, y = NULL, correct = TRUE){
  args <- as.list(environment()) %>%
    add_item(method = "mcnemar_test")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(inherits(x, c("matrix", "table"))) n <- sum(x)
  else n <- length(x)
  results <- stats::mcnemar.test(x, y, correct) %>%
    as_tidy_stat() %>%
    add_significance("p") %>%
    mutate(method = "McNemar test", n = n)
  results[, c("n", "statistic", "df", "p", "p.signif", "method")] %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "mcnemar_test"))
}

#'@describeIn mcnemar_test performs pairwise McNemar's chi-squared test between
#'  multiple groups. Could be used for post-hoc tests following a significant Cochran's Q test.
#'@export
pairwise_mcnemar_test <- function (data, formula, type = c("mcnemar", "exact"), correct = TRUE,
                              p.adjust.method = "bonferroni")
{
  type  <- match.arg(type)
  test.class <- switch (type,
    mcnemar = "mcnemar_test",
    exact = "exact_binom_test"
  )
  data <- data %>% select(!!!syms(all.vars(formula)))
  colnames(data) <- c("outcome", "groups", "participant")
  if(length(unique(data$outcome))> 2){
    stop("Unique possible outcome values should be 2")
  }
  args <- as.list(environment()) %>%
    add_item(method = test.class)

  # helper function to compar pairs
  compare_pair <- function(grps, data, type = "mcnemar"){
    grps <- as.character(grps)
    grps.data <- data[, grps]
    colnames(grps.data) <- c("grp1", "grp2")
    xtab <- stats::xtabs(~grp1+grp2, grps.data)
    if(type == "mcnemar"){
      results <- mcnemar_test(xtab, correct = correct)
    }
    else if(type == "exact"){
      # Get off-diagonal values
      b <- xtab[2, 1]
      c <- xtab[1, 2]
      results <- binom_test(b, (b + c), p = 0.5, detailed = TRUE)
    }
    results %>%
      keep_only_tbl_df_classes() %>%
      select(.data$p, .data$method) %>%
      add_columns(group1 = grps[1], group2 = grps[2], .before = "p")
  }
  # Convert outcome into factor, then spread.
  data <- data %>%
    mutate(outcome = as.factor(.data$outcome)) %>%
    spread(key = "groups", value = "outcome")
  # Pairwise comparisons
  comparisons <- colnames(data)[-1] %>%
    .possible_pairs()
  results <- comparisons %>%
    map(compare_pair, data, type = type) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3))
  results [, c("group1", "group2", "p", "p.adj", "p.adj.signif", "method")] %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", test.class))
}
