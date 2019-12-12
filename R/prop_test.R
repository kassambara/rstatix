#' @include utilities.R
NULL
#'Proportion Test
#'@description Performs proportion tests to either evaluate the homogeneity of
#'  proportions (probabilities of success) in several groups or to test that the
#'  proportions are equal to certain given values.
#'
#'  Wrappers around the R base function \code{\link[stats]{prop.test}()} but have
#'  the advantage of performing pairwise and row-wise z-test of two proportions,
#'  the post-hoc tests following a significant chi-square test of homogeneity
#'  for 2xc and rx2 contingency tables.
#'@inheritParams stats::prop.test
#'@param xtab a cross-tabulation (or contingency table) with two columns and
#'  multiple rows (rx2 design). The columns give the counts of successes and
#'  failures respectively.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@param ... Other arguments passed to the function \code{prop_test()}.
#'
#'@return return a data frame with some the following columns: \itemize{
#' \item \code{n}: the number of participants.
#'\item \code{group}: the categories in the row-wise proportion tests. \item
#'  \code{statistic}: the value of Pearson's chi-squared test statistic. \item
#'  \code{df}: the degrees of freedom of the approximate chi-squared
#'  distribution of the test statistic. \item \code{p}: p-value. \item
#'  \code{p.adj}: the adjusted p-value. \item \code{method}: the used
#'  statistical test. \item \code{p.signif, p.adj.signif}: the significance
#'  level of p-values and adjusted p-values, respectively. \item
#'  \code{estimate}: a vector with the sample proportions x/n. \item
#'  \code{estimate1, estimate2}: the proportion in each of the two populations.
#'  \item \code{alternative}: a character string describing the alternative
#'  hypothesis. \item \code{conf.low,conf.high}: Lower and upper bound on a
#'  confidence interval. a confidence interval for the true proportion if there
#'  is one group, or for the difference in proportions if there are 2 groups and
#'  p is not given, or NULL otherwise. In the cases where it is not NULL, the
#'  returned confidence interval has an asymptotic confidence level as specified
#'  by conf.level, and is appropriate to the specified alternative hypothesis.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#' @examples
#' # Comparing an observed proportion to an expected proportion
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' prop_test(x = 95, n = 160, p = 0.5, detailed = TRUE)
#'
#' # Comparing two proportions
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: frequencies of smokers between two groups
#' xtab <- as.table(rbind(c(490, 10), c(400, 100)))
#' dimnames(xtab) <- list(
#'   group = c("grp1", "grp2"),
#'   smoker = c("yes", "no")
#' )
#' xtab
#' # compare the proportion of smokers
#' prop_test(xtab, detailed = TRUE)
#'
#' # Homogeneity of proportions between groups
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # H0: the proportion of smokers is similar in the four groups
#' # Ha:  this proportion is different in at least one of the populations.
#' #
#' # Data preparation
#' grp.size <- c( 106, 113, 156, 102 )
#' smokers  <- c( 50, 100, 139, 80 )
#' no.smokers <- grp.size - smokers
#' xtab <- as.table(rbind(
#'   smokers,
#'   no.smokers
#' ))
#' dimnames(xtab) <- list(
#'   Smokers = c("Yes", "No"),
#'   Groups = c("grp1", "grp2", "grp3", "grp4")
#' )
#' xtab
#'
#' # Compare the proportions of smokers between groups
#' prop_test(xtab, detailed = TRUE)
#'
#' # Pairwise comparison between groups
#' pairwise_prop_test(xtab)
#'
#'
#' # Pairwise proportion tests
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: Titanic
#' xtab <- as.table(rbind(
#'   c(122, 167, 528, 673),
#'   c(203, 118, 178, 212)
#' ))
#' dimnames(xtab) <- list(
#'   Survived = c("No", "Yes"),
#'   Class = c("1st", "2nd", "3rd", "Crew")
#' )
#' xtab
#' # Compare the proportion of survived between groups
#' pairwise_prop_test(xtab)
#'
#' # Row-wise proportion tests
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Data: Titanic
#' xtab <- as.table(rbind(
#'   c(180, 145), c(179, 106),
#'   c(510, 196), c(862, 23)
#' ))
#' dimnames(xtab) <- list(
#'   Class = c("1st", "2nd", "3rd", "Crew"),
#'   Gender = c("Male", "Female")
#' )
#' xtab
#' # Compare the proportion of males and females in each category
#' row_wise_prop_test(xtab)


#' @describeIn prop_test performs one-sample and two-samples z-test of
#'   proportions. Wrapper around the function \code{\link[stats]{prop.test}()}.
#' @export
prop_test <- function(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
                      correct = TRUE, conf.level = 0.95, detailed = FALSE){
  args <- as.list(environment()) %>%
    add_item(method = "prop_test")
  if(is.data.frame(x)) x <- as.matrix(x)

  if(inherits(x, c("matrix", "table"))){
    if(ncol(x) > 2 & nrow(x) == 2) x <- t(x)
    nb.grp <- nrow(x)
    row.sums <- rowSums(x)
    n <- sum(x)
    Ns <- matrix(c(n, row.sums), nrow = 1, ncol = nb.grp+1)
    colnames(Ns) <- c("n", paste0("n", 1:nb.grp))
  }
  else{
    row.sums <- x
    nb.grp <- length(x)
    Ns <- matrix(c(sum(n), row.sums), nrow = 1, ncol = nb.grp+1)
    colnames(Ns) <- c("n", paste0("n", 1:nb.grp))
  }
  Ns <- as_tibble(Ns)
  results <- stats::prop.test(x, n, p, alternative, conf.level, correct) %>%
    as_tidy_stat() %>%
    add_significance("p") %>%
    mutate(method = "Prop test")

  results <- dplyr::bind_cols(Ns, results)

  if(!detailed) results <- remove_details(results, method = "prop.test")
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "prop_test"))
}

#' @describeIn prop_test pairwise comparisons between proportions, a post-hoc
#'   tests following a significant chi-square test of homogeneity for 2xc
#'   design. Wrapper around \code{\link[stats]{pairwise.prop.test}()}
#' @export
pairwise_prop_test <- function(xtab, p.adjust.method = "holm", ...){
  if(is.data.frame(xtab)) xtab <- as.matrix(xtab)
  if(ncol(xtab) > 2 & nrow(xtab) == 2)
    xtab <- t(xtab)
  args <- c(as.list(environment()), list(...)) %>%
    add_item(method = "prop_test")
  results <- stats::pairwise.prop.test(
    xtab, p.adjust.method = "none",
    ...
    ) %>%
    tidy() %>%
    select(.data$group2, .data$group1, .data$p.value)
  colnames(results) <- c("group1", "group2", "p")
  results <- results %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(
      p = signif(.data$p, digits = 3),
      p.adj = signif(.data$p.adj, digits = 3)
      )
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "prop_test"))
}


#' @describeIn prop_test performs row-wise z-test of two proportions, a post-hoc tests following a significant chi-square test
#'   of homogeneity for rx2 contingency table. The z-test of two proportions is calculated for each category (row).
#' @export
row_wise_prop_test <- function(xtab, p.adjust.method = "holm", detailed = FALSE, ...){
  if(is.data.frame(xtab))
    xtab <- as.matrix(xtab)
  if(!inherits(xtab, c("matrix", "table"))){
    stop("An object of class 'matrix' or 'table' required")
  }
  if(ncol(xtab) !=2){
    stop("A cross-tabulation with two columns required")
  }
  args <- c(as.list(environment()), list(...)) %>%
    add_item(method = "prop_test")
  columns.total <- margin.table(xtab, 2)
  results <- apply(
    xtab, MARGIN = 1, FUN = prop_test,
    n = columns.total, detailed = detailed, ...
    ) %>%
    bind_rows(.id = "group") %>%
    adjust_pvalue(method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(
      p = signif(.data$p, digits = 3),
      p.adj = signif(.data$p.adj, digits = 3)
    ) %>%
    select(-.data$p.signif)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "prop_test"))
}


