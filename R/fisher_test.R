#' @include utilities.R
NULL
#'Fisher's Exact Test for Count Data
#'@description Performs Fisher's exact test for testing the null of independence
#'  of rows and columns in a contingency table.
#'
#'  Wrappers around the R base function \code{\link[stats]{fisher.test}()} but
#'  have the advantage of performing pairwise and row-wise fisher tests, the
#'  post-hoc tests following a significant chi-square test of homogeneity for 2xc
#'  and rx2 contingency tables.
#'@inheritParams stats::fisher.test
#'@param xtab a contingency table in a matrix form.
#'@param p.adjust.method method to adjust p values for multiple comparisons.
#'  Used when pairwise comparisons are performed. Allowed values include "holm",
#'  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't
#'  want to adjust the p value (not recommended), use p.adjust.method = "none".
#'@param detailed logical value. Default is FALSE. If TRUE, a detailed result is
#'  shown.
#'@param ... Other arguments passed to the function \code{fisher_test()}.
#'
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{group}: the categories in the row-wise proportion tests. \item
#'  \code{p}: p-value. \item \code{p.adj}: the adjusted p-value. \item
#'  \code{method}: the used statistical test. \item \code{p.signif,
#'  p.adj.signif}: the significance level of p-values and adjusted p-values,
#'  respectively. \item \code{estimate}: an estimate of the odds ratio. Only
#'  present in the 2 by 2 case. \item \code{alternative}: a character string
#'  describing the alternative hypothesis. \item \code{conf.low,conf.high}: a
#'  confidence interval for the odds ratio. Only present in the 2 by 2 case and
#'  if argument conf.int = TRUE.}
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#' @examples
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
#' fisher_test(xtab, detailed = TRUE)
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
#' fisher_test(xtab, detailed = TRUE)
#'
#' # Pairwise comparison between groups
#' pairwise_fisher_test(xtab)
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
#' pairwise_fisher_test(xtab)
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
#' row_wise_fisher_test(xtab)
#'
#' # A r x c table  Agresti (2002, p. 57) Job Satisfaction
#' Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
#'               dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
#'                              satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))
#' fisher_test(Job)
#' fisher_test(Job, simulate.p.value = TRUE, B = 1e5)


#' @describeIn fisher_test performs Fisher's exact test for testing the null of
#'   independence of rows and columns in a contingency table with fixed
#'   marginals. Wrapper around the function \code{\link[stats]{fisher.test}()}.
#' @export
fisher_test <- function(xtab, workspace = 200000, alternative = "two.sided",
                      conf.int = TRUE, conf.level = 0.95, simulate.p.value = FALSE,
                      B = 2000, detailed = FALSE, ...){
  if(is.data.frame(xtab)) xtab <- as.matrix(xtab)
  args <- as.list(environment()) %>% add_item(method = "fisher_test")
  results <- stats::fisher.test(
    xtab, workspace = workspace, alternative = alternative,
    conf.int = conf.int, conf.level = conf.level,
    simulate.p.value = simulate.p.value, B = B, ...
    ) %>%
    as_tidy_stat() %>%
    add_significance("p") %>%
    add_columns(n = sum(xtab), .before = 1) %>%
    mutate(method = "Fisher's Exact test")
  if(!detailed) results <- remove_details(results, method = "prop.test")
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "fisher_test"))
}

#' @describeIn fisher_test pairwise comparisons between proportions, a post-hoc
#'   tests following a significant Fisher's exact test of homogeneity for 2xc
#'   design.
#' @export
pairwise_fisher_test <- function(xtab, p.adjust.method = "holm", detailed = FALSE, ...){

  if(is.data.frame(xtab)) xtab <- as.matrix(xtab)
  if(ncol(xtab) > 2 & nrow(xtab) == 2)
    xtab <- t(xtab)
  if (is.null(colnames(xtab)) | any(0 %in% nchar(colnames(xtab)))) {
    colnames(xtab) <- paste0("col", 1:ncol(xtab))
  }
  if (is.null(rownames(xtab)) | any(0 %in% nchar(rownames(xtab)))) {
    rownames(xtab) <- paste0("row", 1:nrow(xtab))
  }
  if(ncol(xtab) > 2){
    stop("A two-dimensionnal contingency table required.")
  }
  compare_pair <- function(rows, xtab, ...){
    rows <- as.character(rows)
    fisher_test(xtab[rows, ], detailed = detailed, ...) %>%
      add_columns(group1 = rows[1], group2 = rows[2], .before = 1)
  }
  args <- c(as.list(environment()), list(...)) %>%
    add_item(method = "fisher_test")

  comparisons <- rownames(xtab) %>%
    .possible_pairs()
  results <- comparisons %>%
    map(compare_pair, xtab, ...) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    select(-.data$p.signif)
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "fisher_test"))
}


#' @describeIn fisher_test performs row-wise Fisher's exact test of count data, a post-hoc tests following a significant chi-square test
#'   of homogeneity for rx2 contingency table. The test is conducted for each category (row).
#' @export
row_wise_fisher_test <- function(xtab, p.adjust.method = "holm", detailed = FALSE, ...){
  if(is.data.frame(xtab))
    xtab <- as.matrix(xtab)
  if(!inherits(xtab, c("matrix", "table"))){
    stop("An object of class 'matrix' or 'table' required")
  }
  if(ncol(xtab) !=2){
    stop("A cross-tabulation with two columns required")
  }
  args <- c(as.list(environment()), list(...)) %>%
    add_item(method = "fisher_test")
  # Create xtab for each category (row)
  columns.total <- margin.table(xtab, 2)
  create_xtab <- function(x, n){
    as.data.frame(rbind(x, n-x))
  }
  xtab.list <- apply(xtab, 1, create_xtab, columns.total )
  results <- xtab.list %>%
    map(fisher_test, detailed = detailed, ...) %>%
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
    add_class(c("rstatix_test", "fisher_test"))
}


