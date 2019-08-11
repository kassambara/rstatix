#' @include utilities.R
NULL
#'Pairwise Comparisons of Estimated Marginal Means
#'
#'
#'@description Performs pairwise comparisons between groups using the estimated
#'  marginal means. Pipe-friendly wrapper arround the functions \code{emmans() +
#'  contrast()} from the \code{emmeans} package, which need to be installed
#'  before using this function. This function is useful for performing post-hoc
#'  analyses following ANOVA/ANCOVA tests.
#'@inheritParams t_test
#'@param model a fitted-model objects such as the result of a call to
#'  \code{lm()}, from which the overall degrees of
#'  freedom are to be calculated.
#'@param covariate (optional) covariate names (for ANCOVA)
#'@return return a data frame with some the following columns: \itemize{ \item
#'  \code{.y.}: the y variable used in the test. \item \code{group1,group2}: the
#'  compared groups in the pairwise tests. \item \code{statistic}: Test
#'  statistic (t.ratio) used to compute the p-value. \item \code{df}: degrees of
#'  freedom. \item \code{p}: p-value. \item \code{p.adj}: the adjusted p-value.
#'  \item \code{method}: the statistical test used to compare groups. \item
#'  \code{p.signif, p.adj.signif}: the significance level of p-values and
#'  adjusted p-values, respectively. \item \code{estimate}: estimate of the
#'  effect size, that is the difference between the two emmeans (estimated
#'  marginal means). \item \code{conf.low,conf.high}: Lower and upper bound on a
#'  confidence interval of the estimate. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments. It has also an attribute named "emmeans", a data
#'  frame containing the groups emmeans.
#'@examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#'# Pairwise comparisons
#' res <- df %>%
#'  group_by(supp) %>%
#'  emmeans_test(len ~ dose, p.adjust.method = "bonferroni")
#'res
#'
#' # Display estimated marginal means
#' attr(res, "emmeans")
#'
#' # Show details
#' df %>%
#'  group_by(supp) %>%
#'  emmeans_test(len ~ dose, p.adjust.method = "bonferroni", detailed = TRUE)
#'@export
emmeans_test <- function(data, formula, covariate = NULL, ref.group = NULL,
                         comparisons = NULL, p.adjust.method = "bonferroni",
                         conf.level = 0.95, model = NULL, detailed = FALSE){
  . <- NULL
  covariate <- rlang::enquos(covariate = covariate) %>%
    get_quo_vars_list(data, .) %>% unlist()
  args <- as.list(environment()) %>%
    .add_item(method = "emmeans_test")

  required_package("emmeans")
  outcome <- get_formula_left_hand_side(formula)
  rhs <- group <- get_formula_right_hand_side(formula)
  grouping.vars <- NULL

  if(is_grouped_df(data)){
    grouping.vars <- dplyr::group_vars(data)
    rhs <- c(grouping.vars, rhs) %>%
      paste(collapse = "*")
    data <- dplyr::ungroup(data)
  }
  if(!is.null(covariate)){
    covariate <- paste(covariate, collapse = "+")
    rhs <- paste(covariate, rhs, sep = "+")
  }

  data <- data %>% .as_factor(group, ref.group = ref.group)
  group.levels <- data %>% get_levels(group)

  # Build linear model
  formula <- stats::as.formula(paste(outcome, rhs, sep = " ~ "))
  if(is.null(model))
    model <- stats::lm(formula, data)

  # Fit emmeans
  # Possible pairwise comparisons: if ref.group specified,
  # only comparisons against reference will be kept
  if (is.null(comparisons)) {
    comparisons <- get_comparisons(data, variable = !!group, ref.group = !!ref.group)
  }
  method <- get_emmeans_contrasts(data, group, comparisons)
  formula.emmeans <- stats::as.formula(paste0("~", rhs))
  res.emmeans <- emmeans::emmeans(model, formula.emmeans)
  comparisons <- pairwise_emmeans_test(
    res.emmeans, grouping.vars, method = method,
    p.adjust.method = p.adjust.method,
    conf.level = conf.level
    )

  res.emmeans <- res.emmeans %>%
    tibble::as_tibble() %>%
    dplyr::arrange(!!!syms(grouping.vars)) %>%
    dplyr::rename(se = .data$SE, conf.low = .data$lower.CL, conf.high = .data$upper.CL) %>%
    mutate(method = "Emmeans test")

  if(!detailed){
    to.remove <- c("estimate", "estimate1", "estimate2", "se", "conf.low", "conf.high", "method")
    to.keep <- setdiff(colnames(comparisons), to.remove)
    comparisons <- comparisons[, to.keep]
  }
  comparisons %>%
    add_column(.y. = outcome, .before = "group1") %>%
    set_attrs(args = args, emmeans = res.emmeans) %>%
    add_class(c("rstatix_test", "emmeans_test"))
}

#' @export
#' @param emmeans.test an object of class \code{emmeans_test}.
#' @describeIn emmeans_test returns the estimated marginal means from an object of class \code{emmeans_test}
get_emmeans <- function(emmeans.test){
  if(!inherits(emmeans.test, "emmeans_test")){
    stop("An object of class 'emmeans_test' required.")
  }
  attr(emmeans.test, "emmeans")
}



pairwise_emmeans_test <- function(res.emmeans, grouping.vars = NULL, method = "pairwise",
                                  p.adjust.method = "bonferroni", conf.level = 0.95){
  # Comparisons without adjusting the pvalue
  # reverse the order of subtraction for consistency with pairwise_t_test
  comparisons <- emmeans::contrast(
    res.emmeans, by = grouping.vars, method = method,
    adjust = "none"
  )  %>%
    tidy(infer = TRUE, level = conf.level) %>%
    tidyr::separate(col = "contrast", into = c("group1", "group2"), sep = "-") %>%
    dplyr::rename(se = .data$std.error, p = .data$p.value) %>%
    dplyr::select(!!!syms(grouping.vars), everything())

    # Adjust the pvalue. We don't want to use adjust_pvalue here, because
    # emmeans support method = "tukey", but this is not the case for adjust_pvalue
    p.adjusted <- emmeans::contrast(
      res.emmeans, by = grouping.vars, method = method,
      adjust = p.adjust.method
    )  %>%
      tidy() %>%
      pull("p.value")
    comparisons  <- comparisons %>%
      mutate(p.adj = p.adjusted) %>%
      add_significance("p.adj")

  comparisons %>%
    dplyr::arrange(!!!syms(grouping.vars))
}

# Returns a list of contrasts for specific comparisons
# data: data frame,
# group: grouping columns,
# comparisons a list of comparisons
get_emmeans_contrasts <- function(data, group, comparisons){
  get_dummy_code <- function(level, group.levels){
    dummy.code <- rep(0, length(group.levels))
    lev.pos <- which(group.levels == level)
    dummy.code[lev.pos] <- 1
    dummy.code
  }
  make_emmeans_contrast <- function(groups, contrasts.list ){
    group1 <- groups[1]
    group2 <- groups[2]
    contrasts.list[[group1]]-contrasts.list[[group2]]
  }
  make_comparison_name <- function(groups){
    paste(groups[1], groups[2], sep = "-")
  }
  group.levels <- get_levels(data, group)
  contrasts.list <- group.levels %>%
    map(get_dummy_code, group.levels)
  names(contrasts.list) <- group.levels

  comparison.contrasts <- comparisons %>%
    map(make_emmeans_contrast, contrasts.list)
  comparison.names <- comparisons %>% map(make_comparison_name)
  names(comparison.contrasts) <- comparison.names
  comparison.contrasts
}


