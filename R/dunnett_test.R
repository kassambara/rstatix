#' @include utilities.R emmeans_test.R
NULL
#'Dunnett's Many-to-One Comparisons Test
#'
#'@description Performs Dunnett's test for comparing each of several treatment
#'  groups against a single control (reference) group. Unlike all-pairwise
#'  post-hoc tests, Dunnett's procedure controls the family-wise error rate over
#'  only the \code{k - 1} treatment-vs-control comparisons, using the exact
#'  multivariate-t distribution (which accounts for the correlation between the
#'  comparisons that share the control group).
#'
#'  This is a pipe-friendly wrapper around \code{emmeans::emmeans()} +
#'  \code{emmeans::contrast()} (with \code{adjust = "mvt"}), so the
#'  \code{emmeans} package must be installed. The results match
#'  \code{DescTools::DunnettTest()} and \code{multcomp::glht(..., mcp(... =
#'  "Dunnett"))}.
#'
#'@inheritParams t_test
#'@param ref.group a character string specifying the reference (control) group.
#'  Each remaining group level is compared against this group. If \code{NULL}
#'  (default), the first level of the grouping variable is used as the control.
#'@param conf.level confidence level of the (simultaneous) confidence intervals.
#'@return a data frame with some of the following columns: \itemize{ \item
#'  \code{.y.}: the outcome variable used in the test. \item
#'  \code{group1,group2}: the compared groups; \code{group1} is the control
#'  (reference) and \code{group2} is the treatment, consistent with the
#'  \code{ref.group} convention of \code{t_test()}/\code{wilcox_test()}/
#'  \code{dunn_test()}/\code{emmeans_test()}. \item \code{n1,n2}: sample sizes of
#'  the control and treatment groups. \item \code{estimate}: the estimated mean
#'  difference \code{group1 - group2} (control minus treatment). \item
#'  \code{conf.low,conf.high}: simultaneous confidence interval for the
#'  difference. \item \code{statistic}: the t-statistic. \item \code{df}:
#'  degrees of freedom. \item \code{p.adj}: the Dunnett-adjusted p-value. \item
#'  \code{method}: the statistical test used. \item \code{p.adj.signif}: the
#'  significance level of the adjusted p-value. } The estimate, confidence
#'  interval, se and method columns are returned only when \code{detailed =
#'  TRUE}.
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@references Dunnett, C. W. (1955) A multiple comparison procedure for comparing
#'  several treatments with a control. Journal of the American Statistical
#'  Association, 50, 1096-1121.
#'@seealso \code{\link{tukey_hsd}()}, \code{\link{games_howell_test}()},
#'  \code{\link{emmeans_test}()}
#' @examples
#' # Compare each dose to the control dose ("0.5")
#' ToothGrowth %>% dunnett_test(len ~ dose)
#'
#' # Detailed output (estimate + simultaneous confidence interval)
#' ToothGrowth %>% dunnett_test(len ~ dose, detailed = TRUE)
#'
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   dunnett_test(len ~ dose)
#'@export
dunnett_test <- function(data, formula, ref.group = NULL, conf.level = 0.95,
                         detailed = FALSE){
  if(!is.null(ref.group)) ref.group <- as.character(ref.group)
  args <- as.list(environment()) %>%
    .add_item(method = "dunnett_test")
  if(is_grouped_df(data)){
    results <- data %>%
      doo(.dunnett_test, formula, ref.group = ref.group, conf.level = conf.level)
  }
  else{
    results <- .dunnett_test(data, formula, ref.group = ref.group, conf.level = conf.level)
  }
  if(!detailed){
    results <- results %>%
      select(-any_of(c("estimate", "conf.low", "conf.high", "se", "method", "null.value")))
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "dunnett_test"))
}

.dunnett_test <- function(data, formula, ref.group = NULL, conf.level = 0.95){
  required_package("emmeans")
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups == 1){
    stop("all observations are in the same group")
  }

  data <- data %>%
    select(!!!syms(c(outcome, group))) %>%
    get_complete_cases()
  # Validate ref.group BEFORE releveling (.as_factor would otherwise raise a
  # cryptic relevel error for an unknown reference).
  available.levels <- data %>% .as_factor(group) %>% get_levels(group)
  if(!is.null(ref.group) && !(ref.group %in% available.levels)){
    stop(
      "Specified reference group ('", ref.group, "') is not a level of the ",
      "grouping variable. Valid levels are: ",
      paste(available.levels, collapse = ", "), ".",
      call. = FALSE
    )
  }
  data <- data %>% .as_factor(group, ref.group = ref.group)
  group.levels <- get_levels(data, group)
  if(is.null(ref.group)) ref.group <- group.levels[1]
  group.size <- data %>% get_group_size(group)

  # Each treatment vs the control, oriented as group1 = control (reference),
  # group2 = treatment, so estimate = control - treatment (= group1 - group2),
  # consistent with t_test()/wilcox_test()/dunn_test()/emmeans_test() ref.group.
  treatments <- setdiff(group.levels, ref.group)
  comparisons <- treatments %>% map(~c(ref.group, .x))
  contrasts <- get_emmeans_contrasts(data, group, comparisons)

  # Fit the model and apply the exact Dunnett (multivariate-t) adjustment.
  lm.formula <- stats::as.formula(paste(outcome, group, sep = " ~ "))
  model <- stats::lm(lm.formula, data)
  emm <- emmeans::emmeans(model, stats::as.formula(paste0("~", group)))
  res <- emmeans::contrast(emm, method = contrasts, adjust = "mvt") %>%
    broom::tidy(conf.int = TRUE, conf.level = conf.level) %>%
    keep_only_tbl_df_classes()

  # group1/group2 are taken from the (ordered) comparison list rather than parsed
  # from the contrast name, so factor levels containing "-" are handled correctly.
  # The contrast rows are returned in the same order as `contrasts`.
  group1 <- vapply(comparisons, function(x) x[[1]], character(1))
  group2 <- vapply(comparisons, function(x) x[[2]], character(1))
  # With a single contrast (k = 2) there is no multiplicity column: emmeans
  # returns "p.value" (which is the Dunnett p for one comparison) instead of
  # "adj.p.value". Use whichever is present.
  p.col <- intersect(c("adj.p.value", "p.value"), colnames(res))[1]
  colnames(res)[colnames(res) == "std.error"] <- "se"
  colnames(res)[colnames(res) == p.col] <- "p.adj"

  res %>%
    mutate(
      .y. = outcome, method = "Dunnett",
      group1 = group1, group2 = group2,
      n1 = group.size[group1], n2 = group.size[group2]
    ) %>%
    add_significance("p.adj") %>%
    select(all_of(c(
      ".y.", "group1", "group2", "n1", "n2", "estimate",
      "conf.low", "conf.high", "se", "statistic", "df", "p.adj",
      "p.adj.signif", "method"
    )))
}
