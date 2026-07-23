#' @include utilities.R t_test.R
NULL
#'Games Howell Post-hoc Tests
#'
#'@description Performs Games-Howell test, which is used to compare all possible
#'  combinations of group differences when the assumption of homogeneity of
#'  variances is violated. This post hoc test provides confidence intervals for
#'  the differences between group means and shows whether the differences are
#'  statistically significant.
#'
#'  The test is based on Welch’s degrees of freedom correction and uses Tukey’s
#'  studentized range distribution for computing the p-values. The test compares
#'  the difference between each pair of means with appropriate adjustment for
#'  the multiple testing. So there is no need to apply additional p-value
#'  corrections.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}
#'  for a worked walkthrough.
#'
#'@param effect.size logical. Default is FALSE. If TRUE, a \code{cohens.d} column
#'  and its \code{magnitude} are added. Because Games-Howell assumes unequal
#'  variances, this is the Welch (averaged-variance) Cohen's d,
#'  \code{estimate / sqrt((s1^2 + s2^2) / 2)}, oriented like the reported mean
#'  difference so the two never disagree in sign.
#'@inheritParams t_test
#'@return return a data frame with some of the following columns: \itemize{
#'  \item \code{.y.}: the y (outcome) variable used in the test. \item
#'  \code{group1,group2}: the compared groups in the pairwise tests. \item
#'  \code{n1,n2}: Sample counts. \item \code{estimate, conf.low, conf.high}:
#'  mean difference and its confidence intervals. \item \code{statistic}: Test
#'  statistic (t-value) used to compute the p-value. \item \code{df}: degrees of
#'  freedom calculated using Welch’s correction. \item \code{p.adj}: adjusted p-value using Tukey's method. \item
#'  \code{method}: the statistical test used to compare groups. \item
#'  \code{p.adj.signif}: the significance level of p-values. }
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'@details The Games-Howell method is an improved version of the Tukey-Kramer
#'  method and is applicable in cases where the equivalence of variance
#'  assumption is violated. It is a t-test using Welch’s degree of freedom. This
#'  method uses a strategy for controlling the type I error for the entire
#'  comparison and is known to maintain the preset significance level even when
#'  the size of the sample is different. However, the smaller the number of
#'  samples in each group, the it is more tolerant the type I error control.
#'  Thus, this method can be applied when the number of samples is six or more.
#'
#'  Because the test relies on Welch's variance correction, comparisons involving
#'  a group with zero variance (constant values) or undefined variance (a single
#'  observation) can be undefined; such pairs are returned as \code{NA} (with a
#'  warning) while all other comparisons are computed as usual.
#'
#'@references \itemize{ \item Aaron Schlege,
#'  https://rpubs.com/aaronsc32/games-howell-test. \item Sangseok Lee, Dong Kyu
#'  Lee. What is the proper way to apply the multiple comparison test?. Korean J
#'  Anesthesiol. 2018;71(5):353-360. }
#'
#'
#' @examples
#' # Simple test
#' ToothGrowth %>% games_howell_test(len ~ dose)
#'
#' # Grouped data
#' ToothGrowth %>%
#'   group_by(supp) %>%
#'   games_howell_test(len ~ dose)
#'
#'@rdname games_howell_test
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/anova/anova-in-r}{One-Way ANOVA in R}.
#'@export
games_howell_test <- function(data, formula, conf.level = 0.95, detailed = FALSE,
                              effect.size = FALSE){
  args <- as.list(environment()) %>%
    .add_item(p.adjust.method = "Tukey", method = "games_howell_test")
  if(!isTRUE(effect.size)) args <- remove_item(args, "effect.size")
  results <- data %>%
    doo(.games_howell_test, formula, conf.level = conf.level,
        effect.size = effect.size)
  if(!detailed){
    results <- results %>%
      select(
        -any_of(c("se", "method", "statistic",
        "df", "n1", "n2"))
      )
  }
  results %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "games_howell_test"))
}

.games_howell_test <- function(data, formula, conf.level = 0.95, effect.size = FALSE){
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  number.of.groups <- guess_number_of_groups(data, group)
  if(number.of.groups == 1){
    stop("all observations are in the same group")
  }

  data <- data %>%
    select(!!!syms(c(outcome, group))) %>%
    get_complete_cases() %>%
    .as_factor(group)

  x <- data %>% pull(!!outcome)
  g <- data %>% pull(!!group)
  if (!all(is.finite(g)))
    stop("all group levels must be finite")

  # Statistics for games howell tests
  grp.sizes <- tapply(x, g, length)
  nb.groups <- length(grp.sizes)
  grp.means <- tapply(x, g, mean)
  grp.vars <- tapply(x, g, stats::var)
  # Helper functions
  get_mean_diff <- function(i, j){
    grp.means[i] - grp.means[j]
  }
  get_weltch_sd <- function(i, j){
    sqrt((grp.vars[i]/grp.sizes[i]) + (grp.vars[j]/grp.sizes[j]))
  }
  get_degree_of_freedom <- function(i, j){
    A <- ((grp.vars[i]/grp.sizes[i]) + (grp.vars[j]/grp.sizes[j]))^2
    B <- ((grp.vars[i]/grp.sizes[i])^2)/(grp.sizes[i] - 1)
    C <- ((grp.vars[j]/grp.sizes[j])^2)/(grp.sizes[j] - 1)
    A/(B+C)
  }

  mean.diff <- stats::pairwise.table(
    get_mean_diff, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix()

  # A group with zero variance (constant values) or undefined variance (a single
  # observation, so var() = NA) makes the Welch sd and/or degrees of freedom
  # NA/NaN for the affected pairs; tidy_squared_matrix() drops those rows via
  # na.omit(), which would desynchronise these vectors from the full comparison
  # set and crash (#183). Align both derived tables back onto mean.diff (whose
  # group means are always finite, so it is the complete grid) by joining, so
  # undefined pairs become NA instead of being dropped or silently recycled.
  weltch.tab <- stats::pairwise.table(
    get_weltch_sd, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("welch.sd")
  df.tab <- stats::pairwise.table(
    get_degree_of_freedom, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("df")
  aligned <- mean.diff %>%
    left_join(weltch.tab, by = c("group1", "group2")) %>%
    left_join(df.tab, by = c("group1", "group2"))
  welch.value <- aligned$welch.sd
  df.value <- aligned$df

  t <- abs(mean.diff$value)/welch.value
  p <- stats::ptukey(t*sqrt(2), nb.groups, df.value, lower.tail = FALSE)
  se <- welch.value*sqrt(0.5)

  q <- stats::qtukey(p = conf.level, nb.groups, df = df.value)
  conf.high <- mean.diff$value + q*se
  conf.low <- mean.diff$value - q*se

  # Comparisons involving a group with zero or undefined variance can be
  # undefined (NA Welch sd or NA df): return NA (not Inf/NaN/recycled) for the
  # affected statistics and warn once.
  undefined <- is.na(df.value) | is.na(welch.value)
  if(any(undefined)){
    t[undefined] <- NA
    se[undefined] <- NA
    p[undefined] <- NA
    conf.high[undefined] <- NA
    conf.low[undefined] <- NA
    warning(
      "Some groups have zero or undefined variance (constant values or a single ",
      "observation). The affected Games-Howell comparisons are undefined and ",
      "are returned as NA.",
      call. = FALSE
    )
  }

  n1 <- grp.sizes[mean.diff$group1]
  n2 <- grp.sizes[mean.diff$group2]

  results <- mean.diff %>%
    rename(estimate = "value") %>%
    mutate(
      conf.low = conf.low, conf.high = conf.high,
      se = se, statistic = t, df = df.value, p.adj = p
    ) %>%
    add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    add_column(.y. = outcome, .before = "group1") %>%
    add_significance("p.adj") %>%
    mutate(method = "Games-Howell")
  if(isTRUE(effect.size)){
    # Welch (unequal-variance) Cohen's d = mean difference / sqrt((s1^2 + s2^2)/2)
    # -- the averaged-variance d that cohens_d(var.equal = FALSE) reports, and the
    # variance assumption Games-Howell itself makes. Built from the test's OWN
    # oriented `estimate`, so the d and the mean difference always share a sign.
    v1 <- as.numeric(grp.vars[results$group1])
    v2 <- as.numeric(grp.vars[results$group2])
    d <- results$estimate / sqrt((v1 + v2) / 2)
    results <- results %>%
      mutate(cohens.d = d, magnitude = get_cohens_magnitude(d))
  }
  results
}





