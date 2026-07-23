#' @include utilities.R
NULL
#' Choose and Run the Appropriate Post-Hoc Test
#'
#' @description Given a one-way, independent-groups design
#'   (\code{outcome ~ group}), check the ANOVA assumptions and run the post-hoc
#'   test they imply, following the standard decision tree:
#'   \itemize{
#'   \item each group normal \strong{and} variances equal: Tukey HSD
#'     (\code{\link{tukey_hsd}()});
#'   \item each group normal \strong{but} variances unequal: Games-Howell
#'     (\code{\link{games_howell_test}()});
#'   \item at least one group not normal: Dunn's test
#'     (\code{\link{dunn_test}()}).
#'   }
#'   Normality is assessed \strong{per group} with the Shapiro-Wilk test applied
#'   to each group's values, routing on the smallest p-value across groups (a
#'   single non-normal group sends the data to the non-parametric test). This is
#'   deliberately not the pooled model residuals, which unequal variances would
#'   make non-normal and so hide the Games-Howell case. Homogeneity of variance
#'   is assessed with Levene's test (\code{\link{levene_test}()}). Both are judged
#'   at the \code{significance} level. The function returns the chosen test's
#'   usual pairwise result, with the selected method and the assumption verdicts
#'   attached (and shown when the result is printed), so the routing is
#'   transparent rather than hidden.
#'
#'   To check the same assumptions before the omnibus test and read off the
#'   recommended omnibus/post-hoc pair, use \code{\link{check_test_assumptions}()};
#'   its result can be passed back here through \code{.assumptions} so the checks
#'   are not repeated. Choosing a test by first testing its assumptions on the
#'   same data has a known cost --- see the note in \code{?check_test_assumptions}.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/assumptions/statistical-tests-and-assumptions}{Statistical Tests and Assumptions in R}
#'  for a worked walkthrough.
#'
#' @param data a data frame containing the variables in the formula.
#' @param formula a formula of the form \code{x ~ group} where \code{x} is a
#'   numeric outcome variable and \code{group} is a factor with two or more
#'   levels giving the independent groups.
#' @param significance the significance level used to judge the Shapiro-Wilk and
#'   Levene assumption tests. Default is 0.05.
#' @param ... additional arguments forwarded to the selected post-hoc test, but
#'   only those it accepts, so an argument meant for one route does not error on
#'   another. In particular \code{p.adjust.method} is honoured only when Dunn's
#'   test is chosen; Tukey HSD and Games-Howell carry their own built-in
#'   adjustment and ignore it.
#'
#' @return the pairwise comparison table returned by the selected post-hoc test
#'   (a \code{tukey_hsd}, \code{games_howell_test} or \code{dunn_test} object),
#'   additionally classed \code{posthoc_test}. The selected method and the
#'   assumption verdicts are stored in the attributes \code{"posthoc.method"} and
#'   \code{"assumptions"}, and printed above the table.
#'
#' @seealso \code{\link{check_test_assumptions}()}, \code{\link{tukey_hsd}()}, \code{\link{games_howell_test}()},
#'   \code{\link{dunn_test}()}, \code{\link{levene_test}()},
#'   \code{\link{shapiro_test}()}.
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/assumptions/statistical-tests-and-assumptions}{Statistical Tests and Assumptions in R}.
#'
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Assumptions hold here, so Tukey HSD is chosen
#' df %>% posthoc_test(len ~ dose)
#' @param .assumptions (optional) the tibble returned by
#'   \code{\link{check_test_assumptions}()} for the same data. When supplied its
#'   verdicts are used directly, so the Shapiro-Wilk and Levene tests are not run
#'   a second time (useful after \code{check_test_assumptions()} has already
#'   checked them). Default \code{NULL} (compute the assumptions here).
#' @param omnibus (optional) an omnibus test result --- from
#'   \code{\link{anova_test}()}, \code{\link{welch_anova_test}()} or
#'   \code{\link{kruskal_test}()} --- that you already ran. If the post-hoc route
#'   chosen here belongs to a different family than that omnibus --- the families
#'   being parametric equal-variance (ANOVA / Tukey), parametric unequal-variance
#'   (Welch / Games-Howell) and non-parametric (Kruskal-Wallis / Dunn) --- a
#'   warning is issued so the two stay coherent (for example an \code{anova_test()}
#'   omnibus followed by a Games-Howell route). Default \code{NULL} (no check).
#' @name posthoc_test
#' @export
posthoc_test <- function(data, formula, significance = 0.05, ...,
                         .assumptions = NULL, omnibus = NULL){
  route <- if(is.null(.assumptions)) choose_oneway_route(data, formula, significance)
           else as_oneway_route(.assumptions, data, formula)
  if(!is.null(omnibus)){
    omnibus.family <- get_omnibus_family(omnibus)
    if(!is.na(omnibus.family) && omnibus.family != route$family){
      warning(
        "The chosen post-hoc test (", route$posthoc, ", ", route$family,
        ") belongs to a different family than the supplied omnibus test (",
        omnibus.family, "). Consider using a coherent omnibus and post-hoc ",
        "pair, e.g. via check_test_assumptions().", call. = FALSE
      )
    }
  }
  method <- route$posthoc
  chosen <- switch(
    method, dunn_test = dunn_test, tukey_hsd = tukey_hsd,
    games_howell_test = games_howell_test
  )
  # Forward only the extra arguments the chosen test can accept. The routing is
  # data-dependent and the three tests have different signatures (e.g. only
  # dunn_test() takes p.adjust.method; games_howell_test() has no `...`), so an
  # argument meant for one route must not crash another.
  dots <- list(...)
  chosen.formals <- names(formals(chosen))
  if(!("..." %in% chosen.formals)) dots <- dots[names(dots) %in% chosen.formals]
  res <- do.call(chosen, c(list(route$data, formula), dots))

  attr(res, "posthoc.method") <- method
  attr(res, "assumptions") <- list(
    normality.p = route$normality.p, homogeneity.p = route$homogeneity.p,
    normal = route$normal, equal.variance = route$equal.variance,
    significance = route$significance
  )
  class(res) <- c("posthoc_test", class(res))
  res
}

# Shared one-way assumption-check + routing node used by both posthoc_test() and
# check_test_assumptions(), so the decision tree lives in exactly one place.
# Validates a one-way independent-groups design; checks normality PER GROUP
# (Shapiro-Wilk on each group's values, NOT the pooled residuals -- unequal
# variances turn those into a scale mixture that fails normality and would make
# the Games-Howell branch unreachable; per-group normality is also the assumption
# Games-Howell makes) and homogeneity of variance (Levene). The reported
# normality p-value is the smallest across groups. A verdict that cannot be
# computed (a group with too few or constant values) is treated as "not met", so
# the routing errs toward the more robust option. Returns the factored data, the
# verdicts, and the recommended omnibus and post-hoc test names.
choose_oneway_route <- function(data, formula, significance = 0.05){
  design <- validate_oneway_design(data, formula)
  data <- design$data
  outcome.values <- data %>% dplyr::pull(!!design$outcome)
  group.values <- data %>% dplyr::pull(!!design$group)
  per.group.p <- tapply(outcome.values, group.values, function(v){
    v <- v[!is.na(v)]
    tryCatch(stats::shapiro.test(v)$p.value, error = function(e) NA_real_)
  })
  normality.p <- if(anyNA(per.group.p)) NA_real_ else min(per.group.p)
  homogeneity.p <- tryCatch(
    dplyr::pull(levene_test(data, formula), "p"),
    error = function(e) NA_real_
  )
  normal <- !is.na(normality.p) && normality.p > significance
  equal.variance <- !is.na(homogeneity.p) && homogeneity.p > significance
  oneway_route(design, normality.p, homogeneity.p, normal, equal.variance, significance)
}

# Validate a one-way independent-groups design and factor the group; shared so
# the checks live in one place.
validate_oneway_design <- function(data, formula){
  if(is_grouped_df(data))
    stop("Grouped data are not supported here; call the function on each group ",
         "separately.", call. = FALSE)
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group))
    stop("A grouping variable is required (`outcome ~ group`).", call. = FALSE)
  if(length(group) > 1 || grepl("[+*:|]", group))
    stop("Only a one-way design (`outcome ~ group`, a single grouping variable) ",
         "is supported; got `", deparse(formula), "`. For a multi-factor design, ",
         "use anova_test() with emmeans_test() or tukey_hsd() directly.",
         call. = FALSE)
  data <- data %>% .as_factor(group)
  if(guess_number_of_groups(data, group) < 2)
    stop("The grouping variable must have at least two levels.", call. = FALSE)
  list(data = data, outcome = outcome, group = group)
}

# Assemble a route list from a validated design and the verdicts.
oneway_route <- function(design, normality.p, homogeneity.p, normal, equal.variance, significance){
  family <- if(!normal) "nonparametric" else if(equal.variance) "parametric" else "welch"
  list(
    data = design$data, outcome = design$outcome, group = design$group,
    normality.p = normality.p, homogeneity.p = homogeneity.p,
    normal = normal, equal.variance = equal.variance, significance = significance,
    family = family,
    omnibus = switch(family, parametric = "anova_test",
                     welch = "welch_anova_test", nonparametric = "kruskal_test"),
    posthoc = switch(family, parametric = "tukey_hsd",
                     welch = "games_howell_test", nonparametric = "dunn_test")
  )
}

# Rebuild a route list from a check_test_assumptions() tibble + the data WITHOUT
# recomputing Shapiro/Levene (that is the point of passing .assumptions).
as_oneway_route <- function(assumptions, data, formula){
  a <- keep_only_tbl_df_classes(assumptions)
  design <- validate_oneway_design(data, formula)
  oneway_route(
    design, a$normality.p[1], a$homogeneity.p[1],
    a$normal[1], a$equal.variance[1], a$significance[1]
  )
}

# Family of an omnibus test result, for the coherence warning.
get_omnibus_family <- function(omnibus){
  if(inherits(omnibus, "anova_test")) "parametric"
  else if(inherits(omnibus, "welch_anova_test")) "welch"
  else if(inherits(omnibus, "kruskal_test")) "nonparametric"
  else {
    # An unrecognized object (no stashed args list, no method, or a multi-length
    # method) yields NA -> no warning, rather than erroring on `$` or in switch().
    args <- attr(omnibus, "args")
    method <- if(is.list(args)) args$method else NULL
    if(length(method) != 1) return(NA_character_)
    switch(as.character(method),
      anova_test = "parametric", welch_anova_test = "welch",
      kruskal_test = "nonparametric", NA_character_)
  }
}

#' @param x an object of class \code{posthoc_test}.
#' @method print posthoc_test
#' @rdname posthoc_test
#' @export
print.posthoc_test <- function(x, ...){
  labels <- c(
    tukey_hsd = "Tukey HSD", games_howell_test = "Games-Howell",
    dunn_test = "Dunn test"
  )
  a <- attr(x, "assumptions")
  fmt <- function(p) if(is.na(p)) "not computable" else formatC(p, format = "f", digits = 3)
  cat("Post-hoc test chosen:", labels[attr(x, "posthoc.method")], "\n")
  cat(sprintf(
    "  Normality (Shapiro-Wilk, min across groups): p = %s -> %s\n",
    fmt(a$normality.p), if(a$normal) "normal" else "not normal"
  ))
  cat(sprintf(
    "  Homogeneity of variance (Levene): p = %s -> %s\n\n",
    fmt(a$homogeneity.p),
    if(a$equal.variance) "equal variances" else "unequal variances"
  ))
  y <- x
  class(y) <- setdiff(class(y), "posthoc_test")
  print(y, ...)
  invisible(x)
}
