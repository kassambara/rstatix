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
#' @seealso \code{\link{tukey_hsd}()}, \code{\link{games_howell_test}()},
#'   \code{\link{dunn_test}()}, \code{\link{levene_test}()},
#'   \code{\link{shapiro_test}()}.
#'
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Assumptions hold here, so Tukey HSD is chosen
#' df %>% posthoc_test(len ~ dose)
#' @name posthoc_test
#' @export
posthoc_test <- function(data, formula, significance = 0.05, ...){
  if(is_grouped_df(data))
    stop("`posthoc_test()` does not support grouped data; call it on each ",
         "group separately.", call. = FALSE)
  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group))
    stop("`posthoc_test()` needs a grouping variable (`outcome ~ group`).",
         call. = FALSE)
  data <- data %>% .as_factor(group)
  if(guess_number_of_groups(data, group) < 2)
    stop("The grouping variable must have at least two levels.", call. = FALSE)

  # --- assumption checks ----------------------------------------------------
  # Normality is checked PER GROUP (Shapiro-Wilk on each group's values), not on
  # the pooled residuals: unequal variances turn the pooled residuals into a
  # scale mixture that fails normality, which would make Games-Howell (the
  # normal-but-unequal-variance branch) unreachable. Per-group normality is also
  # exactly the assumption Games-Howell makes. The reported normality p-value is
  # the smallest across groups (the group least consistent with normality), so a
  # single non-normal group routes to the non-parametric test. Homogeneity of
  # variance is Levene's test. A verdict that cannot be computed (a group with
  # too few or constant values) is treated as "assumption not met", so the
  # routing errs toward the more robust option.
  outcome.values <- data %>% dplyr::pull(!!outcome)
  group.values <- data %>% dplyr::pull(!!group)
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

  # --- route ----------------------------------------------------------------
  method <- if(!normal) "dunn_test"
            else if(equal.variance) "tukey_hsd"
            else "games_howell_test"
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
  res <- do.call(chosen, c(list(data, formula), dots))

  attr(res, "posthoc.method") <- method
  attr(res, "assumptions") <- list(
    normality.p = normality.p, homogeneity.p = homogeneity.p,
    normal = normal, equal.variance = equal.variance,
    significance = significance
  )
  class(res) <- c("posthoc_test", class(res))
  res
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
