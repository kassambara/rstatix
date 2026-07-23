#' @include utilities.R posthoc_test.R
NULL
#' Check One-Way Assumptions and Recommend the Test
#'
#' @description For a one-way, independent-groups design
#'   (\code{outcome ~ group}), check the two assumptions that decide which family
#'   of tests is appropriate --- normality (Shapiro-Wilk, per group) and
#'   homogeneity of variance (Levene) --- and return the verdicts together with
#'   the recommended omnibus and post-hoc test:
#'   \itemize{
#'   \item each group normal \strong{and} equal variances: \code{anova_test()} +
#'     \code{tukey_hsd()};
#'   \item each group normal \strong{but} unequal variances: \code{welch_anova_test()}
#'     + \code{games_howell_test()};
#'   \item at least one group not normal: \code{kruskal_test()} + \code{dunn_test()}.
#'   }
#'   The result is a tidy one-row tibble, so the same single assumption check can
#'   drive both the omnibus and the post-hoc coherently --- run the recommended
#'   omnibus, then pass the result to \code{\link{posthoc_test}()} via its
#'   \code{.assumptions} argument to avoid re-checking.
#'
#'   \strong{A note on choosing a test from the data.} Selecting the test by
#'   first testing its assumptions on the same data is convenient but has a known
#'   cost: the assumption gate is least reliable exactly when it matters
#'   (Shapiro-Wilk has little power at small n and rejects trivial departures at
#'   large n), and conditioning the choice on it makes the p-value of the test
#'   finally run no longer the exact nominal quantity. A common alternative is to
#'   skip the gate and use a robust method unconditionally --- Welch ANOVA with
#'   Games-Howell (which reduce to the classic result when variances are equal)
#'   or a rank-based test. Treat this recommendation as guidance, not a
#'   substitute for judgement.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/biostatistics/assumptions/statistical-tests-and-assumptions}{Statistical Tests and Assumptions in R}
#'  for a worked walkthrough.
#'
#' @param data a data frame containing the variables in the formula.
#' @param formula a formula of the form \code{x ~ group} where \code{x} is a
#'   numeric outcome and \code{group} is a factor with two or more levels.
#' @param significance the significance level used to judge the Shapiro-Wilk and
#'   Levene tests. Default is 0.05.
#'
#' @return a one-row tibble with the columns \code{.y.} (the outcome),
#'   \code{normality.p} (the smallest Shapiro-Wilk p across groups),
#'   \code{homogeneity.p} (Levene's p), the logical verdicts \code{normal} and
#'   \code{equal.variance}, the \code{significance} used, and the recommended
#'   \code{omnibus} and \code{posthoc} test names.
#'
#' @seealso \code{\link{posthoc_test}()}, \code{\link{shapiro_test}()},
#'   \code{\link{levene_test}()}.
#'   The Datanovia tutorial: \href{https://www.datanovia.com/learn/biostatistics/assumptions/statistical-tests-and-assumptions}{Statistical Tests and Assumptions in R}.
#'
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' df %>% check_test_assumptions(len ~ dose)
#' @export
check_test_assumptions <- function(data, formula, significance = 0.05){
  route <- choose_oneway_route(data, formula, significance)
  tibble::tibble(
    .y. = route$outcome,
    normality.p = route$normality.p,
    homogeneity.p = route$homogeneity.p,
    normal = route$normal,
    equal.variance = route$equal.variance,
    significance = route$significance,
    omnibus = route$omnibus,
    posthoc = route$posthoc
  )
}
