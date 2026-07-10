#' References and related packages
#'
#' @description
#' Where the methods in \code{rstatix} come from, and which other packages
#' implement them.
#'
#' \code{rstatix} implements its statistical methods in base R, from the
#' published formulas, with the exception noted under \emph{Adapted code} below.
#' Each function documents the source of its method in its own \code{References}
#' section; cite those authors, not this package, when you report a result.
#'
#' @section Method sources:
#' \itemize{
#'   \item \strong{Cramer's V} — Cramer, H. (1946) \emph{Mathematical Methods of
#'   Statistics}. See \code{\link{cramer_v}()}.
#'
#'   \item \strong{Effect-size confidence intervals} (partial eta squared,
#'   Cramer's V) — obtained by inverting a noncentral distribution: Smithson, M.
#'   (2003) \emph{Confidence Intervals}; Steiger, J. H. (2004) Beyond the F test.
#'   \emph{Psychological Methods}, 9, 164-182. See \code{\link{anova_test}()} and
#'   \code{\link{cramer_v}()}.
#'
#'   \item \strong{Conover's all-pairs test} — Conover, W. J. (1999)
#'   \emph{Practical Nonparametric Statistics}, 3rd edition. See
#'   \code{\link{conover_test}()} and \code{\link{friedman_conover_test}()}.
#'
#'   \item \strong{Nemenyi's all-pairs test} — Nemenyi, P. (1963)
#'   \emph{Distribution-free Multiple Comparisons}. See
#'   \code{\link{friedman_nemenyi_test}()}.
#'
#'   \item \strong{Compact letter display} — Piepho, H.-P. (2004) An algorithm
#'   for a letter-based representation of all-pairwise comparisons. \emph{Journal
#'   of Computational and Graphical Statistics}, 13, 456-466. See
#'   \code{\link{add_cld}()}.
#'
#'   \item \strong{Dunnett's many-to-one comparisons} — Dunnett, C. W. (1955) A
#'   multiple comparison procedure for comparing several treatments with a
#'   control. \emph{Journal of the American Statistical Association}, 50,
#'   1096-1121. See \code{\link{dunnett_test}()}.
#' }
#'
#' @section Related packages:
#' The following packages implement some of the same methods. \code{rstatix}
#' compares its results against them while developing, and they offer
#' functionality that \code{rstatix} does not:
#'
#' \itemize{
#'   \item \code{effectsize} — a broad effect-size toolkit;
#'   \code{effectsize::cramers_v()} and \code{effectsize::eta_squared()} produce
#'   the same intervals as \code{\link{cramer_v}(ci = TRUE)} and
#'   \code{\link{anova_test}(ci = )}.
#'   \item \code{DescTools} — \code{DescTools::CramerV()} and
#'   \code{DescTools::DunnettTest()}.
#'   \item \code{multcomp} — \code{multcomp::glht()} for general linear
#'   hypotheses, including Dunnett contrasts.
#'   \item \code{multcompView} — \code{multcompView::multcompLetters()} for
#'   compact letter displays.
#'   \item \code{PMCMRplus} — a large collection of all-pairs and many-to-one
#'   nonparametric procedures, including the Conover and Nemenyi tests.
#' }
#'
#' These packages are not dependencies of \code{rstatix}. The values they return
#' are recorded in the \code{rstatix} test suite as fixed numbers, checked
#' against the package they came from at the time they were written; a recorded
#' number cannot detect a later change in the package that produced it.
#'
#' @section Adapted code:
#' \code{\link{sign_test}()} is the exception to the description above. Its
#' one- and two-sample test code, and the confidence interval it reports for the
#' median, are adapted with modifications from \code{DescTools::SignTest()} and
#' \code{DescTools::MedianCI()}, written by Andri Signorell. \code{DescTools} is
#' distributed under GPL (>= 2); \code{rstatix} is distributed under GPL-2.
#' Every other function listed above is written from the published formula.
#'
#' @seealso \code{\link{cramer_v}()}, \code{\link{anova_test}()},
#'   \code{\link{conover_test}()}, \code{\link{add_cld}()},
#'   \code{\link{dunnett_test}()}.
#' @name rstatix-references
NULL
