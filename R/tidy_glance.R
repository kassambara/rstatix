#' @include utilities.R
NULL

#' Tidy an rstatix Test Result
#'
#' @description \code{tidy()} and \code{glance()} methods for objects of class
#'   \code{rstatix_test} — the result of a test function such as
#'   \code{\link{t_test}()}, \code{\link{wilcox_test}()},
#'   \code{\link{anova_test}()} or \code{\link{kruskal_test}()}. The results are
#'   already tidy tibbles; these methods drop the internal \code{rstatix} classes
#'   and the stashed test arguments so the object passes cleanly to tools that
#'   dispatch on \code{\link[generics]{tidy}} / \code{\link[generics]{glance}},
#'   such as \code{broom}, \code{gtsummary} and \code{gt}. Correlation results
#'   (\code{\link{cor_test}()}, \code{\link{cor_mat}()}) carry a different class
#'   and are not covered by these methods.
#'
#' @param x an object of class \code{rstatix_test}, as returned by an
#'   \code{rstatix} test function.
#' @param ... not used; present for compatibility with the generics.
#'
#' @return \code{tidy()} returns the same result as a plain tibble, one row per
#'   comparison or model term, with the internal classes and the \code{args}
#'   attribute removed. \code{glance()} returns a one-row tibble with the test
#'   \code{method} and \code{n}, the number of rows in the result (the number of
#'   comparisons or model terms).
#'
#' @examples
#' res <- ToothGrowth %>% t_test(len ~ dose)
#'
#' # A plain tibble, ready for broom / gtsummary / gt
#' tidy(res)
#'
#' # One-row summary
#' glance(res)
#'
#' @rdname tidy.rstatix_test
#' @exportS3Method generics::tidy
tidy.rstatix_test <- function(x, ...){
  x <- rstatix_test_table(x)
  attr(x, "args") <- NULL
  tibble::as_tibble(keep_only_tbl_df_classes(x))
}

#' @rdname tidy.rstatix_test
#' @exportS3Method generics::glance
glance.rstatix_test <- function(x, ...){
  method <- attr(x, "args")$method
  if(is.null(method) || !nzchar(method)){
    method <- setdiff(class(x), c("rstatix_test", "tbl_df", "tbl", "data.frame", "list"))
    method <- if(length(method)) method[1] else NA_character_
  }
  # a grouped result is classed grouped_<test>; report the underlying test
  method <- sub("^grouped_", "", method)
  tibble::tibble(method = method, n = nrow(rstatix_test_table(x)))
}

# Most rstatix_test objects are already rectangular tibbles. anova_test() is the
# exception, in four shapes: a between-subjects result is already flat; a
# repeated-measures or mixed result is a list (the ANOVA table plus Mauchly's
# test and the sphericity corrections); and either can be grouped, in which case
# the object is a data frame carrying a packed `anova` list-column. get_anova_table()
# returns the corrected ANOVA table -- one row per term (per group) -- for all of
# them, and is a no-op on an already-flat table, so route every anova object
# through it BEFORE the plain data-frame case, or the grouped list-column slips out.
rstatix_test_table <- function(x){
  if(inherits(x, "anova_test") || inherits(x, "grouped_anova_test"))
    return(get_anova_table(x))
  if(is.data.frame(x)) return(x)
  stop("Cannot tidy this result: it is not a rectangular table.", call. = FALSE)
}
