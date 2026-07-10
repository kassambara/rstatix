#' @include utilities.R
NULL

#' Tidy an rstatix Test Result
#'
#' @description \code{tidy()} and \code{glance()} methods for the objects
#'   returned by the \code{rstatix} test functions (\code{\link{t_test}()},
#'   \code{\link{wilcox_test}()}, \code{\link{anova_test}()},
#'   \code{\link{kruskal_test}()}, and the rest). The results are already tidy
#'   tibbles; these methods drop the internal \code{rstatix} classes and the
#'   stashed test arguments so the object passes cleanly to tools that dispatch
#'   on \code{\link[generics]{tidy}} / \code{\link[generics]{glance}}, such as
#'   \code{broom}, \code{gtsummary} and \code{gt}.
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
  attr(x, "args") <- NULL
  tibble::as_tibble(keep_only_tbl_df_classes(x))
}

#' @rdname tidy.rstatix_test
#' @exportS3Method generics::glance
glance.rstatix_test <- function(x, ...){
  method <- attr(x, "args")$method
  if(is.null(method) || !nzchar(method)){
    method <- setdiff(class(x), c("rstatix_test", "tbl_df", "tbl", "data.frame"))
    method <- if(length(method)) method[1] else NA_character_
  }
  tibble::tibble(method = method, n = nrow(x))
}
