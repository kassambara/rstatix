#' @include utilities.R add_significance.R pull_triangle.R
NULL
#' Add Significance Levels To a Correlation Matrix
#' @description Combines correlation coefficients and significance levels in a
#'   correlation matrix data.
#' @inheritParams add_significance
#' @param x an object of class \code{\link{cor_mat}()}.
#' @return a data frame containing the lower triangular part of the correlation
#'   matrix marked by significance symbols.
#' @examples
#' mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_mat() %>%
#'   cor_mark_significant()
#' @export
cor_mark_significant <- function(x, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05,  1),
                                 symbols = c("****", "***", "**", "*",  ""))
  {
  if(!inherits(x, c("cor_mat", "cor_mat_tri")))
    stop("x should be an object of class cor_mat or cor_mat_tri.")

  cor <- p.signif <- var1 <- var2 <- NULL
  res <- x %>%
    cor_gather (drop.na = FALSE) %>%
    add_significance(cutpoints = cutpoints, symbols = symbols) %>%
    mutate(cor = paste0(cor, p.signif)) %>%
    select(var1, var2, cor) %>%
    cor_spread()

  if(inherits(x, "upper_tri"))
    res %>% pull_upper_triangle()
  else
    res %>% pull_lower_triangle()
}

