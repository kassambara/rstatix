#' @include utilities.R
NULL

#' Remove Non-Significant from Statistical Tests
#' @description Filter out non-significant (NS) p-values from a statistical
#'   test. Can detect automatically p-value columns
#' @param stat.test statistical test results returned by \code{rstatix}
#'   functions or any data frame containing a p-value column.
#' @param col (optional) character specifying the column containing the p-value
#'   or the significance information, to be used for the filtering step.
#'   Possible values include: \code{"p"}, \code{"p.adj"}, \code{"p.signif"},
#'   \code{"p.adj.signif"}. If missing, the function will automatically look for
#'   p.adj.signif, p.adj, p.signif, p in this order.
#' @param signif.cutoff the significance cutoff; default is 0.05. Significance
#'   is declared at \code{p-value <= signif.cutoff}
#' @return a data frame
#' @examples
#' # Statistical test
#' stat.test <- PlantGrowth %>% wilcox_test(weight ~ group)
#' # Remove ns: automatic detection of p-value columns
#' stat.test %>% remove_ns()
#' # Remove ns by the column p
#' stat.test %>% remove_ns(col ="p")
#' @export
remove_ns <- function(stat.test, col = NULL, signif.cutoff = 0.05){

  if(is.null(col)) col <- "any"
  else if(is.na(col)) col <- "any"
  else if(is.logical(col) ){
    if(is.na(col)) col <- "any"
    else if(col == TRUE) col <- "any"
    else if(col == FALSE) return(stat.test)
  }
  if(col == "any"){
    p.adj <- p_adj_names()
    p.adj.signif <- paste0(p.adj, ".signif")
    p <- p_names()
    p.signif <- paste0(p, ".signif")
    possible.cols <- c(p.adj.signif, p.adj, p.signif, p)
    if(!missing(signif.cutoff)) {
      # numeric columns are checked first
      possible.cols <- c(p.adj, p, p.adj.signif, p.signif)
    }
    col <- intersect(possible.cols, colnames(stat.test))
    if(length(col) > 1) col <- col[1]
    else if(length(col) == 0) {
      warning("Specify a column for filtering out ns.",
              "Can't found any automatically", call. = FALSE)
    }
  }

  if(col %in% colnames(stat.test)){
    .value <- stat.test[[col]]
    if(is.numeric(.value)) stat.test <- filter(stat.test, .value <= signif.cutoff)
    else if(is.character(.value)) stat.test <- filter(stat.test, !(.value %in% c("ns", "NS")))
  }
  else{
    stop("Can't find the column `", col, ", in the data", call. = FALSE)
  }
  stat.test
}

