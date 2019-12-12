#' @include utilities.R
NULL
#'Exact Multinomial Test
#'
#'@description Performs an exact multinomial test. Alternative to the chi-square test of goodness-of-fit-test when the sample
#'  size is small.
#'
#'@inheritParams binom_test
#'
#'@seealso \link{binom_test}
#'@return return a data frame containing the p-value and its significance.
#'
#'  The \strong{returned object has an attribute called args}, which is a list
#'  holding the test arguments.
#'
#' @examples
#' # Data
#' tulip <- c(red = 81, yellow = 50, white = 27)
#'
#' # Question 1: are the color equally common ?
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # this is a test of homogeneity
#' res <- multinom_test(tulip)
#' res
#'
#' attr(res, "descriptives")
#'
#' # Pairwise comparisons between groups
#' pairwise_binom_test(tulip, p.adjust.method = "bonferroni")
#'
#'
#' # Question 2: comparing observed to expected proportions
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # this is a goodness-of-fit test
#' expected.p <- c(red = 0.5, yellow = 0.33, white = 0.17)
#' res <- multinom_test(tulip, expected.p)
#' res
#' attr(res, "descriptives")
#'
#' # Pairwise comparisons against a given probabilities
#' pairwise_binom_test_against_p(tulip, expected.p)
#' @export
multinom_test <- function (x, p = rep(1/length(x), length(x)), detailed = FALSE)
{
  args <- as.list(environment()) %>%
    add_item(method = "exact_multinom_test")
  if (!is.vector(x)) {
    stop("'x' must be a vector")
  }
  if (sum(p) != 1) {
    stop("sum of probabilities must be 1")
  }
  if (length(x) != length(p)) {
    stop("'x' and 'p' lengths differ")
  }
  if(is.null(names(x))){
    names(x) <- paste0("grp", 1:length(x))
  }
  size <- sum(x)
  groups <- length(x)
  numEvents <- choose(size + groups - 1, groups - 1)
  pObs <- stats::dmultinom(x, size, p)
  findVectors <- function(groups, size) {
    if (groups == 1) {
      mat <- size
    }
    else {
      mat <- matrix(rep(0, groups - 1), nrow = 1)
      for (i in 1:size) {
        mat <- rbind(mat, findVectors(groups - 1, i))
      }
      mat <- cbind(mat, size - rowSums(mat))
    }
    mat
  }
  eventMat <- findVectors(groups, size)
  eventProb <- apply(eventMat, 1, function(x) stats::dmultinom(x, size, p))
  p.val <- sum(eventProb[eventProb <= pObs])
  results <- tibble(
    p = p.val, method = "Exact multinomial test"
  ) %>%
    add_significance() %>%
    select(.data$p, .data$p.signif, .data$method)
  descriptives <- tibble(
    group = names(x),
    observed = x, expected = p*size
  )
  if(!detailed){
    results <- results[, c("p", "p.signif")]
  }
  results %>%
    set_attrs(args = args, descriptives = descriptives) %>%
    add_class(c("rstatix_test", "exact_multinom_test"))
}
