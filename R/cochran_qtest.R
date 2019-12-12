#' @include utilities.R
NULL
#'Cochran's Q Test
#'@description Performs the Cochran's Q test for unreplicated randomized block
#'  design experiments with a binary response variable and paired data. This
#'  test is analogue to the \code{\link{friedman.test}()} with 0,1 coded
#'  response. It's an extension of the McNemar Chi-squared test for comparing
#'  more than two paired proportions.
#'@param data a data frame containing the variables in the formula.
#'@param formula a formula of the form \code{a ~ b | c}, where \code{a} is the
#'  outcome variable name; b is the within-subjects factor variables; and c
#'  (factor) is the column name containing individuals/subjects identifier.
#'  Should be unique per individual.
#'@examples
#' # Generate a demo data
#' mydata <- data.frame(
#'   outcome = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
#'   treatment = gl(3,1,30,labels=LETTERS[1:3]),
#'   participant = gl(10,3,labels=letters[1:10])
#' )
#' mydata$outcome <- factor(
#'   mydata$outcome, levels = c(1, 0),
#'   labels = c("success", "failure")
#'   )
#' # Cross-tabulation
#' xtabs(~outcome + treatment, mydata)
#'
#' # Compare the proportion of success between treatments
#' cochran_qtest(mydata, outcome ~ treatment|participant)
#'
#' # pairwise comparisons between groups
#' pairwise_mcnemar_test(mydata, outcome ~ treatment|participant)
#'
#'@export
cochran_qtest <- function(data, formula){
  args <- as.list(environment()) %>%
    add_item(method = "cochran_qtest")
 friedman_test(data, formula) %>%
    mutate(method = "Cochran's Q test") %>%
   remove_class("friedman_test") %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "cochran_qtest"))
}

# http://geai.univ-brest.fr/carpentier/2008-2009/Notes-Cochran-Q.pdf
exact_cochran_qtest <- function(data, formula, nboot = 500)
{
  # Data preparation
  data <- data %>% select(!!!syms(all.vars(formula)))
  colnames(data) <- c("outcome", "groups", "participant")
  nb.outcome <- length(unique(data$outcome))
  if(nb.outcome > 2 | nb.outcome == 1){
    stop("Unique possible outcome values should be 2")
  }
  # Convert outcome into 0/1
  if(!is.numeric(data$outcome)){
    data$outcome <- as.numeric(as.factor(data$outcome)) - 1
  }
  if(!all(unique(data$outcome) %in% c(0, 1))){
    stop("Outcome values should be 0 or 1")
  }
  data.wide <- data %>%
    spread(key = "groups", value = "outcome") %>%
    select(-.data$participant)
  nb.row <- nrow(data.wide)
  nb.col <- ncol(data.wide)

  results <- cochran_qtest(data, outcome ~ groups|participant)
  qobs <- results$statistic
  freq <- 0

  perm <- permutations(nb.col)
  # perm.list <- purrr::array_tree(perm)

  for (boot in 1:nboot) {
    data.permutated <- data.wide
    k <- 1+ as.integer(stats::runif(nb.row)*gamma(nb.col+1))
    for (j in 1:nb.row) {
      k <- 1+ as.integer(stats::runif(1)*gamma(nb.col+1))
      data.permutated[j,] <- data.wide[j, perm[k,]]
    }
    qperm <- get_cochran_q(data.permutated)
    if (qperm >= qobs) {freq <- freq + 1}}

  results %>%
    select(-.data$df) %>%
    mutate(
      p = freq/nboot,
      method = "Exact Cochran's Q test"
    )
}


# e1071::permutations
# Returns a matrix containing all permutations of the integers 1:n (one permutation per row).
permutations <- function (n)
{
  if (n == 1)
    return(matrix(1))
  else if (n < 2)
    stop("n must be a positive integer")
  z <- matrix(1)
  for (i in 2:n) {
    x <- cbind(z, i)
    a <- c(1:i, 1:(i - 1))
    z <- matrix(0, ncol = ncol(x), nrow = i * nrow(x))
    z[1:nrow(x), ] <- x
    for (j in 2:i - 1) {
      z[j * nrow(x) + 1:nrow(x), ] <- x[, a[1:i + j]]
    }
  }
  dimnames(z) <- NULL
  z
}


get_cochran_q <- function(data.wide){
  # Compute rows and column totals
  row.total <- apply(data.wide, 1, sum)
  column.total <- apply(data.wide, 2, sum)
  grand.total <- sum(data.wide)
  k <- ncol(data.wide)
  # Cochran's Q test statistic
  numerator <- sum((column.total - (grand.total/k))^2)
  denominator <- sum(row.total * (k - row.total))
  q = k*(k-1)*(numerator/denominator)
  q
}

