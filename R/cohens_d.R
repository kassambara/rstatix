#' @include utilities.R
#' @importFrom stats sd
#' @importFrom stats var
NULL

#' Compute Cohen's d Measure of Effect Size
#'
#' @description Compute the effect size for t-test. T-test conventional effect
#'   sizes, poposed by Cohen, are: 0.2 (small efect), 0.5 (moderate effect) and
#'   0.8 (large effect).
#'
#' @param data a data.frame containing the variables in the formula.
#' @param formula a formula of the form \code{x ~ group} where \code{x} is a
#'   numeric variable giving the data values and \code{group} is a factor with
#'   one or multiple levels giving the corresponding groups. For example,
#'   \code{formula = TP53 ~ cancer_group}.
#' @param paired a logical indicating whether you want a paired test.
#' @param mu theoretical mean, use for one-sample t-test. Default is O.
#' @param var.equal a logical variable indicating whether to treat the two
#'   variances as being equal. If TRUE then the pooled variance is used to
#'   estimate the variance otherwise the Welch (or Satterthwaite) approximation
#'   to the degrees of freedom is used.
#' @return a data frame containing the Cohen's d and the magnitude.
#' @examples
#' # One-sample t test effect size
#' ToothGrowth %>% cohens_d(len ~ 1, mu = 0)
#'
#' # Two indepedent samples t-test effect size
#' ToothGrowth %>% cohens_d(len ~ supp, var.equal = TRUE)
#'
#' # Paired samples effect size
#' df <- data.frame(
#'   id = 1:5,
#'   pre  = c(110, 122, 101, 120, 140),
#'   post = c(150, 160, 110, 140, 155)
#' )
#' df <- df %>% gather(key = "treatment", value = "value", -id)
#' head(df)
#'
#' df %>% cohens_d(value ~ treatment, paired = TRUE)
#' @export
cohens_d <- function(data, formula, paired = FALSE, mu = 0, var.equal = FALSE){

  outcome <- get_formula_left_hand_side(formula)
  group <- get_formula_right_hand_side(formula)
  if(.is_empty(group))
    number.of.groups <- 1  # Null model
  else
    number.of.groups <- data %>%
    pull(group) %>% unique() %>% length()

  if(number.of.groups == 1){
    x <- data %>% pull(outcome)
    d <- one_sample_d(x, mu)
  }
  else if(number.of.groups == 2){
    groups <- data %>% pull(group)
    data <- data %>% split(groups)
    x <- data[[1]] %>% pull(outcome)
    y <- data[[2]] %>% pull(outcome)
    if(paired){
      d <- paired_sample_d(x, y)
    }
    else{
      d <- two_independent_sample_d(x, y, var.equal)
    }
  }
  else{
    stop("The grouping factors contain more than 2 levels.")
  }
  tibble(
    cohens.d = d,
    magnitude = get_cohens_magnitude(d)
  )
}


one_sample_d <- function(x, mu = 0){
  abs(mean(x) - mu)/sd(x)
}


two_independent_sample_d <- function(x, y, var.equal = TRUE){
  if(var.equal){
    squared.dev <- (c(x - mean(x), y - mean(y)))^2
    n <- length(squared.dev)
    SD <- sqrt(sum(squared.dev)/(n-2))
  }
  else {
    SD <- sqrt((var(x) + var(y))/2)
  }
  mean.diff <- mean(x) - mean(y)
  abs(mean.diff/SD)
}


paired_sample_d <- function(x, y){
  abs(mean(x-y)/sd(x-y))
}

get_cohens_magnitude <- function(d){
  magnitude.levels = c(0.2,0.5,0.8)
  magnitude = c("negligible","small","moderate","large")
  d.index <- findInterval(abs(d), magnitude.levels)+1
  magnitude <- factor(magnitude[d.index], levels = magnitude, ordered = TRUE)
  magnitude
}
