#' @include utilities.R
NULL
#' Effect Size for ANOVA
#' @description Compute eta-squared and partial eta-squared for all terms in an
#'   ANOVA model.
#' @param model an object of class aov or anova.
#' @return a numeric vector with the effect size statistics
#' @describeIn eta_squared compute eta squared
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Compute ANOVA
#' res.aov <- aov(len ~ supp*dose, data = df)
#' summary(res.aov)
#'
#' # Effect size
#' eta_squared(res.aov)
#' partial_eta_squared(res.aov)

#' @export
eta_squared <- function(model){
  model %>%
    aov_stat_summary() %>%
    aov_stat_core("eta")
}

#' @describeIn eta_squared compute partial eta squared.
#' @export
partial_eta_squared <- function(model){
  model %>%
    aov_stat_summary() %>%
    aov_stat_core("peta")
}


aov_stat_summary <- function (model)
{
  if (!inherits(model, c("aov", "anova")))
    model <- stats::anova(model)
  aov.sum <- broom::tidy(model)
  if (!tibble::has_name(aov.sum, "meansq"))
    aov.sum <- tibble::add_column(aov.sum, meansq = aov.sum$sumsq/aov.sum$df,
                                  .after = "sumsq")
  aov.sum
}

aov_stat_core <- function(aov.sum, type){
    meansq.resid <- aov.sum[["meansq"]][nrow(aov.sum)]
    ss.total <- sum(aov.sum[["sumsq"]])
    ss.resid <- aov.sum[["sumsq"]][nrow(aov.sum)]
    n_terms <- nrow(aov.sum) - 1
    if (type == "omega") {
      aovstat <- purrr::map_dbl(1:n_terms, function(x) {
        ss.term <- aov.sum[["sumsq"]][x]
        df.term <- aov.sum[["df"]][x]
        (ss.term - df.term * meansq.resid)/(ss.total + meansq.resid)
      })
    }
    else if (type == "eta") {
      aovstat <- purrr::map_dbl(1:n_terms, ~aov.sum[["sumsq"]][.x]/sum(aov.sum[["sumsq"]]))
    }
    else if (type %in% c("cohens.f", "peta")) {
      aovstat <- purrr::map_dbl(1:n_terms, ~aov.sum[["sumsq"]][.x]/(aov.sum[["sumsq"]][.x] +
                                                                      ss.resid))
    }
    if (type == "cohens.f")
      aovstat <- sqrt(aovstat/(1 - aovstat))
    names(aovstat) <- aov.sum[["term"]][1:n_terms]
    aovstat
  }


