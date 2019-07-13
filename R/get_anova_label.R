#' @include utilities.R
NULL
#' Extract Automatically Label from ANOVA results
#' @description Extract information rom ANOVA results. Useful for labelling plots
#'   with ANOVA test outputs.
#' @param res.aov an object of class \code{anova_test} as returned by the
#'   function \code{\link{anova_test}}.
#' @param description the test description used as the prefix of the label.
#'   Examples of description are "ANOVA", "Two Way ANOVA". To remove the
#'   default description, specify \code{description = NULL}.
#' @param type the label type. Can be one of "text" and "expression". Partial
#'   match allowed. If you want to add the label onto a ggplot, it might be
#'   useful to specify \code{type = "expresion"}.
#' @param detailed logical value. If TRUE, returns detailed label.
#' @return a text label or an expression to pass to a plotting function.
#' @examples
#' # Perform ANOVA test
#' res.aov <- PlantGrowth %>% anova_test(weight ~ group)
#' res.aov
#'
#' # Extract information
#' get_anova_label(res.aov, description = "One Way ANOVA")
#'
#' # Detailed information
#' get_anova_label(res.aov, description = "One Way ANOVA", detailed = TRUE)
#' @rdname get_anova_label
#' @export
get_anova_label <- function(res.aov, description = "Anova", type = c("text", "expression"),
                            detailed = FALSE){
  type = match.arg(type)
  if(!inherits(res.aov, "anova_test")){
    stop("An object of class anova_test required")
  }
  n <- get_anova_sample_size(res.aov)
  if(inherits(res.aov, "list")){
    res.aov <- res.aov$ANOVA
  }
  if(!is.null(description)){
    if(description != ""){
      description <- paste0(description, ", ")
    }
  }
  if(type == "expression"){
    get_anova_label.expression(res.aov, description, n, detailed)
  }
  else{
    get_anova_label.text(res.aov, description, n, detailed)
  }
}

get_anova_sample_size <- function(res.aov){
  anova.args <- attr(res.aov, "args")
  wid <- anova.args$wid
  if(is.null(wid)){
    size <- nrow(anova.args$data)
  }
  else{
    size <- anova.args$data %>%
      pull(!!wid) %>% unique() %>%
      length()
  }
  size
}

get_anova_label.expression <- function(res.aov, description = "Anova, ", n = NULL, detailed = FALSE){
  # take the p-value of the last rows
  res.aov <- utils::tail(res.aov, 1)
  effect.size.value <- roundif(get_anova_effect_size_value(res.aov), 2)
  effect.size.text <- get_anova_effect_size_text(res.aov, type = "expression")
  if(detailed){
    substitute(
      expr = paste(
        description,
        statistic.text, "(", parameter, ",", parameter2, ") = ",
        statistic, ", ",
        italic("p"), " = ", p, ", ",
        effect.size.text, " = ", effect.size.value, ", ",
        italic("n"), " = ", n
      ),
      env = list(
        description = description,
        statistic.text = quote(italic("F")),
        parameter = roundif(res.aov$DFn, 2), parameter2 = roundif(res.aov$DFd, 2),
        statistic = roundif(res.aov$F, 2), p = res.aov$p,
        effect.size.text = effect.size.text, effect.size.value = effect.size.value,
        n = n
      )
    )
  }
  else{
    substitute(
      expr = paste(description, italic("p"), " = ", p),
      env = list(description = description, p = res.aov$p)
    )
  }
}


get_anova_label.text <- function(res.aov, description = "Anova, ", n = NULL, detailed = FALSE){
  # take the p-value of the last rows
  res.aov <- utils::tail(res.aov, 1)
  effect.size.value <- roundif(get_anova_effect_size_value(res.aov), 2)
  effect.size.text <- get_anova_effect_size_text(res.aov, type = "text")
  if(detailed){
    paste0(
      description,
      "F(", roundif(res.aov$DFn, 2), ",", roundif(res.aov$DFd, 2), ") = ",
      roundif(res.aov$F, 2), ", p = ", res.aov$p, ", ",
      effect.size.text, " = ", effect.size.value, ", n = ", n
    )
  }
  else{
    paste0(description, "p = ", res.aov$p)
  }

}


get_anova_effect_size_text <- function(res.aov, type = "expression"){
  if("ges" %in% colnames(res.aov)) {
    if(type == "expression") quote(eta["g"]^2)
    else "eta2[g]"
  }
  else if("pes" %in% colnames(res.aov)) {
    if(type == "expression") quote(eta["p"]^2)
    else "eta2[p]"
  }
}
get_anova_effect_size_value <- function(res.aov){
  if("ges" %in% colnames(res.aov)) res.aov$ges
  else if("pes" %in% colnames(res.aov)) res.aov$pes
}

