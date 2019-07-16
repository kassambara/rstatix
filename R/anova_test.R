#' @include utilities.R factorial_design.R anova_summary.R
NULL
#'Anova Test
#'
#'
#'@description Provides a pipe-friendly framework to perform different types of
#'  ANOVA tests, including: \itemize{ \item \strong{Independent measures ANOVA}:
#'  between-Subjects designs, \item \strong{Repeated measures ANOVA}:
#'  within-Subjects designs \item \strong{Mixed ANOVA}: Mixed within within- and
#'  between-Subjects designs, also known as split-plot ANOVA. }
#'
#'  The function is an easy to use wrapper around \code{\link[car]{Anova}()} and
#'  \code{\link[stats]{aov}()}. It makes ANOVA computation handy in R and It's
#'  highly flexible: can support model and formula as input. Variables can be
#'  also specified as character vector using the arguments \code{dv, wid,
#'  between, within, covariate}.
#'
#'  The results include ANOVA table, generalized effect size and some assumption
#'  checks.
#'
#'
#'@param data a data.frame or a model to be analyzed.
#'@param formula a formula specifying the ANOVA model similar to
#'  \link[stats]{aov}. Can be of the form \code{y ~ group} where \code{y} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'
#'  Examples of supported formula include: \itemize{ \item Between-Ss ANOVA
#'  (independent measures ANOVA): \code{y ~ b1*b2} \item Within-Ss ANOVA (repeated
#'  measures ANOVA): \code{y ~ w1*w2 + Error(id/(w1*w2))} \item Mixed ANOVA: \code{y ~
#'  b1*b2*w1 + Error(id/w1)} }
#'
#'  If the formula doesn't contain any within vars, a linear model is directly
#'  fitted and  passed to the ANOVA function. For repeated designs, the ANOVA
#'  variables are parsed from the formula.
#'
#'@param dv (numeric) dependent variable name.
#'@param wid (factor) column name containing individuals/subjects identifier.
#'  Should be unique per individual.
#'@param between (optional) between-subject factor variables.
#'@param within (optional) within-subjects factor variables
#'@param covariate (optional) covariate names (for ANCOVA)
#'@param type the type of sums of squares for ANOVA. Allowed values are either
#'  1, 2 or 3. \code{type = 2} is the default because this will yield identical
#'  ANOVA results as type = 1 when data are balanced but type = 2 will
#'  additionally yield various assumption tests where appropriate. When the data
#'  are unbalanced the \code{type = 3} is used by popular commercial softwares
#'  including SPSS.
#'@param effect.size the effect size to compute and to show in the ANOVA
#'  results. Allowed values can be either "ges" (generalized eta squared) or
#'  "pes" (partial eta squared) or both. Default is "ges".
#'@param white.adjust Default is FALSE. If TRUE, heteroscedasticity correction
#'  is applied to the coefficient of covariance matrix. Used only for
#'  independent measures ANOVA.
#'@param error (optional) for a linear model, an lm model object from which the
#'  overall error sum of squares and degrees of freedom are to be calculated.
#'  Read more in \code{\link[car]{Anova}()} documentation.
#'@param observed Variables that are observed (i.e, measured) as compared to
#'  experimentally manipulated. The default effect size reported (generalized
#'  eta-squared) requires correct specification of the observed variables.
#'@param detailed If TRUE, returns extra information (sums of squares columns,
#'  intercept row, etc.) in the ANOVA table.
#'@param x an object of class \code{Anova_test}
#' @param correction character. Which sphericity correction of the degrees of
#'   freedom should be reported for the within-subject factors (repeated
#'   measures). The default is set to \code{"GG"} corresponding to the
#'   Greenhouse-Geisser correction. Possible values are \code{"GG"}, \code{"HF"}
#'   (i.e., Hyunh-Feldt correction), \code{"none"} (i.e., no correction) and
#'   \code{"auto"} (apply automatically GG correction if the sphericity
#'   assumption is not for within-subject design.
#'@seealso \code{\link{anova_summary}()}, \code{\link{factorial_design}()}
#'@return return an object of class \code{anova_test} a data frame containing
#'  the ANOVA table for independent measures ANOVA.
#'
#'  However, for repeated/mixed measures ANOVA, a list containing the following
#'  components are returned: ANOVA table, Mauchly's Test for Sphericity,
#'  Sphericity Corrections. These table are described more in the documentation
#'  of the function \code{\link{anova_summary}()}.
#'
#'  The \strong{returned object has an attribute} called \code{args}, which is a
#'  list holding the arguments used to fit the ANOVA model, including: data, dv,
#'  within, between, type, model, etc.
#'
#'
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-way ANOVA test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% anova_test(len ~ dose)
#'
#' # Two-way ANOVA test
#' #:::::::::::::::::::::::::::::::::::::::::
#' df %>% anova_test(len ~ supp*dose)
#'
#' # Two-way repeated measures ANOVA
#' #:::::::::::::::::::::::::::::::::::::::::
#' df$id <- rep(1:10, 6) # Add individuals id
#' # Use formula
#' \donttest{
#' df %>% anova_test(len ~ supp*dose + Error(id/(supp*dose)))
#' }
#'
#'
#' # or use character vector
#' df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
#'
#' # Extract ANOVA table and apply correction
#' #:::::::::::::::::::::::::::::::::::::::::
#' res.aov <- df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
#' get_anova_table(res.aov, correction = "GG")
#'
#'
#' # Use model as arguments
#' #:::::::::::::::::::::::::::::::::::::::::
#' .my.model <- lm(yield ~ block + N*P*K, npk)
#' anova_test(.my.model)
#'
#'
#' @describeIn anova_test perform anova test
#'@export
anova_test <- function(data, formula, dv, wid, between, within, covariate, type = NULL,
                       effect.size = "ges", error = NULL,
                       white.adjust = FALSE, observed = NULL, detailed = FALSE){
  . <- NULL
  .args <- rlang::enquos(
    dv = dv, wid = wid, between = between,
    within = within, covariate = covariate) %>%
    get_quo_vars_list(data, .)
  if(!missing(formula)) .args$formula <- formula
  if(is_grouped_df(data)){
    results <- data %>% doo(
      ~anova_test(data = ., formula = .args$formula,
      dv = .args$dv, wid = .args$wid, between = .args$between,
      within = .args$within, covariate = .args$covariate,
      type = type, effect.size = effect.size, error = error,
      white.adjust = white.adjust, observed = observed, detailed = detailed),
      result = "anova"
    )
    return(results)
  }

  .args <- .args %>%
    .add_item(data = data, type = type, white.adjust = white.adjust) %>%
    check_anova_arguments() %>%
    .add_item(method = "anova_test")
  if(.args$type != 1) {
    if(is.null(error)) res.anova <- car_anova(.args)
    else res.anova <- car_anova(.args, error = error)
  }
  else if(.args$type == 1) res.anova <- stats_aov(.args)
  else stop("Something is wrong...")
  res.anova <- res.anova %>%
    anova_summary(
      effect.size = effect.size, detailed = detailed,
      observed = observed
      )
  class(res.anova) <- c("anova_test", class(res.anova), "rstatix_test")
  res.anova
}

#' @describeIn anova_test extract anova table from an object of class \code{anova_test}
#' @export
get_anova_table <- function(x, correction = c("auto", "GG", "HF", "none")){
  correction.method <- method <- match.arg(correction)
  if(method == "auto") method = "GG"
  if(!inherits(x, "anova_test")){
    stop("An object of class 'anova_test' required")
  }
  # Independent anova
  if(!inherits(x, "list")){
    return(x)
  }
  if(correction.method == "none"){
    return(x)
  }
  # repeated/mixed design
  # Get correction table from anova_test
  .args <- attr(x, "args")
  get_corrections_table <- function(x, method = c("GG", "HF")){
    method <- match.arg(method)
    pattern <- paste0("Effect|", method)
    corrections <- x$`Sphericity Corrections` %>%
      select(tidyselect::matches(pattern))
    colnames(corrections) <- c("Effect", "epsilon", "df", "p", "p<.05")
    corrections <- corrections %>%
      tidyr::separate(col = "df", into = c("DFn", "DFd"), sep = ", ", convert = TRUE) %>%
      mutate(method = method)
    corrections
  }
  res.aov <- x$ANOVA
  sphericity <- x$`Mauchly's Test for Sphericity`
  corrections <- get_corrections_table(x, method)
  # If auto apply correction only when sphericity is not assumed (Mauchly p < 0.05)
  if(correction.method == "auto"){
    corrections %<>% filter(sphericity$p <= 0.05)
  }
  if(nrow(corrections) > 0){
    rownames(res.aov) <- res.aov$Effect
    rownames(corrections) <- corrections$Effect
    cols.to.update <- c("DFn", "DFd", "p", "p<.05")
    rows.to.update <- rownames(corrections)
    res.aov[rows.to.update, cols.to.update] <- corrections[rows.to.update, cols.to.update]
    rownames(res.aov) <- 1:nrow(res.aov)
  }
  res.aov <- res.aov %>% set_attrs(args = .args)
  class(res.aov) <- c("anova_test", class(res.aov), "rstatix_test")
  res.aov
}

#' @rdname anova_test
#' @method print anova_test
#' @param ... additional arguments
#' @export
print.anova_test <- function(x, ...) {
  .args <- attr(x, "args")
  type <- switch(.args$type, `1` = "I", `2` = "II", `3` =  "III")
  cat("ANOVA Table (type", type, "tests)\n\n")
  if(inherits(x, "data.frame"))
    print.data.frame(x)
  else if(inherits(x, "list")){
    attr(x, "args") <- NULL
    class(x) <- "list"
    print(x)
  }
}

#' @rdname anova_test
#' @method plot anova_test
#' @export
plot.anova_test <- function(x, ...) {
  .args <- attr(x, "args")
  graphics::plot(.args$model,  ...)
}





# Check the arguments of ANOVA
# .args is a list
check_anova_arguments <- function(.args){
  if(!is.null(.args$formula)){
    .args <- get_anova_vars_from_formula(.args)
    if(is.null(.args$within)) .args$model <- fit_lm(.args)
  }
  if(inherits(.args$data, "aovlist")){
    stop("A model of class aovlist is not supported.")
  }
  else if(has_model(.args)){
    if(is.null(.args$type)) .args$type <- 2
    return(.args)
  }
  .args <- .args %>%
    check_factorial_design() %>%
    check_anova_type()
  .args
}

get_anova_vars_from_formula <- function(.args){
  formula <- .args$formula
  data <- .args$data
  vars <- all.vars(formula)
  stop_if_multiple_error_terms(formula)
  # Detect transformed responses:
  lhs <- all.names(formula[[2]])
  transf <- setdiff(lhs, all.vars(formula[[2]]))
  if (length(transf) == 0)
    transf = NULL
  if (!is.null(transf)) {
    origdv <- setdiff(lhs, transf)
    dv <- paste0(transf[1], ".", origdv)
    data[[dv]] <- eval(formula[[2]], envir = data)  # add transformed version
    vars <- vars[!(vars %in% lhs)]
  }else {
    dv <- vars[1]
    vars <- vars[-1]
  }
  error.vars <- get_formula_error_vars(formula)
  id <- error.vars[1]
  within <- error.vars[-1]
  between <- vars[!(vars %in% c(id, within))]
  if(length(within) == 0) within <- NULL
  if(length(between) == 0) between <- NULL
  if(is.na(id)) id <- NULL
  .args <- .args %>%
    .add_item(data = data, dv = dv, wid = id, between = between, within = within)
  .args
}

stop_if_multiple_error_terms <- function(formula){
  .terms <- stats::terms(formula, "Error")
  .error.terms <- attr(.terms, "specials")$Error
  if (length(.error.terms) > 1L)
    stop(sprintf("there are %d Error terms: only 1 is allowed", length(.error.terms)))
}


# stop if ancova with repeated variables
stop_if_repeated_ancova <- function(.args){
  if(is_repeated_ancova(.args) | is_mixed_ancova(.args)){
    stop("Don't support ANCOVA with repeated measures")
  }
  .args
}

# Check anova design and type
is_design_balanced <- function(.args){
  res <- .args$data %>%
    group_by(!!!syms(.args$between)) %>%
    summarise(count = n())
  length(unique(res$count)) == 1
}
is_repeated_anova <- function(.args){
  is.null(.args$between) & !is.null(.args$within) & is.null(.args$covariate)
}
is_independent_anova <- function(.args){
  !is.null(.args$between) & is.null(.args$within) & is.null(.args$covariate)
}
is_mixed_anova <- function(.args){
  !is.null(.args$between) & !is.null(.args$within) & is.null(.args$covariate)
}
is_repeated_ancova <- function(.args){
  !is.null(.args$within) & !is.null(.args$covariate) & is.null(.args$between)
}
is_independent_ancova <- function(.args){
  is.null(.args$within) & !is.null(.args$covariate) & !is.null(.args$between)
}
is_mixed_ancova <- function(.args){
  !is.null(.args$between) & !is.null(.args$within) & !is.null(.args$covariate)
}

# Check anova type
check_anova_type <- function(.args){
  n.vars <- length(c(.args$between, .args$within))
  if(is.null(.args$type)){
    .args$type <- 2
    if(is_repeated_anova(.args)) .args$type <- 3
    else if(!is.null(.args$between)) {
      if(!is_design_balanced(.args) & n.vars > 1) .args$type <- 3
    }
  }
  else if (.args$type == 1){
    if(!is_design_balanced(.args) & n.vars > 1){
      warning("Your data are unbalanced and there are more than one variable. ",
              "In this case, using 'type = 1' is not recommended. ",
              "Consider using type 3 ANOVA.", immediate.=TRUE, call.=FALSE)
    }
  }
  .args
}

# Model
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_model <- function(object){
  models <- c("lm", "aov", "glm", "multinom", "polr", "mlm", "manova")
  inherits(object,  models)
}
# Get anova model from the list of arguments
get_anova_model <- function(.args){
  if(!is.null(.args$model)) return(.args$model)
  else if(is_model(.args$data)) return(.args$data)
  else stop("No model detected in ANOVA arguments")
}
# Check if ANOVA arguments contain model
has_model <- function(.args){
  !is.null(.args$model) | is_model(.args$data)
}


# Fit lm from formula and data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fit_lm <- function(.args){
  .args <- remove_missing_values_in_data(.args)
  lm_data <- droplevels(.args$data)
  lm_formula <- .args$formula
  stats::lm(lm_formula, lm_data)
}

# Compute the different types of ANOVA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
car_anova <- function(.args, ...){
  if(has_model(.args)){
    .model <- get_anova_model(.args)
    res.anova <- car::Anova(
      .model, type = .args$type,
      white.adjust = .args$white.adjust, ...
    )
    .args$model <- .model
  }
  else{
    design <- factorial_design(
      data = .args$data, dv = .args$dv, wid = .args$wid, between = .args$between,
      within = .args$within, covariate = .args$covariate
      )
    if(is_independent_anova(.args)){
      res.anova <- Anova(
        design$model, type = .args$type,
        white.adjust = .args$white.adjust, ...
      )
    }
    else{
      res.anova <- Anova(
        design$model, idata = design$idata,
        idesign = design$idesign, type = .args$type, ...
      )
    }
   .args$model <- design$model
  }
  attr(res.anova, "args") <- .args
  res.anova
}

# R stats aov
stats_aov <- function(.args){
  if(has_model(.args)){
    .model <- get_anova_model(.args)
    res.anova <- stats::aov(.model)
  }
  else{
    aov.formula <- create_aov_formula(.args)
    data <- .args$data
    res.anova <- .model <- stats::aov(aov.formula, data)
  }
  .args$model <- .model
  attr(res.anova, "args") <- .args
  res.anova
}

create_aov_formula <- function(.args){
  between <- paste(.args$between, collapse = "*")
  within <-  paste(.args$within, collapse = "*")
  covariate <- paste(.args$covariate, collapse = "+")
  error <- ifelse(
    within != "",
    error <- paste0("+Error(", .args$wid, "/(", within, "))"),
    ""
  )
  bw.sep <- ifelse(between != "" & within != "", "*", "")  # Between and Within vars separator
  bc.sep <- ifelse(covariate != "", "+", "") # Between and covariate vars separator
  .formula <- paste0(.args$dv, " ~ ", covariate, bc.sep, between, bw.sep, within, error) %>%
    stats::as.formula()
  .formula
}



# Not used helpers
check_anova_assumptions <- function(data, dv, between){
  . <- NULL
  outliers <- data %>%
    group_by(!!!syms(between)) %>%
    identify_outliers(!!dv)
  groups.normality <- data %>%
    group_by(!!!syms(between)) %>%
    shapiro_test(vars = dv)

  formula <- paste(between, collapse = "*") %>%
    paste(dv, ., sep = " ~ ") %>%
    stats::as.formula()
  model <- stats::lm(formula, data)
  .residuals <- stats::residuals(model)

  variance.homogeneity <- levene_test(data, formula)

  arguments <- list( dv = dv, between = between)

  results <- list(
    outliers = outliers,
    residuals.normality = shapiro_test(.residuals),
    groups.normality = groups.normality,
    variance.homogeneity = variance.homogeneity
  ) %>%
    set_attrs(arguments = arguments)

  results
}


check_repeated_anova_assumptions <- function(data, dv, wid, within){
  . <- NULL
  results <- check_anova_assumptions(data, dv, within)
  results$variance.homogeneity  <- NULL
  arguments <- list( dv = dv, wid = wid, within = within)
  results <- results %>% set_attrs(arguments = arguments)
    within <- paste(within, collapse = ", ") %>%
      paste0("c(", ., ")")
    data.name <- deparse(substitute(data))
    anova.formula <- paste0(
      "anova_test(", data.name, ", dv = ", dv,
      ", wid = ", wid, ", within = ", within, ")"
    )
    res.anova <- eval(parse(text = anova.formula))
    results <- results %>%
      .add_item(sphericity = res.anova$`Mauchly's Test for Sphericity`)
  results
}


check_mixed_anova_assumptions <- function(data, dv, wid , between, within){
  . <- NULL

  arguments <- list( dv = dv, wid = wid, between = between, within = within)


  grouping <- c(between, within)
  outliers <- data %>%
    group_by(!!!syms(grouping)) %>%
    identify_outliers(!!dv)
  groups.normality <- data %>%
    group_by(!!!syms(grouping)) %>%
    shapiro_test(vars = dv)

  formula <- paste(between, collapse = "*") %>%
    paste(dv, ., sep = " ~ ") %>%
    stats::as.formula()
  variance.homogeneity <- data %>%
    group_by(!!!syms(within)) %>%
    levene_test(formula)

  results <- list(
    outliers = outliers,
    groups.normality = groups.normality,
    variance.homogeneity = variance.homogeneity
  ) %>%
    set_attrs(arguments = arguments)

    within <- paste(within, collapse = ", ") %>%
      paste0("c(", ., ")")
    between <- paste(between, collapse = ", ") %>%
      paste0("c(", ., ")")
    data.name <- deparse(substitute(data))
    anova.formula <- paste0(
      "anova_test(", data.name, ", dv = ", dv,
      ", wid = ", wid, ", within = ", within, ", between = ", between, ")"
    )
    res.anova <- eval(parse(text = anova.formula))
    results <- results %>%
      .add_item(sphericity = res.anova$`Mauchly's Test for Sphericity`)

  results

}

