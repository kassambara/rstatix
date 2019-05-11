#' @include utilities.R factorial_design.R
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
#'@param data a data.frame or a model to be analyzed.
#'@param formula a formula specifying the ANOVA model similar to
#'  \link[stats]{aov}. Can be of the form \code{y ~ group} where \code{y} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'@param dv dependent variable name. Should be a numeric column.
#'@param wid column name containing individuals/subjects identifier. Should be
#'  unique per individual.
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
#'@param ... other arguments to be passed to the function
#'  \code{\link[car]{Anova}()}.
#'@param x an object of class \code{Anova_test}
#'
#'@return return an object of class \code{anova_test} a data frame containing
#'  the ANOVA table for independent measures ANOVA. However, for repeated/mixed
#'  measures ANOVA, it is a list containing the following components are
#'  returned:
#'
#'  \itemize{ \item \strong{ANOVA}: a data frame containing ANOVA results \item
#'  \strong{Mauchly's Test for Sphericity}: If any within-Ss variables with more
#'  than 2 levels are present, a data frame containing the results of Mauchly's
#'  test for Sphericity. Only reported for effects that have more than 2 levels
#'  because sphericity necessarily holds for effects with only 2 levels. \item
#'  \strong{Sphericity Corrections}: If any within-Ss variables are present, a
#'  data frame containing the Greenhouse-Geisser and Huynh-Feldt epsilon values,
#'  and corresponding corrected p-values. }
#'
#'  The returned object has an attribute called \code{args}, which is a list
#'  holding the arguments used to fit the ANOVA model, including: data, dv,
#'  within, between, type, model, etc.
#'
#'
#'  The following abbreviations are used in the different results tables:
#'
#'  \itemize{ \item DFn	Degrees of Freedom in the numerator (i.e. DF effect).
#'  \item DFd	Degrees of Freedom in the denominator (i.e., DF error). \item
#'  SSn	Sum of Squares in the numerator (i.e., SS effect). \item SSd	Sum of
#'  Squares in the denominator (i.e.,SS error). \item F	F-value. \item p	p-value
#'  (probability of the data given the null hypothesis). \item p<.05	Highlights
#'  p-values less than the traditional alpha level of .05. \item ges	Generalized
#'  Eta-Squared measure of effect size. \item GGe	Greenhouse-Geisser epsilon.
#'  \item p[GGe]	p-value after correction using Greenhouse-Geisser epsilon.
#'  \item p[GGe]<.05	Highlights p-values (after correction using
#'  Greenhouse-Geisser epsilon) less than the traditional alpha level of .05.
#'  \item HFe	Huynh-Feldt epsilon. \item p[HFe]	p-value after correction using
#'  Huynh-Feldt epsilon. \item p[HFe]<.05	Highlights p-values (after correction
#'  using Huynh-Feldt epsilon) less than the traditional alpha level of .05.
#'  \item W	Mauchly's W statistic }
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
#' # df %>% anova_test(len ~ supp*dose + Error(id/(supp*dose)))
#' # or use character vector
#' df %>% anova_test(dv = len, wid = id, within = c(supp, dose))
#'
#' # Use model as arguments
#' #:::::::::::::::::::::::::::::::::::::::::
#' .my.model <- lm(yield ~ block + N*P*K, npk)
#' anova_test(.my.model)
#'
#'@name anova_test
#'@export
anova_test <- function(data, formula, dv, wid, between, within, covariate, type = NULL,
                       effect.size = "ges", error = NULL,
                       white.adjust = FALSE, observed = NULL, detailed = FALSE, ...){
  . <- NULL
  .args <- rlang::enquos(
    dv = dv, wid = wid, between = between,
    within = within, covariate = covariate) %>%
    map(~get_quo_vars(data, .))
  if(!missing(formula)) .args$formula <- formula
  if(is_grouped_df(data)){
    results <- data %>% doo(
      anova_test, data = ., formula = .args$formula,
      dv = .args$dv, wid = .args$wid, between = .args$between,
      within = .args$within, covariate = .args$covariate,
      type = type, effect.size = effect.size, error = error,
      white.adjust = white.adjust, observed = observed, detailed = detailed, ...
    )
    return(results)
  }

  .args <- .args %>%
    .add_item(data = data, type = type, white.adjust = white.adjust) %>%
    check_anova_arguments()
  if(.args$type != 1) {
    if(is.null(error)) res.anova <- car_anova(.args, ...)
    else res.anova <- car_anova(.args, error = error, ...)
  }
  else if(.args$type == 1) res.anova <- stats_aov(.args)
  else stop("Something is wrong...")
  res.anova <- res.anova %>%
    summarize_anova(
      effect.size = effect.size, detailed = detailed,
      observed = observed
      )
  .args <- attr(res.anova, "args")
  if(length(res.anova) == 1)
    res.anova <- res.anova[[1]]
  attr(res.anova, "args") <- .args
  class(res.anova) <- c("anova_test", class(res.anova))
  res.anova
}

#' @rdname anova_test
#' @method print anova_test
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
  graphics::plot(.args$model, sub = "", ...)
}


is_model <- function(object){
  models <- c("lm", "aov", "glm", "multinom", "polr", "mlm", "manova")
  inherits(object,  models)
}

# Summarize ANOVA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# effect.size = c("ges", "pes")
summarize_anova <- function(res.anova, effect.size = "ges",
                            detailed = FALSE, observed = NULL){
  if(inherits(res.anova, "Anova.mlm")){
    results <- repeated_anova_summary(res.anova)
  }
  else if(inherits(res.anova, "anova")){
    results <- summary_independent_anova(res.anova)
  }
  else if(inherits(res.anova, c("aov", "aovlist"))){
    results <- summary_aov(res.anova)
  }
  else{
    stop("Non-supported object passed: ",
         paste(class(res.anova), collapse = ", "), ". ",
         "Object needs to be of class 'Anova.mlm' or 'anova'.")
  }
  .args <- attr(res.anova, "args")
  results <- results %>%
    add_anova_effect_size(effect.size, observed)

  if(!detailed){
    results <- remove_details(results)
  }
  results$ANOVA <- order_by_interaction_levels(results$ANOVA)
  results %>%
    map(~dplyr::mutate_if(., is.numeric, roundif, 3)) %>%
    rlang::set_attrs(args = .args)
}


# Summary of Anova.mlm object: summary_anova_mlm
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# this function is used for repeated and mixed anova
repeated_anova_summary <- function(res.anova, detailed = FALSE){
  .summary <- suppressWarnings(summary(res.anova))
  # Anova table converted into data frame
  aov.table <- .summary$univariate.tests %>%
    convert_anova_object_as_data_frame() %>%
    set_colnames(c("Effect", "SSn", "DFn", "SSd", "DFd", "F", "p")) %>%
    select(
      .data$Effect, .data$DFn, .data$DFd,
      .data$SSn, .data$SSd, .data$F, .data$p
      ) %>%
    mutate(`p<.05` = ifelse(.data$p < 0.05, "*",''))
  sphericity.test <- corrections <- NULL
  # Mauchly's Test for Sphericity
  if(nrow(.summary$sphericity.tests) > 0){
    sphericity.test <- .summary$sphericity.tests %>%
      convert_anova_object_as_data_frame() %>%
      set_colnames(c("Effect", "W", "p")) %>%
      mutate(`p<.05` = ifelse(.data$p < 0.05, "*",''))
  }
  # Sphericity corrections
  if(nrow(.summary$sphericity.tests) > 0){
    corrections <- .summary$pval.adjustments %>%
      as.data.frame() %>%
      set_colnames(c("GGe", "p[GG]", "HFe", "p[HF]")) %>%
      tibble::rownames_to_column("Effect")
    p.gg.signif <- ifelse(corrections[["p[GG]"]] < 0.05, "*",'')
    p.hf.signif <- ifelse(corrections[["p[HF]"]] < 0.05, "*",'')
    corrections <- corrections %>%
      add_column(`p[GG]<.05` = p.gg.signif, .after = "p[GG]") %>%
      add_column(`p[HF]<.05` = p.hf.signif, .after = "p[HF]")
  }
  # Results
  results <- list(ANOVA = aov.table)
  if(!is.null(sphericity.test)){
    results $`Mauchly's Test for Sphericity` <- sphericity.test
    results$`Sphericity Corrections` <-  corrections
    results <- results %>% add_corrected_df()
  }
  results
}
convert_anova_object_as_data_frame <- function(aov.table){
  aov.table.list <- list(Effect = rownames(aov.table))
  for(col in colnames(aov.table)){
    aov.table.list[[col]] <- aov.table[, col]
  }
  aov.table <- as.data.frame(aov.table.list, stringsAsFactors = FALSE)
  rownames(aov.table) <- 1:nrow(aov.table)
  aov.table
}

add_corrected_df <- function(.summary){
  aov.table <- .summary$ANOVA %>%
    select(.data$Effect, .data$DFn, .data$DFd)
  corrections <- .summary$`Sphericity Corrections` %>%
    dplyr::left_join(aov.table, by = "Effect") %>%
    mutate(
      df.gg = paste(roundif(.data$GGe*.data$DFn, 2), roundif(.data$GGe*.data$DFd, 2), sep = ", "),
      df.hf = paste(roundif(.data$HFe*.data$DFn, 2), roundif(.data$HFe*.data$DFd, 2), sep = ", ")
      ) %>%
    select(-.data$DFd, -.data$DFn)
  df.gg <- corrections$df.gg
  df.hf <- corrections$df.hf
  .summary$`Sphericity Corrections` <- corrections %>%
    select(-.data$df.gg, -.data$df.hf) %>%
    add_column(`DF[GG]` = df.gg, .after = "GGe") %>%
    add_column(`DF[HF]` = df.hf, .after = "HFe")
  .summary
}

# Summary of independent anova
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary_independent_anova <- function(res.anova){
  res.anova <- as.data.frame(res.anova)
  .residuals <- res.anova["Residuals", 1:2]
  if('Sum Sq' %in% colnames(res.anova)){
    colnames(res.anova) <- c('SSn','DFn','F','p')
    ss.exists <- TRUE
  }
  else{
    # case of white.adjust = TRUE. SS doesnt exist in the results
    colnames(res.anova) <- c('DFn','F','p')
    ss.exists <- FALSE
  }
  res.anova <- res.anova %>%
    tibble::rownames_to_column("Effect")  %>%
    add_column(DFd = .residuals$Df, .after = "DFn") %>%
    mutate(`p<.05` = ifelse(.data$p < 0.05, "*",'')) %>%
   filter(.data$Effect != "Residuals")
  if(ss.exists){
    res.anova <- res.anova %>%
      add_column(SSd = .residuals$`Sum Sq`, .after = "SSn")
  }
  results <- list(ANOVA = res.anova)
  results
}

# Summary of anova from stats::aov
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary_aov <- function(res.anova){
  remove_empty_space <- function(x){
    sapply(x, function(x){strsplit(x, " ")[[1]][1]})
  }
  reformat_aov_summary <- function(aov.summary){
    if(inherits(aov.summary, "listof"))
      aov.summary <- as.data.frame(aov.summary[[1]])
    else as.data.frame(aov.summary)
    .residuals <- aov.summary["Residuals", 1:2]
    aov.summary <- aov.summary %>%
      set_colnames(c("DFn", "SSn", "MS", "F", "p")) %>%
      tibble::rownames_to_column("Effect")  %>%
      add_column(DFd = .residuals$Df, .after = "DFn") %>%
      add_column(SSd = .residuals$`Sum Sq`, .after = "SSn") %>%
      mutate(`p<.05` = ifelse(.data$p < 0.05, "*",'')) %>%
      mutate(Effect = remove_empty_space(.data$Effect)) %>%
      filter(!is.na(.data$p)) %>%
      select(-.data$MS)
    aov.summary
  }
  res.anova <- summary(res.anova) %>%
    map(reformat_aov_summary) %>%
    dplyr::bind_rows() %>%
    order_by_interaction_levels()
  results <- list(ANOVA = res.anova)
  results
}


# Reorder ANOVA table by interaction levels in the term
order_by_interaction_levels <- function(aov.table){
  .terms <- aov.table$Effect
  nb.interaction <- str_count(.terms, ":")
  aov.table %>% dplyr::arrange(nb.interaction)
}






# Remove details from ANOVA summary: such as intercept row, Sum Sq columns
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
remove_details <- function(res.anova.summary){
  aov.table <- res.anova.summary$ANOVA
  aov.table = aov.table[, names(aov.table) %in% c('Effect','DFn','DFd','F','p','p<.05', 'ges', 'pes')]
  intercept.row <- grepl("Intercept", aov.table$Effect)
  res.anova.summary$ANOVA<- aov.table[!intercept.row, ]
  res.anova.summary
}

# Add effect size
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
add_anova_effect_size <- function(res.anova.summary, effect.size = "ges",  observed = NULL){
  ss.exists <- "SSn" %in% colnames(res.anova.summary$ANOVA)
  if(!ss.exists){
    return(res.anova.summary)
  }
  if("pes" %in% effect.size){
    res.anova.summary <- res.anova.summary %>%
      add_partial_eta_squared()
  }
  else {
    res.anova.summary <- res.anova.summary %>%
      add_generalized_eta_squared(observed)
  }
  res.anova.summary
}

# Generalized eta squared
add_generalized_eta_squared <- function(res.anova.summary, observed = NULL){
  aov.table <- res.anova.summary$ANOVA
  if(!is.null(observed)){
    obs <- rep(FALSE, nrow(aov.table))
    for(i in observed){
      if (!any(grepl(paste0("\\b",i,"\\b"), aov.table$Effect)))
        stop("Specified observed variable not found in data: ", i)
      obs <- obs | grepl(paste0("\\b",i,"\\b"), aov.table$Effect)
    }
    obs.SSn1 = sum(aov.table$SSn*obs)
    obs.SSn2 = aov.table$SSn*obs
  }
  else{
    obs.SSn1 <- 0
    obs.SSn2 <- 0
  }
  aov.table <- aov.table %>%
    mutate(ges = .data$SSn / (.data$SSn + sum(unique(.data$SSd)) + obs.SSn1 - obs.SSn2))
  res.anova.summary$ANOVA <- aov.table
  res.anova.summary
}
# Partial eta squared
add_partial_eta_squared <- function(res.anova.summary){
  res.anova.summary$ANOVA <- res.anova.summary$ANOVA %>%
    mutate(pes = .data$SSn/(.data$SSn + .data$SSd))
  res.anova.summary
}



# Get the value of enquo variables
get_quo_vars <- function (data, vars)
{
  if(rlang::quo_is_missing(vars)){
    return(NULL)
  }
  names(data) %>%
    tidyselect::vars_select(!!vars) %>%
    magrittr::set_names(NULL)
}

# Check if all columns in a data frame are numeric
is_all_columns_numeric <- function(data){
  data %>%
    map(is.numeric) %>%
    unlist() %>%
    all()
}

# Check the arguments of ANOVA
# .args is a list
check_anova_arguments <- function(.args){

  if(.is_empty(.args$between)) .args$between <- NULL
  if(.is_empty(.args$within)) .args$within <- NULL
  if(.is_empty(.args$wid)) .args$wid <- NULL
  if(.is_empty(.args$covariate)) .args$covariate <- NULL

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
  else if(!inherits(.args$data, "data.frame")){
    stop('data should be a data frame.')
  }
  if(is.null(.args$within) & is.null(.args$between)){
    stop("Specify at least one of the arguments: 'within' and 'between'")
  }
  .args$data <- droplevels(.args$data) %>%
    as_tibble()
  .args <- .args %>%
    remove_missing_values_in_data() %>%
    assertthat_dv_is_numeric() %>%
    assertthat_wid_is_specified() %>%
    asserthat_wid_is_unique() %>%
    convert_grouping_vars_to_factor() %>%
    assertthat_iv_has_enough_levels() %>%
    check_anova_type() %>%
    stop_if_repeated_ancova()
  .args
}

get_anova_vars_from_formula <- function(.args){
  formula <- .args$formula
  data <- .args$data
  vars <- all.vars(formula)
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

# Fit lm model for car::Anova
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fit_wide_lm <- function(.args){
  data <- .args$data
  dv <- .args$dv
  wid <- .args$wid
  between <- .args$between
  within <- .args$within
  . <- NULL

  res <-  list()
  if(!is.null(within)){
    nested <- data %>%
      group_by(!!!syms(within)) %>%
      nest()
    # Get intra-subject factor levels
    res$idata <- nested %>%
      select(-data) %>%
      dplyr::arrange(!!!syms(within)) %>%
      as.data.frame()
    res$idesign <- paste(within, collapse = "*") %>%
      paste0('~',.) %>%
      stats::as.formula()
    # Collapse intra-subject factors into one grouping column,
    # then spread the data into wide format
    wide <- nested %>%
      tidyr::unite(!!!syms(within), col = ".group.", sep = "_") %>%
      select(.data$.group., data) %>%
      unnest() %>%
      select(!!!syms(c(wid, ".group.", between, dv))) %>%
      spread(key = ".group.", value = dv) %>%
      as.data.frame()
    # dv are all possible combinations of within-subjects factor levels
    wide.dv.name <- setdiff(colnames(wide), c(wid, between))
    wide.dv <- wide %>%
      select(!!!syms(wide.dv.name)) %>%
      as.matrix()
  }
  else{
    wide <- data
  }
  # Case of repeated measures anova
  if(is.null(between)){
    lm_formula <- wide.dv ~ 1
  }
  # Case of independent measures anova
  else if(is.null(within)){
    covariate <- paste(.args$covariate, collapse = "+")
    between <- paste(between, collapse = "*")
    bc.sep <- ifelse(covariate != "", "+", "") # Between and covariate vars separator
    lm_formula <- paste0(dv, " ~ ", covariate, bc.sep, between)
  }
  # Case of mixed ANOVA
  else{
    lm_formula <- paste(between, collapse = "*") %>%
      paste("wide.dv ~ " ,., sep='')
  }
  lm_formula <- stats::as.formula(lm_formula)
  opt <- options( "contrasts" = c( "contr.sum", "contr.poly" ) )
  res$lm <- stats::lm(lm_formula, wide)
  options(opt)
  res
}

# Fit lm from formula and data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fit_lm <- function(.args){
  .args <- remove_missing_values_in_data(.args)
  data <- droplevels(.args$data)
  lm_formula <- .args$formula
  stats::lm(lm_formula, data)
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
    model <- fit_wide_lm(.args)
    if(is_independent_anova(.args)){
      res.anova <- Anova(
        model$lm, type = .args$type,
        white.adjust = .args$white.adjust, ...
      )
    }
    else{
      res.anova <- Anova(
        model$lm, idata = model$idata,
        idesign = model$idesign, type = .args$type, ...
      )
    }
   .args$model <- model$lm
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
  if(base::requireNamespace("ez", quietly = TRUE)){
    within <- paste(within, collapse = ", ") %>%
      paste0(".(", ., ")")
    data.name <- deparse(substitute(data))
    anova.formula <- paste0(
      "ez::ezANOVA(", data.name, ", dv = ", dv,
      ", wid = ", wid, ", within = ", within, ")"
    )
    res.anova <- eval(parse(text = anova.formula))
    results <- results %>%
      .add_item(sphericity = res.anova$`Mauchly's Test for Sphericity`)
  }
  else{
    warning(
      "Install the ez R package for ",
      "checking the assumption of sphericity",
      call. = FALSE
    )
  }
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

  if(base::requireNamespace("ez", quietly = TRUE)){
    within <- paste(within, collapse = ", ") %>%
      paste0(".(", ., ")")
    between <- paste(between, collapse = ", ") %>%
      paste0(".(", ., ")")
    data.name <- deparse(substitute(data))
    anova.formula <- paste0(
      "ez::ezANOVA(", data.name, ", dv = ", dv,
      ", wid = ", wid, ", within = ", within, ", between = ", between, ")"
    )
    res.anova <- eval(parse(text = anova.formula))
    results <- results %>%
      .add_item(sphericity = res.anova$`Mauchly's Test for Sphericity`)
  }
  else{
    warning(
      "Install the ez R package for ",
      "checking the assumption of sphericity",
      call. = FALSE
    )
  }

  results

}

