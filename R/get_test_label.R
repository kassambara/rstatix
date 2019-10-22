#' @include utilities.R
NULL

#' Extract Label Information from Statistical Tests
#' @description Extracts label information from statistical tests. Useful for
#'   labelling plots with test outputs.
#' @param stat.test statistical test results returned by \code{rstatix}
#'   functions.
#' @param description the test description used as the prefix of the label.
#'   Examples of description are "ANOVA", "Two Way ANOVA". To remove the default
#'   description, specify \code{description = NULL}. If missing, we'll try to
#'   guess the statistical test default description.
#' @param p.col character specifying the column containing the p-value. Default
#'   is \code{"p"}, can be \code{"p.adj"}.
#' @param type the label type. Can be one of "text" and "expression". Partial
#'   match allowed. If you want to add the label onto a ggplot, it might be
#'   useful to specify \code{type = "expresion"}.
#' @param correction character, considered only in the case of ANOVA test. Which sphericity
#'   correction of the degrees of freedom should be reported for the
#'   within-subject factors (repeated measures). The default is set to
#'   \code{"GG"} corresponding to the Greenhouse-Geisser correction. Possible
#'   values are \code{"GG"}, \code{"HF"} (i.e., Hyunh-Feldt correction),
#'   \code{"none"} (i.e., no correction) and \code{"auto"} (apply automatically
#'   GG correction if the sphericity assumption is not for within-subject
#'   design.
#' @param row numeric, the row index to be considered. If NULL, the last row is
#'   automatically considered for ANOVA test.
#' @param statistic.text character specifying the test statistic. For example
#'   \code{statistic.text = "F"} (for ANOVA test ); \code{statistic.text = "t"}
#'   (for t-test ).
#' @param statistic the numeric value of a statistic.
#' @param p the p-value of the test.
#' @param parameter string containing the degree of freedom (if exists). Default
#'   is \code{NA} to accommodate non-parametric tests. For example
#'   \code{parameter = "1,9"} (for ANOVA test. Two parameters exist: DFn and
#'   DFd); \code{sparameter = "9"} (for t-test ).
#' @param n sample count, example: \code{n = 10}.
#' @param effect.size the effect size value
#' @param effect.size.text a character specifying the relevant effect size. For
#'   example, for \code{Cohens d} statistic, \code{effect.size.text = "d"}. You
#'   can also use plotmath expression as follow \code{quote(italic("d"))}.
#' @param detailed logical value. If TRUE, returns detailed label.
#' @return a text label or an expression to pass to a plotting function.
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-way ANOVA test
#' #:::::::::::::::::::::::::::::::::::::::::
#' anov <- df %>% anova_test(len ~ dose)
#' get_test_label(anov, detailed = TRUE, type = "text")
#'
#' # Two-way ANOVA test
#' #:::::::::::::::::::::::::::::::::::::::::
#' anov <- df %>% anova_test(len ~ supp*dose)
#' get_test_label(anov, detailed = TRUE, type = "text",
#'    description = "Two Way ANOVA")
#'
#'
#' # Kruskal-Wallis test
#' #:::::::::::::::::::::::::::::::::::::::::
#' kruskal<- df %>% kruskal_test(len ~ dose)
#' get_test_label(kruskal, detailed = TRUE, type = "text")
#'
#' # Wilcoxon test
#' #:::::::::::::::::::::::::::::::::::::::::
#' # Unpaired test
#' wilcox <- df %>% wilcox_test(len ~ supp)
#' get_test_label(wilcox, detailed = TRUE, type = "text")
#'# Paired test
#' wilcox <- df %>% wilcox_test(len ~ supp, paired = TRUE)
#' get_test_label(wilcox, detailed = TRUE, type = "text")
#'
#' # T test
#' #:::::::::::::::::::::::::::::::::::::::::
#' ttest <- df %>% t_test(len ~ dose)
#' get_test_label(ttest, detailed = TRUE, type = "text")
#'
#'
#' # Pairwise comparisons labels
#' #:::::::::::::::::::::::::::::::::::::::::
#' get_pwc_label(ttest, type = "text")
#'
#'
#' # Create test labels
#' #:::::::::::::::::::::::::::::::::::::::::
#' create_test_label(
#'   statistic.text = "F", statistic = 71.82,
#'   parameter = "4, 294",
#'   p = "<0.0001",
#'   description = "ANOVA",
#'   type = "text"
#' )
#'
#' @describeIn get_test_label Extract label from pairwise comparisons.
#' @export
get_pwc_label <- function(stat.test, type = c("expression", "text")){
  methods <- get_pairwise_comparison_methods()
  stat.test %>% stop_ifnot_class(names(methods))
  type <- match.arg(type)
  args <- attr(stat.test, "args")
  stat.method <- methods[args$method]
  p.adjust.method <- args$p.adjust.method  %>%
    to_uppercase_first_letter()
  if(! "p.adj" %in% colnames(stat.test)){
    p.adjust.method <- "None"
  }
  if(type == "text"){
    paste0("pwc: ", stat.method, "; p.adjust: ", p.adjust.method)
  }
  else if(type == "expression"){
    substitute(
      expr = paste(
        "pwc: ", bold(stat.method),
          "; p.adjust: ", bold(p.adjust.method)
        ),
      env = list(stat.method = stat.method, p.adjust.method = p.adjust.method)
    )
  }
}

#' @describeIn get_test_label Extract labels for statistical tests.
#' @export
get_test_label <- function(stat.test, description = NULL, p.col = "p",
                           type = c("expression", "text"),
                           correction = c("auto", "GG", "HF", "none"), row = NULL, detailed = FALSE){
  type = match.arg(type)
  allowed.tests <- c(
    get_pairwise_comparison_methods(),
    kruskal_test = "Kruskal-Wallis",
    friedman_test = "Friedman test",
    anova_test = "Anova",
    welch_anova_test = "Welch ANOVA",
    chisq_test = "Chi-square test",
    exact_multinom_test = "Exact multinomial test",
    exact_binom_test = "Exact binomial test",
    cochran_qtest = "Cochran Q test",
    chisq_trend_test = "Chi-square trend test"
  )
  stop_ifnot_class(stat.test, .class = names(allowed.tests))
  is_anova_test <- inherits(stat.test, "anova_test")
  if(is_anova_test){
    stat.test <- get_anova_table(stat.test, correction = correction)
    if(is.null(row)) row <-  nrow(stat.test) # consider the last row
  }
  if(!is.null(row)) stat.test %<>% dplyr::slice(row)

  statistic.text <- get_statistic_text(stat.test, type = type)
  statistic <- get_statistic(stat.test)
  df <- get_df(stat.test)
  n <- get_n(stat.test)
  effect <- get_effect_size(stat.test, type)
  effect.size <- effect$value
  effect.size.text <- effect$text

  if(missing(description)){
    description <- get_description(stat.test)
  }
  if(!is.null(description)){
    if(description != ""){
      description <- paste0(description, ", ")
    }
  }
  if(!(p.col %in% colnames(stat.test))){
    # automatic detection of p.col
    p.col <- p_detect(stat.test)
  }
  stat.test %<>%
    select(!!sym(p.col)) %>%
    rename(p = p.col) %>%
    mutate(
      row.id = 1:nrow(stat.test), n = n,
      statistic = statistic, parameter = df,
      effect.size = effect.size
    )
  if(is.numeric(stat.test$p)){
    stat.test$p <- p_format(stat.test$p, 3)
  }

  get_label_func <- switch (
    type,
    expression = create_test_label.expression,
    text = create_test_label.text
  )
  get_label_func_df <- function(df){
    get_label_func(
      description, statistic.text = statistic.text,
      statistic = df$statistic, parameter = df$parameter,
      p = df$p, n = df$n,  effect.size = df$effect.size,
      effect.size.text = effect.size.text, detailed = detailed
    )
  }
  if(nrow(stat.test) > 1){
    results <- stat.test %>%
      group_by(.data$row.id) %>%
      doo(get_label_func_df) %>%
      pull(.data$.results.)
  }
  else{
    results <- get_label_func_df(stat.test)
  }
  results
}

#' @describeIn get_test_label Create labels from user specified test results.
#' @export
create_test_label <- function(
  statistic.text, statistic, p, parameter = NA, description = NULL, n = NA, effect.size = NA, effect.size.text = NA,
  type = c("expression", "text"), detailed = FALSE)
{
  type <- match.arg(type)
  if(!is.null(description)){
    if(description != ""){
      description <- paste0(description, ", ")
    }
  }
  else description <- ""
  label_func <- switch(
    type,
    text = create_test_label.text,
    expression = create_test_label.expression,
    create_test_label.text
  )
  label_func(
    description = description, statistic.text = statistic.text,
    statistic = statistic, parameter = parameter,
    p = p, n = n,  effect.size = effect.size,
    effect.size.text = effect.size.text, detailed = detailed
  )
}

# Build test labeles
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# description: stat test description, e.g "T test"
# statistic.text: statistic text, example: "t",
# statistic: statistic value, example: 10
# parameter: string containing the degree of freedom,
#   ex: "9"  for t-test or "1,9" for ANOVA (DFn = 1 and DFd = 9)
# p: p value
# n: sample count
create_test_label.expression <- function(
  description, statistic.text, statistic, parameter, p,  n = NA,
  effect.size = NA, effect.size.text = NA, detailed = FALSE)
  {

  if(is.na(parameter)) parameter <- ""
  else parameter <- paste0("(", parameter, ")")
  # Sample count
  if(is.na(n)) {
    n <- ""
  }
  else{
    n <- substitute(
      expr = paste(", ", italic("n"), " = ", n),
      env = list(n = n)
    )
  }
  # Effect size
  if(is.na(effect.size)){
    effect.size <- ""
  }
  else{
    effect.size <- round_value(effect.size, 2)
    effect.size <- substitute(
      expr = paste(", ", effect.size.text, " = ", effect.size),
      env = list(effect.size.text = effect.size.text, effect.size = effect.size)
    )
  }
  # Create label
  statistic <- round_value(statistic, 2)
  equal <- " = "
  if(is.na(statistic))
    statistic.text <- equal <- statistic <- ""
  else
    statistic <- paste0(statistic, ", ")
  env <- as.list(environment())
  if(detailed){
    substitute(
      expr = paste(
        description, statistic.text, parameter, equal, statistic,
        italic("p"), " = ", p, effect.size, n
      ),
      env = env
    )
  }
  else{
    substitute(
      expr = paste(description, italic("p"), " = ", p),
      env = env
      )
  }
}

create_test_label.text <- function(description, statistic.text,
                                statistic, parameter, p,  n = NA,
                                effect.size = NA, effect.size.text = NA,  detailed = FALSE){
  if(is.na(parameter)) parameter <- ""
  else parameter <- paste0("(", parameter, ")")
  if(is.na(effect.size)) effect.size <- ""
  else effect.size <- paste0(", ", effect.size.text, " = ", effect.size)
  if(is.na(n)) n <- ""
  else n <- paste0(", ", "n", " = ", n)
  if(!is.na(statistic)){
    statistics <- paste0(statistic.text, parameter, " = ", round_value(statistic, 2), ", ")
  }
  else statistics <- ""
  if(detailed){
    paste0(
      description, statistics,
      "p", " = ", p, effect.size, n
    )
  }
  else{
    paste0(description, "p = ", p)
  }
}

# Get label parameters
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Statical test text: F, t, W, V, X2, -------------------------------------------
get_statistic_text <- function(stat.test, type = c("expression", "text")){
  type <- match.arg(type)
  args <- attr(stat.test, "args")
  stat.method <- args$method
  is.paired <- args$paired
  if(!is.null(is.paired)){
    if(is.paired & stat.method == "wilcox_test"){
      stat.method = "wilcox_test_paired"
    }
  }
  if(is.null(is.paired)) is.paired <- FALSE
  if(type == "expression"){
    statistic.text <- switch(
      stat.method,
      t_test = quote(italic("t")),
      wilcox_test = quote(italic("W")),
      wilcox_test_paired = quote(italic("V")),
      sign_test = quote(italic("S")),
      dunn_test = quote(italic("Z")),
      emmeans_test = quote(italic("t")),
      tukey_hsd = quote(italic("t")),
      games_howell_test = quote(italic("t")),
      kruskal_test = quote(italic(chi)^2),
      friedman_test = quote(italic(chi)^2),
      anova_test = quote(italic("F")),
      welch_anova_test = quote(italic("F")),
      chisq_test = quote(italic(chi)^2),
      mcnemar_test = quote(italic(chi)^2),
      prop_test = quote(italic(chi)^2),
      cochran_qtest = quote(italic(chi)^2),
      chisq_trend_test = quote(italic(chi)^2),
      quote(italic("Stat"))
    )
  }
  else{
    statistic.text <- switch(
      stat.method,
      t_test = "t",
      wilcox_test = "W",
      wilcox_test_paired = "V",
      sign_test = "S",
      dunn_test = "Z",
      emmeans_test = "t",
      tukey_hsd = "t",
      games_howell_test = "t",
      kruskal_test = "X2",
      friedman_test = "X2",
      anova_test = "F",
      welch_anova_test = "F",
      chisq_test = "X2",
      mcnemar_test = "X2",
      prop_test = "X2",
      cochran_qtest = "X2",
      chisq_trend_test = "X2",
      "Stat"
    )
  }
  statistic.text
}

# Statistic values -------------------------------------------------
get_statistic <- function(stat.test){
  stat.cols <- colnames(stat.test)
  if("statistic" %in% stat.cols){
    result <- stat.test$statistic
  }
  else if ("F" %in% stat.cols){
    result <- stat.test$F
  }
  else{
    # statistic column not found
    result <- rep(NA, nrow(stat.test))
  }
  result
}

# Degree of freedom-------------------------------------------------
get_df <- function(stat.test){
  args <- attr(stat.test, "args")
  df.cols <- c("df", "DFn", "DFd")
  if(!any(df.cols %in% colnames(stat.test))){
    return(NA)
  }
  if(all(c("DFn", "DFd") %in% colnames(stat.test))){
    dfn <- round_value(stat.test$DFn, 2)
    dfd <- round_value(stat.test$DFd, 2)
    df <- paste(dfn, dfd, sep = ",")
  }
  else{
    df <- round_value(stat.test$df, 2)
  }
  df
}

# Sample count-------------------------------------------------
get_n <- function(stat.test){
  if(inherits(stat.test, "anova_test")){
    .args <- attr(stat.test, "args")
    wid <- .args$wid
    if(is.null(wid)) n <- nrow(.args$data)
    else n <- .args$data %>% pull(!!wid) %>% unique() %>% length()
    stat.test %<>% mutate(n = !!n)
  }
  n.cols <- c("n", "n1", "n2")
  if(!any(n.cols %in% colnames(stat.test))){
    return(NA)
  }
  if("n" %in% colnames(stat.test)){
    stat.test$n
  }
  else if(all(c("n1", "n2") %in% colnames(stat.test))){
    if(is_paired(stat.test)) stat.test$n1
    else stat.test$n1 + stat.test$n2
  }
}

# Statistical test description ---------------------------------
get_description <- function(stat.test){
  tests <- c(
    t_test = "T test",
    wilcox_test = "Wilcoxon test",
    sign_test = "Sign test",
    dunn_test = "Dunn test",
    emmeans_test = "Emmeans test",
    tukey_hsd = "Tukey HSD",
    anova_test = "Anova",
    welch_anova_test = "Welch Anova",
    kruskal_test = "Kruskal-Wallis",
    friedman_test = "Friedman test",
    cor_test = "Correlation",
    prop_test = "Z-Prop test",
    fisher_test = "Fisher's exact test",
    chisq_test = "Chi-square test",
    exact_multinom_test = "Exact multinomial test",
    exact_binom_test = "Exact binomial test",
    mcnemar_test = "McNemar test",
    cochran_qtest = "Cochran Q test",
    chisq_trend_test = "Chi-square trend test"
  )
  args <- attr(stat.test, "args")
  stat.method <- args$method
  if(stat.method %in% names(tests)){
    description <- tests[stat.method]
  }
  else{
    description  <- stat.method
  }
  as.character(description)
}

# Efect size ---------------------------------
get_effect_size <- function(stat.test, type = "text"){
  stat.method <- attr(stat.test, "args")$method
  value <- text <- NA

  if("ges" %in% colnames(stat.test)) {
    value <- stat.test$ges
    if(type == "expression") text <- quote(eta["g"]^2)
    else text <- "eta2[g]"
  }
  else if("pes" %in% colnames(stat.test)) {
    if(type == "expression") text <- quote(eta["p"]^2)
    else text <- "eta2[p]"
    value <- stat.test$pes
  }
  else if("effsize" %in% colnames(stat.test)){
    value <- stat.test$effsize
    if(type == "expression"){
      text <- switch(
        stat.method,
        t_test = quote(italic("d")),
        wilcox_test = quote(italic("r")),
        kruskal_test = quote(eta["H"]^2),
        friedman_test = quote(italic("W")["Kendall"]),
        quote(italic("effsize"))
      )
    }
    else{
      text <- switch(
        stat.method,
        t_test = "d",
        wilcox_test = "r",
        kruskal_test = "eta2[H]",
        friedman_test = "W[Kendall]",
        "effsize"
      )
    }
  }
  list(value = value, text = text)
}

# Check if paired stat test--------------------------------------------
is_paired <- function(stat.test){
  args <- attr(stat.test, "args")
  is.paired <- args$paired
  if(is.null(is.paired)) is.paired <- FALSE
  is.paired
}

