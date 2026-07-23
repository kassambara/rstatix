#' @include utilities.R
NULL

#' Extract Label Information from Statistical Tests
#' @description Extracts label information from statistical tests. Useful for
#'   labelling plots with test outputs.
#'
#'  See the Datanovia tutorial
#'  \href{https://www.datanovia.com/learn/data-visualization/ggpubr/p-values-from-tests}{P-values from Tests on ggplots in R (rstatix)}
#'  for a worked walkthrough.
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
#' @param style the label style. Either \code{"classic"} (default, the historical
#'   format) or \code{"apa"} for an APA-7 in-text statistical report: the leading
#'   test-name label is dropped, the p-value is formatted APA-style
#'   (\code{p = .023}, \code{p < .001}), and the effect size is shown with its
#'   confidence interval when the object carries one (e.g. \code{anova_test(ci = )}),
#'   otherwise as a point estimate, otherwise omitted. Bounded effect sizes
#'   (\eqn{\eta^2}, \emph{r}, Cliff's \eqn{\delta}, rank-biserial) drop the
#'   leading zero; Cohen's \emph{d} keeps it.
#' @param effect.size.ci a length-two numeric \code{c(low, high)} giving the
#'   effect size's confidence interval, appended after the effect size when
#'   \code{style = "apa"}. Defaults to \code{NA} (no interval).
#' @param effect.size.bounded logical. Whether the effect size lies in
#'   \code{[-1, 1]} (e.g. \eqn{\eta^2}, \emph{r}, Cliff's \eqn{\delta}); used by
#'   \code{style = "apa"} to decide whether to drop the leading zero. Defaults to
#'   \code{TRUE}; set \code{FALSE} for Cohen's \emph{d}.
#' @param effect.size.ci.level the confidence level \code{effect.size.ci} was
#'   computed at, shown before the interval (e.g. \code{95\%} CI). Defaults to
#'   \code{0.95}.
#' @references American Psychological Association (2020). \emph{Publication Manual
#'   of the American Psychological Association} (7th ed.).
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
#'
#' # Extract infos
#' #:::::::::::::::::::::::::::::::::::::::::
#' stat.test <- df %>% t_test(len ~ dose)
#' get_n(stat.test)
#' get_description(stat.test)
#'
#'
#' @describeIn get_test_label Extract label from pairwise comparisons.
#' @seealso The Datanovia tutorial: \href{https://www.datanovia.com/learn/data-visualization/ggpubr/p-values-from-tests}{P-values from Tests on ggplots in R (rstatix)}.
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
                           correction = c("auto", "GG", "HF", "none"), row = NULL, detailed = FALSE,
                           style = c("classic", "apa")){
  type = match.arg(type)
  style = match.arg(style)
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
    chisq_trend_test = "Chi-square trend test",
    fligner_test = "Fligner-Killeen"
  )
  stop_ifnot_class(stat.test, .class = names(allowed.tests))
  is_anova_test <- inherits(stat.test, "anova_test")
  anova.n <- NA
  anova.ci.level <- NA
  anova.simple <- FALSE
  if(is_anova_test){
    # the confidence level anova_test(ci = ) was run with; needed below because
    # the slice strips the attributes that carry it
    ci.arg <- attr(stat.test, "args")$ci
    if(is.numeric(ci.arg) && length(ci.arg) == 1) anova.ci.level <- ci.arg
    # A plain one-table result (between-subjects, or a within design with no
    # sphericity machinery) displays the same df the stored interval was
    # computed at. A list result carries sphericity corrections that
    # get_anova_table() may apply to DFn/DFd, while conf.low/conf.high stay at
    # the uncorrected df -- so re-deriving the interval from the displayed df
    # would contradict the object's own interval (see below).
    anova.simple <- is.data.frame(stat.test)
    stat.test <- get_anova_table(stat.test, correction = correction)
    # derive the sample size now, before the slice below strips the anova_test
    # class/attributes that get_n() needs to compute n for ANOVA (#150). The total
    # sample size is the same for every effect row, so a single value is taken.
    anova.n <- get_n(stat.test)[1]
    if(is.null(row)) row <-  nrow(stat.test) # consider the last row
  }
  if(!is.null(row)) {
    stat.test <- stat.test %>%
      keep_only_tbl_df_classes() %>%
      dplyr::slice(row)
  }

  statistic.text <- get_statistic_text(stat.test, type = type)
  statistic <- get_statistic(stat.test)
  # APA-7 writes two degrees of freedom with a space: F(2, 57); the classic
  # style keeps its historical compact form F(2,57).
  df <- get_df(stat.test, sep = if(style == "apa") ", " else ",")
  n <- get_n(stat.test)
  if(is_anova_test) n <- anova.n   # use the n derived before the class was stripped (#150)
  effect <- get_effect_size(stat.test, type, style = style)
  effect.size <- effect$value
  effect.size.text <- effect$text
  effect.size.bounded <- isTRUE(effect$bounded)
  # Effect-size confidence interval for the APA style. Only taken when it is
  # unambiguously the SHOWN effect size's interval: for anova_test(ci = ), the
  # conf.low/conf.high computed by the noncentral-F inversion belong to PARTIAL
  # eta-squared (pes). If the label is showing generalized eta-squared (ges) --
  # e.g. anova_test(effect.size = c("ges", "pes"), ci = ) -- pairing that ges
  # estimate with the pes interval would be wrong, so the CI is attached only
  # when the displayed effect size IS pes. For other tests conf.low/conf.high is
  # the location-difference interval, not the effect size's, so it is never used.
  effect.size.ci <- NA
  effect.size.ci.level <- 0.95
  if(style == "apa" && is_anova_test && identical(effect$column, "pes") &&
     nrow(stat.test) == 1 && !is.na(anova.ci.level) &&
     all(c("conf.low", "conf.high") %in% colnames(stat.test))){
    effect.size.ci <- c(stat.test$conf.low[1], stat.test$conf.high[1])
    effect.size.ci.level <- anova.ci.level
    # The table stores pes/conf.low/conf.high rounded to 3 decimals
    # (anova_summary()), and rounding those again to APA's 2 decimals can move
    # the last digit (a true bound 0.7854 is stored as 0.785 and would print
    # ".78" instead of ".79"). Re-derive the estimate and the interval from the
    # row's F and df, which pins them well beyond 2 displayed decimals. Only
    # for a plain table (anova.simple): a sphericity-corrected row displays
    # corrected df while the stored interval is defined at the uncorrected df,
    # so there the stored bounds are shown as they are.
    if(anova.simple && all(c("F", "DFn", "DFd") %in% colnames(stat.test))){
      effect.size.ci <- partial_eta_squared_ci(
        stat.test$F[1], stat.test$DFn[1], stat.test$DFd[1],
        conf.level = anova.ci.level
      )
      effect.size <- stat.test$F[1] * stat.test$DFn[1] /
        (stat.test$F[1] * stat.test$DFn[1] + stat.test$DFd[1])
    }
  }

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
  stat.test <- stat.test %>%
    keep_only_tbl_df_classes() %>%
    select(p = all_of(p.col)) %>%
    mutate(
      row.id = 1:nrow(stat.test), n = n,
      statistic = statistic, parameter = df,
      effect.size = effect.size
    )
  # Classic keeps its pre-formatted p string; the APA style formats the raw
  # numeric p itself (three decimals, no leading zero, "p < .001").
  if(style == "classic" && is.numeric(stat.test$p)){
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
      effect.size.text = effect.size.text, detailed = detailed,
      style = style, effect.size.ci = effect.size.ci,
      effect.size.ci.level = effect.size.ci.level,
      effect.size.bounded = effect.size.bounded
    )
  }
  if(nrow(stat.test) > 1){
    results <- stat.test %>%
      group_by(.data$row.id) %>%
      doo(get_label_func_df) %>%
      pull(".results.")
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
  type = c("expression", "text"), detailed = FALSE,
  style = c("classic", "apa"), effect.size.ci = NA, effect.size.bounded = TRUE,
  effect.size.ci.level = 0.95)
{
  type <- match.arg(type)
  style <- match.arg(style)
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
    effect.size.text = effect.size.text, detailed = detailed,
    style = style, effect.size.ci = effect.size.ci,
    effect.size.bounded = effect.size.bounded,
    effect.size.ci.level = effect.size.ci.level
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
  effect.size = NA, effect.size.text = NA, detailed = FALSE,
  style = "classic", effect.size.ci = NA, effect.size.bounded = TRUE,
  effect.size.ci.level = 0.95)
  {
  if(style == "apa"){
    return(apa_test_label.expression(
      statistic.text = statistic.text, statistic = statistic, parameter = parameter,
      p = p, effect.size = effect.size, effect.size.text = effect.size.text,
      effect.size.ci = effect.size.ci, effect.size.bounded = effect.size.bounded,
      effect.size.ci.level = effect.size.ci.level
    ))
  }
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
                                effect.size = NA, effect.size.text = NA,  detailed = FALSE,
                                style = "classic", effect.size.ci = NA,
                                effect.size.bounded = TRUE,
                                effect.size.ci.level = 0.95){
  if(style == "apa"){
    return(apa_test_label.text(
      statistic.text = statistic.text, statistic = statistic, parameter = parameter,
      p = p, effect.size = effect.size, effect.size.text = effect.size.text,
      effect.size.ci = effect.size.ci, effect.size.bounded = effect.size.bounded,
      effect.size.ci.level = effect.size.ci.level
    ))
  }
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

# APA-7 in-text statistical report string (no leading label, italics unavailable
# in plain text): "t(58) = 2.31, p = .025, d = 0.61, 95% CI [0.08, 1.13]".
apa_test_label.text <- function(statistic.text, statistic, parameter, p,
                                effect.size, effect.size.text,
                                effect.size.ci, effect.size.bounded,
                                effect.size.ci.level = 0.95){
  param <- if(is.na(parameter)) "" else paste0("(", parameter, ")")
  stat.part <- if(is.na(statistic)) ""
               else paste0(statistic.text, param, " = ", apa_format_statistic(statistic), ", ")
  es.part <- ""
  if(!is.na(effect.size)){
    es.part <- paste0(", ", effect.size.text, " = ",
                      apa_format_effsize_value(effect.size, effect.size.bounded))
    if(length(effect.size.ci) == 2 && !anyNA(effect.size.ci)){
      es.part <- paste0(
        es.part, ", ", apa_ci_prefix(effect.size.ci.level), " [",
        apa_format_effsize_value(effect.size.ci[1], effect.size.bounded), ", ",
        apa_format_effsize_value(effect.size.ci[2], effect.size.bounded), "]"
      )
    }
  }
  paste0(stat.part, apa_format_p(p), es.part)
}

# APA-7 reports inferential statistics to two decimals, trailing zero kept
# ("F(2, 20) = 14.30", not "14.3"), matching the fixed-width p and effect-size
# formatters below.
apa_format_statistic <- function(statistic){
  formatC(round(unname(statistic), 2), format = "f", digits = 2)
}
# "95% CI" / "90% CI": the level the interval was actually computed at.
apa_ci_prefix <- function(level){
  paste0(formatC(level * 100, format = "fg"), "% CI")
}
# APA-7 p-value: "p < .001" or "p = .023" (three decimals, no leading zero).
apa_format_p <- function(p){
  paste0("p", apa_p_suffix(p))
}
# The part after "p": " < .001" or " = .023". Shared by the text and expression
# builders (the latter renders the leading italic "p" via plotmath).
apa_p_suffix <- function(p){
  if(length(p) != 1 || is.na(p)) return(" = NA")
  if(is.character(p)){
    # A pre-formatted p string ("<0.0001", "0.023") reaches here from
    # create_test_label(). Parse it: comparing the string itself against a
    # number would collate locale-dependently, and round() would error.
    p.chr <- gsub("\\s", "", p)
    is.less <- grepl("^<", p.chr)
    p.num <- suppressWarnings(as.numeric(sub("^[<=]", "", p.chr)))
    if(is.na(p.num)) return(paste0(" = ", p))
    if(is.less){
      # "p < x" asserts only an upper bound: keep the bound as given (APA
      # leading zero dropped), tightening to the standard "< .001" floor.
      if(p.num <= 0.001) return(" < .001")
      return(paste0(" < ", sub("^0\\.", ".", formatC(p.num, format = "fg"))))
    }
    p <- p.num
  }
  if(p < 0.001) return(" < .001")
  # APA never reports "p = 1.000" (nor ".000"): cap at "> .999".
  if(round(p, 3) >= 1) return(" > .999")
  paste0(" = ", sub("^(-?)0\\.", "\\1.", formatC(round(p, 3), format = "f", digits = 3)))
}
# APA-7 plotmath expression, mirroring apa_test_label.text() with italic symbols.
apa_test_label.expression <- function(statistic.text, statistic, parameter, p,
                                      effect.size, effect.size.text,
                                      effect.size.ci, effect.size.bounded,
                                      effect.size.ci.level = 0.95){
  param <- if(is.na(parameter)) "" else paste0("(", parameter, ")")
  if(is.na(statistic)){
    base <- substitute(paste(italic("p"), psuffix),
                       env = list(psuffix = apa_p_suffix(p)))
  }
  else{
    base <- substitute(
      paste(st, param, " = ", sv, ", ", italic("p"), psuffix),
      env = list(st = statistic.text, param = param,
                 sv = apa_format_statistic(statistic), psuffix = apa_p_suffix(p))
    )
  }
  if(is.na(effect.size)) return(base)
  es.val <- apa_format_effsize_value(effect.size, effect.size.bounded)
  ci.str <- ""
  if(length(effect.size.ci) == 2 && !anyNA(effect.size.ci)){
    ci.str <- paste0(
      ", ", apa_ci_prefix(effect.size.ci.level), " [",
      apa_format_effsize_value(effect.size.ci[1], effect.size.bounded), ", ",
      apa_format_effsize_value(effect.size.ci[2], effect.size.bounded), "]"
    )
  }
  substitute(
    paste(base, ", ", et, " = ", ev, ci),
    env = list(base = base, et = effect.size.text, ev = es.val, ci = ci.str)
  )
}

# APA-7 effect-size / CI-bound value: two decimals, dropping the leading zero for
# a metric bounded within [-1, 1] (r, eta-squared, Kendall's W, Cliff's delta,
# rank-biserial) and keeping it for the unbounded Cohen's d.
apa_format_effsize_value <- function(value, bounded = TRUE){
  if(is.na(value)) return(NA_character_)
  txt <- formatC(round(value, 2), format = "f", digits = 2)
  if(isTRUE(bounded)) txt <- sub("^(-?)0\\.", "\\1.", txt)
  txt
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
      conover_test = quote(italic("t")),
      friedman_conover_test = quote(italic("t")),
      friedman_nemenyi_test = quote(italic("q")),
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
      fligner_test = quote(italic(chi)^2),
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
      conover_test = "t",
      friedman_conover_test = "t",
      friedman_nemenyi_test = "q",
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
      fligner_test = "X2",
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
get_df <- function(stat.test, sep = ","){
  args <- attr(stat.test, "args")
  df.cols <- c("df", "DFn", "DFd")
  if(!any(df.cols %in% colnames(stat.test))){
    return(NA)
  }
  if(all(c("DFn", "DFd") %in% colnames(stat.test))){
    dfn <- round_value(stat.test$DFn, 2)
    dfd <- round_value(stat.test$DFd, 2)
    df <- paste(dfn, dfd, sep = sep)
  }
  else{
    df <- round_value(stat.test$df, 2)
  }
  df
}

# Sample count-------------------------------------------------
#' @describeIn get_test_label Extracts sample counts (n) from an rstatix test outputs. Returns a numeric vector.
#' @export
get_n <- function(stat.test){
  if(inherits(stat.test, "anova_test")){
    .args <- attr(stat.test, "args")
    wid <- .args$wid
    if(is.null(wid)) n <- nrow(.args$data)
    else n <- .args$data %>% pull(tidyselect::all_of(wid)) %>% unique() %>% length()
    stat.test$n <- n
  }
  else if(inherits(stat.test, "grouped_anova_test")){
    # compute sample size of data subsets
    .args <- attr(stat.test, "args")
    stat.test$n <- .args$data %>%
      dplyr::summarise(n = dplyr::n()) %>%
      pull("n")
  }
  n.cols <- c("n", "n1", "n2")
  if(!any(n.cols %in% colnames(stat.test))){
    return(NA)
  }
  if("n" %in% colnames(stat.test)){
    n <- stat.test$n
  }
  else if(all(c("n1", "n2") %in% colnames(stat.test))){
    if(is_paired(stat.test)) n <- stat.test$n1
    else n <- stat.test$n1 + stat.test$n2
  }
  n
}

# Statistical test description ---------------------------------
#' @describeIn get_test_label Extracts the description of an rstatix test outputs. Returns a character vector.
#' @export
get_description <- function(stat.test){
  tests <- c(
    t_test = "T test",
    wilcox_test = "Wilcoxon test",
    sign_test = "Sign test",
    ks_test = "Kolmogorov-Smirnov test",
    dunn_test = "Dunn test",
    conover_test = "Conover test",
    friedman_conover_test = "Durbin-Conover test",
    friedman_nemenyi_test = "Nemenyi test",
    dunnett_test = "Dunnett test",
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
    chisq_trend_test = "Chi-square trend test",
    fligner_test = "Fligner-Killeen"
  )
  args <- attr(stat.test, "args")
  if(is.null(args)) return("")
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
get_effect_size <- function(stat.test, type = "text", style = "classic"){
  stat.method <- attr(stat.test, "args")$method
  value <- text <- column <- NA
  # bounded = the metric lies in [-1, 1] (eta-squared, r, Kendall's W, Cliff's
  # delta, rank-biserial); FALSE only for the unbounded Cohen's d. Used by the
  # APA style to drop the leading zero of bounded effect sizes.
  bounded <- TRUE

  if("ges" %in% colnames(stat.test)) {
    value <- stat.test$ges
    column <- "ges"
    if(type == "expression") text <- quote(eta["g"]^2)
    else text <- "eta2[g]"
  }
  else if("pes" %in% colnames(stat.test)) {
    if(type == "expression") text <- quote(eta["p"]^2)
    else text <- "eta2[p]"
    value <- stat.test$pes
    column <- "pes"
  }
  else if("effsize" %in% colnames(stat.test)){
    value <- stat.test$effsize
    column <- "effsize"
    bounded <- !identical(stat.method, "t_test")
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
  # Per-metric effect-size columns added by effect.size = TRUE on the pairwise
  # tests (t_test/games_howell -> cohens.d, wilcox -> cliff.delta, dunn -> r,
  # paired wilcox -> rank.biserial). Consulted ONLY for style = "apa": the
  # classic label historically ignored these columns, so reading them there
  # would change a pre-existing label (regression).
  else if(style == "apa" && "cohens.d" %in% colnames(stat.test)){
    value <- stat.test$cohens.d
    column <- "cohens.d"
    bounded <- FALSE
    text <- if(type == "expression") quote(italic("d")) else "d"
  }
  else if(style == "apa" && "cliff.delta" %in% colnames(stat.test)){
    value <- stat.test$cliff.delta
    column <- "cliff.delta"
    text <- if(type == "expression") quote(italic(delta)) else "delta"
  }
  else if(style == "apa" && "rank.biserial" %in% colnames(stat.test)){
    value <- stat.test$rank.biserial
    column <- "rank.biserial"
    text <- if(type == "expression") quote(italic("r")["rb"]) else "r[rb]"
  }
  else if(style == "apa" && "r" %in% colnames(stat.test)){
    value <- stat.test$r
    column <- "r"
    text <- if(type == "expression") quote(italic("r")) else "r"
  }
  list(value = value, text = text, bounded = bounded, column = column)
}

# Check if paired stat test--------------------------------------------
is_paired <- function(stat.test){
  args <- attr(stat.test, "args")
  is.paired <- args$paired
  if(is.null(is.paired)) is.paired <- FALSE
  is.paired
}

