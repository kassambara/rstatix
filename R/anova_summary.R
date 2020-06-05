#' @include utilities.R utilities_two_sample_test.R factorial_design.R
NULL
#'Create Nice Summary Tables of ANOVA Results
#'
#'
#'@description Create beautiful summary tables of ANOVA test results obtained
#'  from either \code{\link[car]{Anova}()} or \code{\link[stats]{aov}()}.
#'
#'  The results include ANOVA table, generalized effect size and some assumption
#'  checks.
#'
#'@param effect.size the effect size to compute and to show in the ANOVA
#'  results. Allowed values can be either "ges" (generalized eta squared) or
#'  "pes" (partial eta squared) or both. Default is "ges".
#'@param observed Variables that are observed (i.e, measured) as compared to
#'  experimentally manipulated. The default effect size reported (generalized
#'  eta-squared) requires correct specification of the observed variables.
#'@param detailed If TRUE, returns extra information (sums of squares columns,
#'  intercept row, etc.) in the ANOVA table.
#'@param object an object of returned by either \code{\link[car]{Anova}()}, or
#'  \code{\link[stats]{aov}()}.
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
#'  The \strong{returned object might have an attribute} called \code{args} if
#'  you compute ANOVA using the function \code{\link{anova_test}()}. The attribute \code{args} is a
#'  list holding the arguments used to fit the ANOVA model, including: data, dv,
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
#'@seealso \code{\link{anova_test}()}, \code{\link{factorial_design}()}
#' @examples
#'# Load data
#'#:::::::::::::::::::::::::::::::::::::::
#'data("ToothGrowth")
#'df <- ToothGrowth
#'df$dose <- as.factor(df$dose)
#'
#'# Independent measures ANOVA
#'#:::::::::::::::::::::::::::::::::::::::::
#'# Compute ANOVA and display the summary
#' res.anova <- Anova(lm(len ~ dose*supp, data = df))
#' anova_summary(res.anova)
#'
#'# Display both SSn and SSd using detailed = TRUE
#'# Show generalized eta squared using effect.size = "ges"
#'anova_summary(res.anova, detailed = TRUE, effect.size = "ges")
#'
#'# Show partial eta squared using effect.size = "pes"
#'anova_summary(res.anova, detailed = TRUE, effect.size = "pes")
#'
#'# Repeated measures designs using car::Anova()
#'#:::::::::::::::::::::::::::::::::::::::::
#'# Prepare the data
#'df$id <- as.factor(rep(1:10, 6)) # Add individuals ids
#'head(df)
#'
#'# Easily perform repeated measures ANOVA using the car package
#' design <- factorial_design(df, dv = len, wid = id, within = c(supp, dose))
#' res.anova <- Anova(design$model, idata = design$idata, idesign = design$idesign, type = 3)
#' anova_summary(res.anova)
#'
#'# Repeated measures designs using stats::Aov()
#'#:::::::::::::::::::::::::::::::::::::::::
#' res.anova <- aov(len ~ dose*supp + Error(id/(supp*dose)), data = df)
#' anova_summary(res.anova)

#'@name anova_summary
#'@export
anova_summary <- function(object, effect.size = "ges", detailed = FALSE, observed = NULL){
  if(inherits(object, "Anova.mlm")){
    results <- repeated_anova_summary(object)
  }
  else if(inherits(object, "anova")){
    results <- summary_independent_anova(object)
  }
  else if(inherits(object, c("aov", "aovlist"))){
    results <- summary_aov(object)
  }
  else{
    stop("Non-supported object passed: ",
         paste(class(object), collapse = ", "), ". ",
         "Object needs to be of class 'Anova.mlm' or 'anova'.")
  }
  .args <- attr(object, "args") # exist only in anova_test()
  results <- results %>%
    add_anova_effect_size(effect.size, observed)

  if(!detailed){
    results <- remove_details(results, method = "anova")
  }
  results$ANOVA <- order_by_interaction_levels(results$ANOVA)
  results <- results %>% map(~dplyr::mutate_if(., is.numeric, round_value, 3))
  if(length(results) == 1) results <- results[[1]]
  results %>% set_attrs(args = .args)
}




# Summary of Anova.mlm object: summary_anova_mlm
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      df.gg = paste(round_value(.data$GGe*.data$DFn, 2), round_value(.data$GGe*.data$DFd, 2), sep = ", "),
      df.hf = paste(round_value(.data$HFe*.data$DFn, 2), round_value(.data$HFe*.data$DFd, 2), sep = ", ")
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
summary_independent_anova <- function(res.anova){
  res.anova <- as.data.frame(res.anova)
  .residuals <- res.anova["Residuals", 1:2]
  if('Mean Sq' %in% colnames(res.anova)){
    # exists when res.anova is from stats::anova
    res.anova <- select(res.anova, -.data$`Mean Sq`)
  }
  if('Sum Sq' %in% colnames(res.anova)){
    # in stats::anova, Sum Sq is not the first column, so do select
    res.anova <- res.anova %>% select(.data$`Sum Sq`, dplyr::everything())
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      mutate(`p<.05` = as.character(ifelse(.data$p < 0.05, "*",''))) %>%
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


# Add effect size
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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



