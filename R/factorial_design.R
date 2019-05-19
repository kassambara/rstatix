#' @include utilities.R
NULL
#'Build Factorial Designs for ANOVA
#'
#'
#'@description Provides helper functions to build factorial design for easily
#'  computing ANOVA using the \code{\link[car]{Anova}()} function. This might be
#'  very useful for repeated measures ANOVA, which is hard to set up with the
#'  \code{car} package.
#'@inheritParams anova_test
#'@param data a data frame containing the variables
#'@param ... within variable names (i.e., repeated measure variables)
#'@return a list with the following components: \itemize{ \item \strong{the
#'  specified arguments}: \code{dv, wid, between, within} \item \strong{data}:
#'  the original data (long format) or independent ANOVA. The wide format is
#'  returned for repeated measures ANOVA. \item \strong{idata}: an optional data
#'  frame giving the levels of factors defining the intra-subject model for
#'  multivariate repeated-measures data. \item \strong{idesign}: a one-sided
#'  model formula using the “data” in idata and specifying the intra-subject
#'  design. \item \strong{repeated}: logical. Value is TRUE when the data is a
#'  repeated design. \item \strong{lm_formula}: the formula used to build the
#'  \code{lm} model. \item \strong{lm_data}: the data used to build the \code{lm}
#'  model. Can be either in a long format (i.e., the original data for
#'  independent measures ANOVA) or in a wide format (case of repeated measures ANOVA). \item \strong{model}: the \code{lm} model }
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{anova_test}()}, \code{\link{anova_summary}()}
#'@examples
#'# Load data
#'#:::::::::::::::::::::::::::::::::::::::
#'data("ToothGrowth")
#'df <- ToothGrowth
#' head(df)
#'
#'# Repeated measures designs
#'#:::::::::::::::::::::::::::::::::::::::::
#'# Prepare the data
#'df$id <- rep(1:10, 6) # Add individuals id
#'head(df)
#'# Build factorial designs
#'design <- factorial_design(df, dv = len, wid = id, within = c(supp, dose))
#'design
#'# Easily perform repeated measures ANOVA using the car package
#' res.anova <- Anova(design$model, idata = design$idata, idesign = design$idesign, type = 3)
#' summary(res.anova, multivariate = FALSE)
#'
#'# Independent measures designs
#'#:::::::::::::::::::::::::::::::::::::::::
#'# Build factorial designs
#' df$id <- 1:nrow(df)
#' design <- factorial_design(df, dv = len, wid = id, between = c(supp, dose))
#' design
#' # Perform ANOVA
#' Anova(design$model, type = 3)
#'
#'@rdname factorial_design
#'@export
factorial_design <- function(data, dv, wid, between, within, covariate){
  # Check factorial design %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  . <- NULL
  .args <- rlang::enquos(dv = dv, between = between, wid = wid, within = within, covariate = covariate) %>%
    get_quo_vars_list(data, .) %>%
    remove_null_items() %>%
    add_item(data = data) %>%
    check_factorial_design()
  data <- .args$data
  dv <- .args$dv
  between <- .args$between
  within <- .args$within
  covariate <- .args$covariate
  rhs <- create_formula_right_hand_side(between, covariate)
  # Repeated measures  designs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(!is.null(within)){
    not.within.vars <- setdiff(colnames(data), within)
    nested <- data %>%
      group_by(!!!syms(within)) %>%
      nest()
    # Get intra-subject factor levels
    .args$idata <- nested %>%
      select(-data) %>%
      dplyr::arrange(!!!syms(within)) %>%
      as.data.frame()
    .args$idesign <- paste(within, collapse = "*") %>%
      paste0('~',.) %>%
      stats::as.formula()
    # Unite intra-subject factors into one grouping column,
    # then spread the data into wide format
    wide <- nested %>%
      tidyr::unite(!!!syms(within), col = ".group.", sep = "_") %>%
      select(.data$.group., data) %>%
      unnest() %>%
      spread(key = ".group.", value = dv) %>%
      as_tibble()
    .args$lm_data <- wide
    .args$repeated <- TRUE
    # Build model formula: orders of wide dv name and data colnames should match
    # dv are all possible combinations of within-subjects factor levels
    wide.dv.name <- setdiff(colnames(wide), not.within.vars) %>%
      paste(collapse = ", ")
    lm_formula <- paste0("cbind(", wide.dv.name, ") ~ ", rhs)
  }
  # Independent measures designs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  else if(!is.null(between)){
    .args$lm_data <- .args$data
    .args$repeated <- FALSE
    lm_formula <- paste0(dv, " ~ ", rhs)
  }
  # Fit lm
  lm_formula <- .args$lm_formula <- stats::as.formula(lm_formula)
  data <- .args$lm_data
  opt <- options( "contrasts" = c( "contr.sum", "contr.poly" ) )
  .args$model <- stats::lm(lm_formula, data)
  options(opt)
  .args
}


create_formula_right_hand_side <- function(between, covariate = NULL){
  covariate <- paste(covariate, collapse = "+")
  between <- paste(between, collapse = "*")
  bc.sep <- ifelse(covariate != "" & between != "", " + ", "") # Between and covariate vars separator
  rhs <- paste0(covariate, bc.sep, between)
  if(rhs == "") rhs <- "1"
  rhs
}


# Cheking the design
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_factorial_design <- function(.args){
  if(!inherits(.args$data, "data.frame")){
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
    assertthat_iv_has_enough_levels()
  .args
}

# Remove missing values
remove_missing_values_in_data <- function(.args){
  complete.rows <- stats::complete.cases(.args$data)
  na.rows <- which(!complete.rows)
  na.exists <- length(na.rows) > 0
  if(na.exists){
    warning(
      "NA detected in rows: ", paste(na.rows, collapse = ","), ".",
      "\nRemoving this rows before the analysis.",
      call. = FALSE
    )
    .args$data <- .args$data[complete.rows, ]
  }
  .args
}

# Make sure the dependent variable (dv) is numeric
assertthat_dv_is_numeric <- function(.args){
  if(is.null(.args$dv)){
    stop("The dependent variable argument 'dv' is required")
  }
  dv.data <- .args$data %>% select(!!!syms(.args$dv))
  if(!is_all_columns_numeric(dv.data)){
    stop("The dependent variable 'dv' should be numeric")
  }
  invisible(.args)
}

# Make sure that the id is provided, otherwise
# Create it in the case of between-Ss ANOVA
assertthat_wid_is_specified <- function(.args){
  if(is.null(.args$wid)){
    if(!is.null(.args$within)){
      stop("Specify the argument 'wid'",
           ", required for repeated measures ANOVA")
    }
    else{
      .args$wid <- ".id."
      .args$data$.id. <- factor(1:nrow(.args$data))
    }
  }
  .args
}

# Check if individual id is unique in each between groups
# otherwise, create unique id accross between groups
asserthat_wid_is_unique <- function(.args){
  if(is.null(.args$between)) return(.args)
  if(!is_id_unique_by_between_vars(.args)){
    warning("The 'wid' column contains duplicate ids across ",
            "between-subjects variables. ",
            "Automatic unique id will be created",
            immediate. = TRUE, call. = FALSE)
    wid <- .args$wid
    .args$data <- .args$data %>%
      mutate(!!wid := create_uniqueId_by_bteween_vars(.args))
  }
  .args
}
is_id_unique_by_between_vars <- function(.args){
  data <- .args$data
  wid <- .args$wid
  between <- .args$between
  # Split the data by between variables
  nested <- data %>%
    group_by(!!!syms(between)) %>%
    nest()  %>%
    mutate(data = map(.data$data, dplyr::distinct, !!sym(wid), .keep_all = TRUE))
  # Check that id is unique accross between groups
  freq <- nested %>%
    unnest() %>%
    group_by(!!!syms(c(wid))) %>%
    summarise(count = n()) %>%
    pull(.data$count)
  all(freq == 1)
}
create_uniqueId_by_bteween_vars <- function(.args){
  data <- .args$data
  vars <- c(.args$wid, .args$between)
  data %>%
    select(!!!syms(vars)) %>%
    dplyr::mutate_all(as.character) %>%
    purrr::pmap(paste, sep = ".") %>%
    unlist() %>%
    factor()
}

# Make sure that independent variables (iv) has more than one levels
assertthat_iv_has_enough_levels <- function(.args){
  vars <- c(.args$within, .args$between)
  data <- .args$data
  for(.var in vars){
    n.levels <- unique(data[[.var]]) %>% length()
    if(n.levels == 1){
      stop("Variable ", .var, "has only one level. ",
           "Remove it from the model.")
    }
  }
  .args
}
# Convert the grouping variables to factor
convert_grouping_vars_to_factor <- function(.args){
  .args$data <- .args$data %>%
    convert_as_factor(vars = c(.args$wid, .args$between)) %>%
    convert_as_factor(vars = .args$within, make.valid.levels = TRUE)
  .args
}



