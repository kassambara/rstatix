#' @include utilities.R
NULL
#'Compute Summary Statistics
#'@description Compute summary statistics for one or multiple numeric variables.
#'@param data a data frame
#'@param ... (optional) One or more unquoted expressions (or variable names)
#'  separated by commas. Used to select a variable of interest. If no variable
#'  is specified, then the summary statistics of all numeric variables in the
#'  data frame is computed.
#'@param type type of summary statistics. Possible values include: \code{"full",
#'  "common", "robust",  "five_number", "mean_sd", "mean_se", "mean_ci",
#'  "median_iqr", "median_mad", "quantile", "mean", "median",  "min", "max"}
#'@param show a character vector specifying the summary statistics you want to
#'  show. Example: \code{show = c("n", "mean", "sd")}. This is used to filter
#'  the output after computation. It can additionally include \code{"skewness"}
#'  and/or \code{"kurtosis"} (e.g. \code{show = c("mean", "sd", "skewness",
#'  "kurtosis")}); these two are computed on demand and are not part of any
#'  default \code{type}.
#' @param probs numeric vector of probabilities with values in [0,1]. Used only when type = "quantile".
#'@param digits integer indicating the number of decimal places to round the
#'  summary statistics to. Default is 3. Increase it when summarizing very small
#'  values that would otherwise round to 0.
#'@return A data frame containing descriptive statistics, such as: \itemize{
#'  \item \strong{n}: the number of individuals \item \strong{min}: minimum
#'  \item \strong{max}: maximum \item \strong{median}: median \item
#'  \strong{mean}: mean \item \strong{q1, q3}: the first and the third quartile,
#'  respectively. \item \strong{iqr}: interquartile range \item \strong{mad}:
#'  median absolute deviation (see ?MAD) \item \strong{sd}: standard deviation
#'  of the mean \item \strong{se}: standard error of the mean \item \strong{ci}: 95 percent confidence interval of the mean }
#'
#'  When requested through \code{show}, the output can also contain: \itemize{
#'  \item \strong{skewness}: bias-corrected sample skewness \item
#'  \strong{kurtosis}: bias-corrected sample excess kurtosis (0 for a normal
#'  distribution). } Both use the type-2 (bias-corrected) estimator, matching
#'  \code{e1071} with \code{type = 2}:
#'  skewness \eqn{= g_1\sqrt{n(n-1)}/(n-2)} and kurtosis \eqn{= [(n+1)g_2 + 6]
#'  (n-1)/[(n-2)(n-3)]}, where \eqn{g_1 = m_3/m_2^{1.5}} and \eqn{g_2 =
#'  m_4/m_2^2 - 3}. Skewness is \code{NA} for n < 3 and kurtosis for n < 4.
#'@seealso \code{\link{rstatix-programming}} for selecting columns by names held
#'  in strings (\code{!!}, \code{\{\{ \}\}}, \code{vars=}, \code{all_of()}).
#' @examples
#' # Full summary statistics
#' data("ToothGrowth")
#' ToothGrowth %>% get_summary_stats(len)
#'
#' # Summary statistics of grouped data
#' # Show only common summary
#' ToothGrowth %>%
#'   group_by(dose, supp) %>%
#'   get_summary_stats(len, type = "common")
#'
#' # Robust summary statistics
#' ToothGrowth %>% get_summary_stats(len, type = "robust")
#'
#' # Five number summary statistics
#' ToothGrowth %>% get_summary_stats(len, type = "five_number")
#'
#' # Compute only mean and sd
#' ToothGrowth %>% get_summary_stats(len, type = "mean_sd")
#'
#' # Compute full summary statistics but show only mean, sd, median, iqr
#' ToothGrowth %>%
#'     get_summary_stats(len, show = c("mean", "sd", "median", "iqr"))
#'
#' # Include skewness and kurtosis (computed on demand via show)
#' ToothGrowth %>%
#'     get_summary_stats(len, show = c("mean", "sd", "skewness", "kurtosis"))
#'
#'@export
get_summary_stats <- function(
  data, ..., type = c("full", "common", "robust",  "five_number",
                      "mean_sd", "mean_se", "mean_ci", "median_iqr", "median_mad", "quantile",
                      "mean", "median",  "min", "max" ),
  show = NULL, probs = seq(0, 1, 0.25), digits = 3
  ){
  type = match.arg(type)
  if(is_grouped_df(data)){
    results <- data %>%
      doo(get_summary_stats, ..., type = type, show = show, probs = probs, digits = digits)
    return(results)
  }
  data <- data %>% select_numeric_columns()
  vars <- data %>% get_selected_vars(...)
  n.vars <- length(vars)
  if(n.vars >= 1){
    data <- data %>% select(!!!syms(vars))
  }
  variable <- .value. <- NULL
  data <- data %>%
    gather(key = "variable", value = ".value.") %>%
    filter(!is.na(.value.)) %>%
    dplyr::mutate(variable = factor(.data$variable, levels = vars)) %>%
    group_by(variable)
  results <- switch(
    type,
    common = common_summary(data),
    robust = robust_summary(data),
    five_number = five_number_summary(data),
    mean_sd = mean_sd(data),
    mean_se = mean_se(data),
    mean_ci = mean_ci(data),
    median_iqr = median_iqr(data),
    median_mad = median_mad(data),
    quantile = quantile_summary(data, probs),
    mean = mean_(data),
    median = median_(data),
    min = min_(data),
    max = max_(data),
    full_summary(data)
  ) %>%
    dplyr::ungroup()
  # Skewness/kurtosis are not part of any default type; compute on demand only
  # when requested through `show`, then join so rounding/selection cover them.
  if(!is.null(show) && any(c("skewness", "kurtosis") %in% show)){
    .value. <- NULL
    dist <- data %>%
      dplyr::summarise(
        skewness = .skewness(.data$.value.),
        kurtosis = .kurtosis(.data$.value.)
      ) %>%
      dplyr::ungroup()
    results <- dplyr::left_join(results, dist, by = "variable")
  }
  results <- results %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  if(!is.null(show)){
    show <- unique(c("variable", "n", show))
    results <- results %>% select(!!!syms(show))
  }

  results
}


full_summary <- function(data){
  confidence <- 0.95
  alpha <- 1 - confidence
  .value. <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      min = min(.value., na.rm=TRUE),
      max = max(.value., na.rm=TRUE),
      median = stats::median(.value., na.rm=TRUE),
      q1 = stats::quantile(.value., 0.25, na.rm = TRUE),
      q3 = stats::quantile(.value., 0.75, na.rm = TRUE),
      iqr = stats::IQR(.value., na.rm=TRUE),
      mad = stats::mad(.value., na.rm=TRUE),
      mean = mean(.value., na.rm = TRUE),
      sd = stats::sd(.value., na.rm = TRUE)
    ) %>%
    mutate(
      se = .data$sd / sqrt(.data$n),
      ci = abs(stats::qt(alpha/2, .data$n-1)*.data$se)
    )
}

common_summary <- function(data){
  confidence <- 0.95
  alpha <- 1 - confidence
  .value. <- ci <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      min = min(.value., na.rm=TRUE),
      max = max(.value., na.rm=TRUE),
      median = stats::median(.value., na.rm=TRUE),
      iqr = stats::IQR(.value., na.rm=TRUE),
      mean = mean(.value., na.rm = TRUE),
      sd = stats::sd(.value., na.rm = TRUE)
    ) %>%
    mutate(
      se = .data$sd / sqrt(.data$n),
      ci = abs(stats::qt(alpha/2, .data$n-1)*.data$se)
    )
}

robust_summary <- function(data){
  .value. <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      median = stats::median(.value., na.rm=TRUE),
      iqr = stats::IQR(.value., na.rm=TRUE)
    )
}

quantile_summary <- function(data, probs = seq(0, 1, 0.25)){
  core_func <- function(data, probs){
    .value. <- NULL
    n <- sum(!is.na(data$.value.))
    names(n) <- "n"
    q <- stats::quantile(data$.value., probs, na.rm = TRUE)
    results <- t(matrix(c(n, q)))
    colnames(results) <- c("n", names(q))
    tibble::as_tibble(results)
  }
  results  <- data %>%
    nest() %>%
    mutate(.results. = map(data, core_func, probs)) %>%
    select(all_of(c("variable", ".results."))) %>%
    unnest(cols = ".results.")
  results
}



five_number_summary <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      min = min(.value., na.rm=TRUE),
      max = max(.value., na.rm=TRUE),
      q1 = stats::quantile(.value., 0.25, na.rm = TRUE),
      median = stats::median(.value., na.rm=TRUE),
      q3 = stats::quantile(.value., 0.75, na.rm = TRUE)
    )
}

mean_ <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      mean = mean(.value., na.rm = TRUE)
    )
}
median_ <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      median = stats::median(.value., na.rm=TRUE)
    )
}
max_ <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      max = max(.value., na.rm = TRUE)
    )
}
min_ <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      min = min(.value., na.rm = TRUE)
    )
}

mean_sd <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      mean = mean(.value., na.rm = TRUE),
      sd = stats::sd(.value., na.rm = TRUE)
    )
}

mean_se <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      mean = mean(.value., na.rm = TRUE),
      sd = stats::sd(.value., na.rm = TRUE)
    ) %>%
    mutate(se = .data$sd / sqrt(.data$n))%>%
    select(-any_of("sd"))
}

mean_ci <- function(data){
  confidence <- 0.95
  alpha <- 1 - confidence
  .value. <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      mean = mean(.value., na.rm = TRUE),
      sd = stats::sd(.value., na.rm = TRUE)
    ) %>%
    mutate(
      se = .data$sd / sqrt(.data$n),
      ci = abs(stats::qt(alpha/2, .data$n-1)*.data$se)
    )%>%
    select(-any_of(c("se", "sd")))
}

median_iqr <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      median = stats::median(.value., na.rm=TRUE),
      iqr = stats::IQR(.value., na.rm=TRUE)
    )
}

median_mad <- function(data){
  .value.  <- NULL
  data %>%
    dplyr::summarise(
      n = sum(!is.na(.value.)),
      median = stats::median(.value., na.rm=TRUE),
      mad = stats::mad(.value., na.rm=TRUE)
    )
}

# Bias-corrected sample skewness/kurtosis (type-2 estimator, matching e1071
# with type = 2). NA when n is too small. (#99)
.skewness <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  if(n < 3) return(NA_real_)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  if(m2 == 0) return(NA_real_)
  g1 <- m3 / m2^1.5
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

.kurtosis <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  if(n < 4) return(NA_real_)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m4 <- mean((x - m)^4)
  if(m2 == 0) return(NA_real_)
  g2 <- m4 / m2^2 - 3
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}
