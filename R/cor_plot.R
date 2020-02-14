#' @include utilities.R
NULL
#' Visualize Correlation Matrix Using Base Plot
#' @description Provide a tibble-friendly framework to visualize a correlation
#'   matrix. Wrapper around the R base function
#'   \code{\link[corrplot]{corrplot}()}. Compared to
#'   \code{\link[corrplot]{corrplot}()}, it can handle directly the output of the
#'   functions \code{\link{cor_mat}() (in rstatix)}, \code{rcorr() (in Hmisc)},
#'   \code{correlate() (in corrr)} and \code{cor() (in stats)}.
#'
#'   The p-values contained in the outputs of the functions
#'   \code{\link{cor_mat}()} and \code{rcorr()} are automatically detected and
#'   used in the visualization.
#' @inheritParams corrplot::corrplot
#' @param cor.mat the correlation matrix to visualize
#' @param palette character vector containing the color palette.
#' @param p.mat matrix of p-value corresponding to the correlation matrix.
#' @param significant.level significant level, if the p-value is bigger than
#'   \code{significant.level}, then the corresponding correlation coefficient is
#'   regarded as insignificant.
#' @param insignificant character, specialized insignificant correlation
#'   coefficients, "cross" (default), "blank". If "blank", wipe away the
#'   corresponding glyphs; if "cross", add crosses (X) on corresponding glyphs.
#' @param label logical value. If TRUE, shows the correlation coefficient
#'   labels.
#' @param font.label a list with one or more of the following elements: size
#'   (e.g., 1), color (e.g., "black") and style (e.g., "bold"). Used to
#'   customize the correlation coefficient labels. For example \code{font.label
#'   = list(size = 1, color = "black", style = "bold")}.
#' @seealso \code{\link{cor_as_symbols}()}
#' @examples
#' # Compute correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.mat <- mtcars %>%
#'   select(mpg, disp, hp, drat, wt, qsec) %>%
#'   cor_mat()
#'
#' # Visualize correlation matrix
#' #::::::::::::::::::::::::::::::::::::::::::
#' # Full correlation matrix,
#' # insignificant correlations are marked by crosses
#' cor.mat %>% cor_plot()
#'
#' # Reorder by correlation coefficient
#' # pull lower triangle and visualize
#' cor.lower.tri <- cor.mat %>%
#'   cor_reorder() %>%
#'   pull_lower_triangle()
#' cor.lower.tri %>% cor_plot()
#'
#' # Change visualization methods
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.lower.tri %>%
#'   cor_plot(method = "pie")
#'
#' cor.lower.tri %>%
#'   cor_plot(method = "color")
#'
#' cor.lower.tri %>%
#'   cor_plot(method = "number")
#'
#' # Show the correlation coefficient: label = TRUE
#' # Blank the insignificant correlation
#' #::::::::::::::::::::::::::::::::::::::::::
#' cor.lower.tri %>%
#'   cor_plot(
#'     method = "color",
#'     label = TRUE,
#'     insignificant = "blank"
#'   )
#'
#' # Change the color palettes
#' #::::::::::::::::::::::::::::::::::::::::::
#'
#' # Using custom color palette
#' # Require ggpubr: install.packages("ggpubr")
#' if(require("ggpubr")){
#'   my.palette <- get_palette(c("red", "white", "blue"), 200)
#'   cor.lower.tri %>%
#'    cor_plot(palette = my.palette)
#' }
#'
#' # Using RcolorBrewer color palette
#' if(require("ggpubr")){
#'   my.palette <- get_palette("PuOr", 200)
#'   cor.lower.tri %>%
#'    cor_plot(palette = my.palette)
#' }
#'
#' @export
cor_plot <- function(cor.mat, method = "circle", type = "full", palette = NULL,
                     p.mat = NULL, significant.level = 0.05, insignificant = c("cross", "blank"),
                     label = FALSE, font.label = list()) {


  insignificant <- match.arg(insignificant)
  if(insignificant == "cross")
    insignificant <- "pch"

  # Outline color of circle, ellipse, ....
  outline <- ifelse(method == "color", "white", FALSE)

  # Correlation coefficients label parameters
  font <- parse_font(font.label)
  addCoef.col <- NULL
  if(label) addCoef.col <- font$color

  # Correlation matrix data
  show.diagonal <- TRUE
  if(inherits(cor.mat, "cor_mat")){
    cor.value <- cor.mat %>% as_matrix()
    p.mat <- cor.mat %>% cor_get_pval() %>% as_matrix()
  }
  else if(inherits(cor.mat, "cor_mat_tri")){

    cor.value <- cor.mat %>%
      as_numeric_triangle() %>%
      as_matrix()
    p.mat <- cor.mat %>%
      cor_get_pval() %>%
      as_numeric_triangle() %>%
      as_matrix()

    if(inherits(cor.mat, "lower_tri"))
      type <- "lower"
    else
      type <- "upper"

    cor.diagonal <- diag(cor.value)
    cor.diagonal.is.na <- all(is.na(cor.diagonal))
    if(cor.diagonal.is.na)
      show.diagonal <- FALSE
    else
      show.diagonal <- TRUE
  }
  else if(inherits(cor.mat, "rcorr")){
    cor.value <- cor.mat$r
    p.mat <- cor.mat$P
  }
  else {
    cor.value <- cor.mat %>% as_matrix()
  }

  # Correlation matrix p-value
  if(inherits(p.mat, "tbl_df"))
    p.mat <- p.mat %>% as_matrix()

  corrplot <- corrplot::corrplot
  corrplot(
    cor.value,  method = method, type = type,
    tl.col="black", tl.srt = 45,
    col = palette, diag = show.diagonal,
    p.mat = p.mat, sig.level = significant.level,
    insig = insignificant, pch.cex = 2,
    outline = outline, addCoef.col = addCoef.col,
    number.cex = font$size, number.font = font$style
    )

}


# Parse label font
parse_font <- function(font){

  if(.is_empty(font)){
    font <- list(size = 1, color = "black", style = "plain")
  }
  else if(!is.list(font)){
    stop("The argument font should be a list. ",
         "Example: font <- list(size = 1, color = 'black', style = 2)")
  }
  else{

    font$size <- ifelse(is.null(font$size), 1, font$size)
    font$color <- ifelse(is.null(font$color), "black", font$color)
    font$style <- ifelse(is.null(font$style), "plain", font$style)
  }

  # convert fon style to numeric
  available.styles <- c(1, 2, 3, 4) %>%
    rlang::set_names(c("plain",  "bold", "italic", "bold.italic"))
  font$style <- available.styles[font$style]

  font
}
