% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mode.R
\name{get_mode}
\alias{get_mode}
\title{Compute Mode}
\usage{
get_mode(x)
}
\arguments{
\item{x}{a vector. Can be numeric, factor or character vector.}
}
\description{
Compute the mode in a given vector. Mode is the most frequent
  value.
}
\examples{

# Mode of numeric vector
x <- c(1:5, 6, 6, 7:10)
get_mode(x)

# Bimodal
x <- c(1:5, 6, 6, 7, 8, 9, 9, 10)
get_mode(x)

# No mode
x <- c(1, 2, 3, 4, 5)
get_mode(x)

# Nominal vector
fruits <-  c(rep("orange", 10), rep("apple", 5), rep("lemon", 2))
get_mode(fruits)
}
