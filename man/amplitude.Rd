% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures_of_dispersion.R
\name{amplitude}
\alias{amplitude}
\title{Amplitude}
\usage{
amplitude(x, rounding = 2, na.rm = FALSE, details = FALSE, grouped = TRUE)
}
\arguments{
\item{x}{R object (list) of class leem. Use \code{new_leem()} function.}

\item{rounding}{Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).}

\item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.}

\item{details}{Logical object. Details of data (default \code{FALSE}).}

\item{grouped}{Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).}
}
\description{
Compute the sample range
}
\examples{
# Example 1: Poisson data
set.seed(10)
rpois(30, 2.5) |>
  new_leem() |>
  amplitude(grouped = FALSE)
# Example 2: Normal data
rnorm(50, 100, 2.5) |>
  new_leem(variable = 2) |>
  amplitude()
}
