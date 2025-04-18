% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabfreq.R
\name{tabfreq}
\alias{tabfreq}
\title{Frequency distribution table}
\usage{
tabfreq(data, ...)
}
\arguments{
\item{data}{R object (data structure vector) of class leem. Use \code{new_leem()} function.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The result of \code{tabfreq()} is a list. This list has two elements: \code{table} and \code{statistics}. The first is the data frequency table, and the second represents some useful statistics for methods of leem class.
}
\description{
Generic function that allows you to tabulate continuous and categorical data (quantitative or qualitative) in frequency distribution. Depending on the nature of the data, they can be grouped into class ranges or not.
}
\examples{
# Example 1
library(leem)
x <- rbinom(36, 10, 0.6)
x <- new_leem(x, variable = "discrete")
tabfreq(x)

# Example 2 (Pipe operator)
rnorm(36, 100, 4) |>
  new_leem(variable = "continuous") |> tabfreq()

# Example 3
x <- rbinom(36, 10, 0.6)
# Constructor (object of leem class)
x <- new_leem(x, variable = "discrete")
tab <- tabfreq(x)
# Details
tab$table
tab$statistics

# Example 3 - ordered categories ("d","a", "b", "c")
w <- rep(letters[1:4], 1:4)
w |> new_leem(variable = "discrete") |> tabfreq(ordered = c("d","a", "b", "c"))

}
