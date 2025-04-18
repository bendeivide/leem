% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showcdf.R
\name{showcdf}
\alias{showcdf}
\title{Distribution Function Properties}
\usage{
showcdf(variable = "discrete", prop = NULL)
}
\arguments{
\item{variable}{Variabe type. Defaults \code{variable = "discrete"}. The options are: \code{discrete} or "\code{1}" and \code{continuous} or "\code{2}".}

\item{prop}{Properties for distribution function. See Details.}
}
\description{
Graphic presentation of properties for distribution function
}
\details{
\itemize{
\item \code{prop = 1}: \eqn{\lim_{x\rightarrow\infty}F_X(x)=1} and \eqn{\lim_{x\rightarrow -\infty}F_X(x)=0};
\item \code{prop = 2}: \eqn{F_X(x)\leq F_X(y), ~ x\leq y~\forall x,y \in \mathbb{R}};
\item \code{prop = 3}: \eqn{\lim_{x_n\downarrow x}F_X(x_n)\downarrow F_X(x)}.
}
}
\examples{
library(leem)
# Example 1
showcdf()
}
