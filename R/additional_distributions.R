#' The Gumbel Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the normal distribution with parameters: location and scale
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param location numerical. It represents location parameter. See Details.
#' @param scale numerical. It represents scale parameter.  See Details.
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
#'
#' @details
#' The CDF of Gumbel distribution is:
#' \deqn{
#'    F(x;\mu ,\beta )=e^{-e^{-(x-\mu )/\beta }}, \quad \mu \in \mathbf{R}, \beta > 0,
#' }
#'where \eqn{\mu} is location parameter (\code{location}) and \eqn{\beta} is scale parameter (\code{scale}).
#' The PDF of Gumbel distribution is:
#' \deqn{
#'    \frac{1}{\beta }e^{-(z+e^{-z})},
#' }
#' where \eqn{z={\frac {x-\mu }{\beta }}}.
#' The quantile is:
#' \deqn{
#'   \mu -\beta \ln(-\ln(p)), \quad 0 < p < 1.
#' }
#'
#' @examples
#' # PDF
#' dgumbel(1, 0, 1)
#' # CDF
#' pgumbel(1, 0, 1)
#' # Quantile
#' qgumbel(0.2, 0, 1)
#' @name Gumbel_distribution
#' @export
dgumbel <- function(x, location, scale){
  z <- (x - location)/scale
  density <- (exp(-(z + exp(-z))))/scale
  return(density)
}

#' @rdname Gumbel_distribution
#' @export
pgumbel <- function(q, location, scale, lower.tail = TRUE) {
  z <- (q - location)/scale
  p <- exp(-(z+exp(-z)))/scale
  prob <- exp(-exp(-z))
  if (lower.tail == FALSE) {
    prob <- 1 - prob
  }
  return(prob)
}

#' @rdname Gumbel_distribution
#' @export
qgumbel <- function(p, location = 0, scale = 1, lower.tail = TRUE) {
  if(!lower.tail){
    p <- 1 - p
  }
    quantil <- location - scale * log(-log(p))
  return(quantil)
}
