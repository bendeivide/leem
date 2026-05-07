#' Z-Transformation (Standardization of the Normal Distribution)
#'
#' Computes the Z-transformation (standardization) of a random variable
#' assumed to follow a normal distribution, given an observed value,
#' the mean, and the standard deviation.
#'
#' The transformation is defined as:
#'
#' \deqn{Z = \frac{X - \mu}{\sigma}}
#'
#' where:
#' \itemize{
#'   \item \eqn{X} is the observed value;
#'   \item \eqn{\mu} is the mean of the distribution;
#'   \item \eqn{\sigma} is the standard deviation of the distribution.
#' }
#'
#' @param x A numeric value or vector of values to be standardized.
#' @param mu The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution (must be positive).
#'
#' @return A numeric value or vector containing the standardized values (Z-scores).
#'
#' @details
#' The Z-transformation converts a variable with distribution
#' \eqn{N(\mu, \sigma^2)} into a standard normal variable
#' \eqn{N(0, 1)}.
#'
#' @examples
#' # Simple example
#' z(x = 10, mu = 8, sd = 2)
#'
#' # Vector of values
#' z(x = c(10, 12, 14), mu = 10, sd = 2)
#'
#' @export
z <- function(x, mu = 0, sd = 1) {

  # Basic checks
  if (!is.numeric(x)) {
    stop("Argument 'x' must be numeric.")
  }

  if (!is.numeric(mu) || length(mu) != 1) {
    stop("Argument 'mu' must be a single numeric value.")
  }

  if (!is.numeric(sd) || length(sd) != 1 || sd <= 0) {
    stop("Argument 'sd' must be a positive numeric value.")
  }

  # Z-transformation
  z <- (x - mu) / sd

  return(z)
}
