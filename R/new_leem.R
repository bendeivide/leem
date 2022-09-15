# Constructor of object of leem class
#
# @param x R object (vector as data structure).
# @param variable Type of data. If \code{discrete} (default), the data are categorical (numeric or not). If continuous, the data are numeric.
# @return The \code{variable} argument also allows using \code{argument = 1} for categorical variable and \code{variable = 2} for continuous variable.
# @examples
# # Example 1
# library(leem)
# x <- rbinom(36, 10, 0.6)
# new_leem(x, variable = 1)
#
# # Example 2 (Pipe operator)
# rnorm(36, 100, 4) |> new_leem(variable = 2)
#
#' @export
new_leem <- function(x = vector(), variable = "discrete") {
  stopifnot("The x argument should be vector!" = is.vector(x))
  if (variable == 1) variable <- "discrete"
  if (variable == 2) variable <- "continuous"
  if(!any(variable == c("discrete", "continuous"))) stop("The variable argument must be 'discrete' or 'continuous'.")
  structure(x, class = "leem", variable = variable)
}
