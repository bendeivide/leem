#' Plot of cumulative distribution function of any discrete variable
#'
#' Help in building the plot of the cumulative distribution function of any discrete variable
#'
#' @param x numeric vector of values of \eqn{X}. See __Details__.
#' @param fda numeric vector of \eqn{F_X(x)}. See __Details__.
#' @param main main title for the plot.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @return The output is plot of distribution function. See *Example 1*.
#' @details
#' Consider the \eqn{X} distribution:
#'
#' | \eqn{p_X(x)}: | 0.23 | 0.27 | 0.30 | 0.12 | 0.08|
#' |--------------:|:----:|:----:|:----:|:----:|:---:|
#' | \eqn{x}:      |  1   | 2    | 3    | 4    | 5   |
#'
#' where \eqn{p_X(x)} and \eqn{x} are probability function and values of \eqn{X}. Consider also the \eqn{X} distribution function:
#'
#' \deqn{
#' F_X(x) = \left\{\begin{array}{ll}
#' 0, & \textrm{if } x < 1;\\
#' 0.23, & \textrm{if } 1 \leq x < 2;\\
#' 0.50, & \textrm{if } 2 \leq x < 3;\\
#' 0.80, & \textrm{if } 3 \leq x < 4;\\
#' 0.92, & \textrm{if } 4 \leq x < 5;\\
#' 1.00 & \textrm{if } x \geq 5.\\
#' \end{array}\right.
#' }
#'
#' This way, the \code{cdfd} function  needs to consider only the vectors `x <- 1:5` and
#' `fda <- c(0.23, 0.50, 0.80, 0.92, 1)`, that is, only the equality conditions for \eqn{x}. See *Example 1*.
#' @md
#' @examples
#' # Example 1
#' x <- 1:5
#' fda <- c(0.23, 0.5, 0.8, 0.92, 1)
#' cdfd(x, fda)
#' @export
cdfd <- function(x, fda, main = NULL, xlab = NULL, ylab = NULL) {
  xlim <- c(min(x) - 1, max(x) + 1)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)

  # Title
  if (is.null(main)) {
    title(bquote(atop(bold("Distribution Function"), "Discrete variable")),
          xlab = bquote(X), ylab = bquote(F[X](x)), col.lab = "blue")
  } else{
    title(main = main, xlab = xlab, ylab = ylab)
  }

  # Axis
  axis(1, at = c(x, max(x)), col.axis = "blue", col.ticks = "blue", col = "blue")
  axis(2, at = c(0, fda, 1), las = 2, col.axis = "blue", col.ticks = "blue", col = "blue")

  # Segments
  x <- min(xlim):max(xlim)
  fda <- c(0, fda, 1)
  w <- 1:length(x)
  for (i in w) {
    segments(
      x[i],
      fda[i],
      x[i + 1],
      fda[i],
      lty = 1,
      col = "black"
    )
    segments(x[i + 1],
             fda[i],
             x[i + 1],
             fda[i + 1],
             lty = 2,
             col = "black")
  }
  points(x[2:6], fda[1:5], lwd = 2, pch = 19, bg = "white", col = "white")
  points(x[2:6], fda[1:5], lwd = 2, pch = 1)
  points(x[2:6], fda[2:6], lwd = 2, pch = 19)

}


#' Plot of probability function of any discrete variable
#'
#' Help in building the plot of the probability function of any discrete variable
#'
#' @param x numeric vector of values of \eqn{X}. See __Details__.
#' @param p numeric vector of \eqn{p_X(x)}. See __Details__.
#' @param main main title for the plot.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @return The output is plot of distribution function. See *Example 1*.
#' @details
#' Consider the \eqn{X} distribution:
#'
#' | \eqn{p_X(x)}: | 0.23 | 0.27 | 0.30 | 0.12 | 0.08|
#' |--------------:|:----:|:----:|:----:|:----:|:---:|
#' | \eqn{x}:      |  1   | 2    | 3    | 4    | 5   |
#'
#' where \eqn{p_X(x)} and \eqn{x} are probability function and values of \eqn{X}. See *Example 1*.
#' @md
#' @examples
#' # Example 1
#' x <- 1:5
#' p <- c(0.23, 0.27,0.30, 0.12, 0.08)
#' apf(x, p)
#' @export
apf <- function(x, p, main = NULL, xlab = NULL, ylab = NULL) {
  xlim <- c(min(x), max(x))
  ylim <- c(0, max(p) + 0.05)
  plot.new()
  plot.window(xlim, ylim)

  # Title
  if (is.null(main)) {
    title(bquote(atop(bold("Probability Function"), "Discrete variable")),
          xlab = bquote(X), ylab = bquote(p[X](x)), col.lab = "blue")
  } else{
    title(main = main, xlab = xlab, ylab = ylab)
  }

  # Axis
  axis(1, at = x, col.axis = "blue", col.ticks = "blue", col = "blue")
  axis(2, at = c(0, p), las = 2, col.axis = "blue", col.ticks = "blue", col = "blue")

  # lines and points
  points(x, p, type = "h")
  points(x, p, lwd = 2, pch = 19)

}
