#' Probability function
#'
#' \code{PF} This function computes the value of the probability function for the discrete random variables
#'
#' @details The standard distribution to be computed will be the Binomial distribution (\code{dist='binomial'}). Once you forget the function arguments, don't worry, they will be requested via the console. This means if the user does not know the parameters of the distribution, the \code{pf()} function takes care of asking the user.
#' - If \code{dist='binomial'}: \eqn{X \sim Bin(n, p)}
#'   - additional arguments: \code{size} \eqn{(n)} and \code{prob} \eqn{(p)};
#'   - Suport of X: \eqn{S_X=\{0,1,..., n\}}; this represents the values for the \code{x} argument;
#'
#'
#' @param x represents the assumed value of the random variable X. The \code{x} argument is a value of support of X. See Details.
#' @param dist distribution to use. The default is \code{'binomial'}. Options: \code{'binomial'} and \code{'poisson'}. The other discrete distributions are being implemented.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param porcentage logical; if \code{FALSE} (default), the result in decimal. Otherwise, probability is given in percentage.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{'none'}, \code{'rstudio'} or \code{'tcltk'}.
#' @param plot logical; if \code{TRUE} (default), the result will be shown along with its graphical representation; otherwise, only the result will be shown.
#' @param ... additional arguments according to the chosen distribution.
#'
#' @return \code{PF} returns the probability and its graphical representation. The result can be given as a percentage or not.
#'
#' @examples
#' # Loading package
#' library(leem)
#' # Example 1 - Binomial distribution
#' \dontrun{
#' PF(2, dist = "binomial", size = 10, prob = 0.8)
#' }
#' @export
PF <- function(x, dist = "binomial", rounding = 5, porcentage = FALSE, gui = "plot", main = NULL, plot = TRUE, ...){
  # x argument
  q <- x
  # Arguments in '...'
  argaddit <- list(...)
  # Formal arguments
  argdef <- formals(PF)
  if(dist == "poisson"){
    #Security
    if (!any(names(argaddit) == "lambda")) {
      lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
      argaddit$lambda <- as.numeric(lambda)
    }

    if (argaddit$lambda <= 0 ) stop("The 'lambda' argument must be greater then zero!", call. = FALSE, domain = "R-leem")

    #Variables
    lambda <- argaddit$lambda

    #Graphic
    minimo <- if (q < lambda) trunc(q - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
    if (minimo < 0) minimo <- 0 else minimo <- round(minimo)

    maximo <- if (q > lambda) ceiling(q + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))

    x <- minimo:maximo

    fx <- dpois(x, lambda = lambda)

    xlim <- c(minimo, maximo)
    ylim <- c(min(x), max(fx) * 1.2)

    plot.new()
    plot.window(xlim, ylim)

    axis(1, 0:maximo)
    axis(2)

    if (!any(names(argaddit) == "main")){
      main <- substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")), list(t = q, t2 = q + 1))
    }
    title(ylab = expression(p[X](x)), xlab = "X", main = main)

    lines(x, fx, type = "h", lwd = 2, panel.first = grid(col = "gray90"))

    points(x, fx, pch = 19, lwd = 2, col = "#99ccff")

    p <- dpois(q, lambda = lambda)
    pp <- round(dpois(q, lambda = lambda), digits = 2)

    segments(par("usr")[1], p, q, lty = 2, col = "red", lwd = 2)

    lines(q, p, type = "h", lwd = 2, col = "red")

    points(q, p, pch = 19, col = "red", cex = 1.5, lwd = 2)

    auxposh <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
    auxposv <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 30

    axis(side = 1, at = q, labels = FALSE, col = "#880000", lwd.ticks = 2, tick = TRUE)
    axis(side = 1, at = q, labels = substitute(x == q, list(p = p, q = q)), col = "#880000",col.axis = "#880000", font = 2, lwd = 1, tick = FALSE, pos = auxposh)

    axis(side = 2, at = p, labels = FALSE, col = "red", lwd.ticks = 2, tick = TRUE)
    axis(side = 2, at = p, labels = substitute("p"[x](q) == p, list(p = pp, q = q)), col = "red", col.axis = "red", font = 2, lwd = 1, tick = FALSE, pos = auxposv)

    legend("topleft", cex = 0.9, box.col = "black", bg = "#e0e0e0",
           legend = substitute(bold("Parameters:"~lambda ==  lambd),
                               list(lambd = lambda)))
    #Result
    prob <- dpois(q, lambda = lambda)
  }
  if(dist == "binomial"){
    #Security
    if (!any(names(argaddit) == "size")) {
      size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
      argaddit$size <- as.numeric(size)
    }

    if (argaddit$size <= 0 ) stop("The 'size' argument must be greater then zero!", call. = FALSE, domain = "R-leem")

    if (!any(names(argaddit) == "prob")) {
      prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
      argaddit$prob <- as.numeric(prob)
    }

    if (argaddit$prob > 1 ) stop("The 'prob' argument must be lower then one!", call. = FALSE, domain = "R-leem")
    if (argaddit$prob <= 0 ) stop("The 'prob' argument must be greater then zero!", call. = FALSE, domain = "R-leem")


    #Variables
    size <- argaddit$size
    prob <- argaddit$prob

    #Graphic
    minimo <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
    if (minimo < 0) minimo <- 0 else minimo <- round(minimo)
    maximo <- if (q > size) ceiling(q + 5) else size

    x <- minimo:maximo

    fx <- dbinom(x, size = size, prob = prob)

    xlim <- c(minimo, maximo)
    ylim <- c(0, max(fx) * 1.2)

    plot.new()
    plot.window(xlim, ylim)

    axis(1, at = seq(0, maximo + 1, by = 5))
    axis(2)

    if (!any(names(argaddit) == "main")){
      main <- substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}),
                        list(t = q, t2 = q + 1))
                      }
    title(ylab = expression(p[X](x)), xlab = "X", main = main)

    lines(x, fx, type = "h", lwd = 2, panel.first = grid(col = "gray90"))

    points(x, fx, pch = 19, lwd = 2, col = "#99ccff")

    p <- dbinom(q, size = size, prob = prob)
    pp <- round(dbinom(q, size = size, prob = prob), digits = 2)

    segments(par("usr")[1], p, q, lty = 2, col = "red", lwd = 2)

    lines(q, p, type = "h", lwd = 2, col = "red")

    points(q, p, pch = 19, col = "red", cex = 1.5, lwd = 2)

    auxposh <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
    auxposv <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 30

    axis(side = 1, at = q, labels = FALSE, col = "#880000", lwd.ticks = 2, tick = TRUE)
    axis(side = 1, at = q, labels = substitute(x == q, list(p = p, q = q)), col = "#880000",col.axis = "#880000", font = 2, lwd = 1, tick = FALSE, pos = auxposh)

    axis(side = 2, at = p, labels = FALSE, col = "red", lwd.ticks = 2, tick = TRUE)
    axis(side = 2, at = p, labels = substitute("p"[x](q) == p, list(p = pp, q = q)), col = "red", col.axis = "red", font = 2, lwd = 1, tick = FALSE, pos = auxposv)

    legend("topleft", cex = 0.9, box.col = "black", bg = "#e0e0e0",
           legend = substitute(bold("Parameters:"~size ==  sizex ~","~prob == probx),
                               list(sizex = size, probx = prob)))
    #Result
    prob <- dbinom(q, size = size, prob = prob)
  }
  #Return
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}
