#' @export

PF <- function(q, dist = "normal", rounding = 5, porcentage = FALSE, gui = "plot", main = NULL, plot = TRUE, ...){
  # Arguments in '...'
  argaddit <- list(...)
  # Formal arguments
  argdef <- formals(PF)
  if(dist == "normal"){
    #Security
    if (!any(names(argaddit) == "mean")) {
      mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
      argaddit$mean <- as.numeric(mean)
    }

    if (!any(names(argaddit) == "sd")) {
      sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
      argaddit$sd <- as.numeric(sd)
    }

    if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")

    #Variables
    mu <- argaddit$mean
    sigma <- argaddit$sd

    #Graphic
    minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
    maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma

    x <- seq(minimo, maximo, by = 0.01)

    fx <- dnorm(x, mean = mu, sd = sigma)

    if (!any(names(argaddit) == "main")){
      main <- substitute(atop(bold("Probability Function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2), list(t1 = q, x = "x"))
    }

    curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
          ylim = c(0, 1.2*max(fx)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main)

    polygon(c(x, rev(x)),
            c(fx, rep(0, length(fx))),
            col="#99ccff")

    p <- round(dnorm(q, mean = mu, sd = sigma), digits = 2)

    segments(par("usr")[1], p, q, lty = 2, col = "red", lwd = 2)
    segments(q, 0, q, p, lty = 2, col = "#880000", lwd = 2)

    points(q, p, pch = 16, col = "red", cex = 1.5)

    auxposh <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
    auxposv <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 30


    axis(side = 1, at = q, labels = FALSE, col = "#880000", lwd.ticks = 2, tick = TRUE)
    axis(side = 1, at = q, labels = substitute(x == q, list(p = p, q = q)), col = "#880000",col.axis = "#880000", font = 2, lwd = 1, tick = FALSE, pos = auxposh)

    axis(side = 2, at = p, labels = FALSE, col = "red", lwd.ticks = 2, tick = TRUE)
    axis(side = 2, at = p, labels = substitute(f[x](q) == p, list(p = p, q = q)), col = "red", col.axis = "red", font = 2, lwd = 1, tick = FALSE, pos = auxposv)

    legend("topleft", cex = 0.9, box.col = "black", bg = "#e0e0e0",
           legend = substitute(bold("Parameters:"~mu ==  mean ~ "," ~ sigma == varen),
                               list(mean = mu, varen = sigma)))
    #Result
    prob <- dnorm(q, mean = mu, sd = sigma)
  }
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
    maximo <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))

    x <- minimo:maximo

    fx <- dbinom(x, size = size, prob = prob)

    xlim <- c(minimo, maximo)
    ylim <- c(min(x), max(fx) * 1.2)

    plot.new()
    plot.window(xlim, ylim)

    axis(1, 0:maximo)
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
