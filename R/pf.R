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

  #Return
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}
