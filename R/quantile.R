#' Quantile distribution function.
#'
#' \code{Q} Quantile function for multiple distributions.
#'
#' @param p probability. The \code{p} argument need have length 1 and value lower then 1.
#' @param dist distribution to use. The default is \code{'t-student'}. Options: \code{'normal'}, \code{'t-student'}, \code{'gumbel'}, \code{'binomial'}, \code{'poisson'}, and ....
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}. This argument is valid only if \code{q} has length 1.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{'plot'} and....
#' @param ... additional arguments according to the chosen distribution.
#' @details The expression of quantile function is given by:
#' \deqn{
#' Q(p)=\inf {x\in \mathbb{R}: p \leq F(x)},
#' }
#' where \code{p} is the first argument of \code{Q()} and \code{x} its return value;
#' @return \code{Q} returns the point and its graphical representation. The result is give by a integer number.
#'
#' @examples
#' # Loading package
#' library(leem)
#' \dontrun{
#' Q(p = 0.8, dist = "normal", mean = 200, sd=30)
#' }
#' @import manipulate
#' @import tkRplotR
#  @import shiny
#' @export

Q <- function(p, dist = "t-student", lower.tail = TRUE, rounding = 2, gui = "plot", mfrow = c(1, 2), type = "both", ...) {
  if (p>1) stop("The 'p' argument are very large, please insert a value correct for probabilities!", call. = FALSE)
  argaddit <- list(...)
  argdef <- formals(Q)
  if (dist == "normal") {
    if (!any(names(argaddit) == "mean")) {
      mean <- readline(gettext("Insert the 'mean' argument: ", domain = "R-leem"))
      argaddit$mean <- as.numeric(mean)
    }
    if (!any(names(argaddit) == "sd")) {
      sd <- readline(gettext("Insert the 'sd' argument: ", domain = "R-leem"))
      argaddit$sd <- as.numeric(sd)
    }
    if (lower.tail) {
      # Plot function - Type I
      plotcurve <- function(p, mu, sigma) {
        x <- qnorm(p, mu, sigma)
        curve(pnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma, ylab = expression(F[X](x)),
              ylim = c(0, 1.2), xlab = "X",panel.first = grid(col = "gray90"), main = gettext("Quantile Function: Normal", domain = "R-leem"),
              lwd = 4)
        x <- seq(mu - 4 * sigma, x[1], by = 0.01)
        y <- seq(x[1], mu + 4 * sigma,by = 0.01)
        fx <- pnorm(x, mu, sigma)
        fy <- pnorm(y, mu, sigma)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "gray90"
        )
        # polygon(c(x, rev(x)),
        #         c(fx, rep(0, length(fx))),
        #         col = "red"
        # )
        #abline(v = mu, lty = 2)
        qq <- round(p, digits = rounding)
        qqaux <- round(qnorm(p,mu,sigma), digits = rounding)
        # Pr <- gsub("\\.", ",", Pr)
        # qq <- gsub("\\.", ",", qq)
        aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
        axis(side = 1, at = qqaux, labels = qqaux, col.axis = "red", font = 2, pos = aux2, lwd.ticks = 0)
        axis(side = 1, at = qqaux, labels = FALSE, col.axis = "red", col = "red", font = 2, tick = TRUE, lwd.ticks = 1)
        # auxiliar variable
        aux <- par("usr")[1]-(par("usr")[2] - par("usr")[1])/20
        axis(side = 2, at = qq, labels = qq, col.axis = "red", font = 2, pos = aux, lwd.ticks = 0)
        axis(side = 2, at = qq, labels = FALSE, col.axis = "red", col = "red", font = 2, tick = TRUE, lwd.ticks = 1)


        segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
        segments(par("usr")[1], qq, qqaux, qq, lty = 2, col = "red")
        points(qqaux, qq, pch = 16, col = "red")
        # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
        legend("topleft", bty = "n", col = "red", pch = 16,
               legend = substitute(Q(p==p1 ~"; " ~ mu == media ~ ","~ sigma == varen) == Qr ~ "\n\n",
                                   list(p = "p", p1 = qq, Qr = qqaux, media = mu, varen = sigma))
        )
      }
      # Auxiliar variables
      q <- qnorm(p, argaddit$mean, argaddit$sd)
      minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
      maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
      # Plot function
      plotcurve2 <- function(q, mu, sigma) {
        minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
        maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
        x <- seq(minimo, q, by = 0.01)
        y <- seq(q, maximo, by = 0.01)
        fx <- dnorm(x, mean = mu, sd = sigma)
        fy <- dnorm(y, mean = mu, sd = sigma)
        if (!any(names(argaddit) == "main")) {
          main <- gettext("Distribution Function: Normal", domain = "R-leem")
        } else {
          main <- argaddit$main
        }
        curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
              ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
              panel.first = grid(col = "gray90"),
              main = main)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        # Insert vertical line over the mean
        abline(v=mu, lty=2)
        qq <- round(q, digits=rounding)
        qqaux <-round(q, digits=rounding)
        Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE), digits=rounding)
        #Pr <- gsub("\\.", ",", Pr)
        #qq <- gsub("\\.", ",", qq)
        # Insert red q point and vertical line (X-axis)
        aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
        # Insert red horizontal and vertical line (X-axis)
        axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
             col="red", font = 2, lwd.ticks = 1, labels = FALSE)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(F[X](X<= ~ q ~ ";" ~ mu ==  mean ~ "," ~ sigma == varen)==Pr, list(q = qq, Pr = Pr, mean = mu, varen = sigma)))
      }
      plotcurve2aux <- function(p, mu, sigma) {
        q <- qnorm(p, mu, sigma)
        plotcurve2(q, mu, sigma)
      }
      plotcurveaux <- function(p, mu, sigma) {
        op <- par(mfrow = mfrow)
        plotcurve(p, mu, sigma)
        q <- qnorm(p, mu, sigma)
        plotcurve2(q, mu,sigma)
        # Preserving the global variable
        par(op)
      }

      if (type == "both") {
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotcurveaux(p, mu, sigma)
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotcurveaux(p, mean, sd),
                                 p = manipulate::slider(0.001, 0.999, p),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotcurve(p, mu, sigma)
          #plotcurve2(qnorm(p, mu, sigma), mu,sigma)
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotcurve(p, mean, sd),
                                 p = manipulate::slider(0.001, 0.999, p),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          #plotcurve(p, mu, sigma)
          plotcurve2aux(p, mu,sigma)
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotcurve2aux(p, mean, sd),
                                 p = manipulate::slider(0.001, 0.999, qnorm(p, mu, sigma)),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
      }
      point <- qnorm(p, mean = mu, sd = sigma)
    }
    else {
      plotcurve <- function(p, mu, sigma) {
        x<-qnorm(p,mu,sigma)
        curve(pnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma, ylab = expression(F[X](x)),
              xlab = "X",panel.first = grid(col = "gray90"), main = gettext("Quantitative Function: Normal.", domain = "R-leem"))
        x <- seq(mu - 4 * sigma, x[1], by = 0.01)
        y <- seq(x[1], mu + 4 * sigma,by = 0.01)
        fx <- pnorm(x, mu, sigma)
        fy <- pnorm(y, mu, sigma)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "red"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "gray90"
        )
        abline(v = argaddit$mean, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(qnorm(p,mu,sigma), digits = rounding)
        Pr <- round(qnorm(qq, mean = mu, sd = sigma, lower.tail = T), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2)
        axis(side = 1, at = as.character(c(qqaux, mu + 4 * sigma)),
             labels = c(qqaux,""), lwd.ticks = 0 , col = "red", font = 2, col.axis = "red")
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",
               legend = substitute(Q("P=" ~ p ~"; " ~ mu == media ~ "; "~ sigma == varen)~">"~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, media = mu, varen = sigma))
        )
      }
      if (gui == "plot") {
        mu <- argaddit$mean
        sigma <- argaddit$sd
        point <- qnorm(p, mean = mu, sd = sigma)
        plotcurve(p, mu, sigma)
      }
    }
  }
  if (dist == "gumbel") {
    if (!any(names(argaddit) == "location")) {
      location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
      argaddit$location <- as.numeric(location)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }
    if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                   call. = FALSE, domain = "R-leem")
    loca <- argaddit$location
    sca <- argaddit$scale
    if (lower.tail) {
      xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
      xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
        xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
        xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
      }
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
        xvq <- 10*abs(loca)
        xvq1 <- -10*abs(loca)
      }
      plotcurve <- function(p, location,scale) {
        q <- qgumbel(p,loca,sca,lower.tail = TRUE)
        curve(pgumbel(x,location,scale,lower.tail = TRUE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
              ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
              main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
        x <- seq(xvq1, q[1], by = 0.01)
        y <- seq(q[1], xvq,by = 0.01)
        fx <- pgumbel(x, location,scale)
        fy <- pgumbel(y, location, scale)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "gray90"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "red"
        )
        abline(v = location, lty = 2,col = "black")
        pp <- round(p, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(q, digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", pp)
        axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
        axis(side = 1, at = as.character(c(qqaux,xvq1)),labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.8,
               legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ "<=" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, media = location, varen = scale)))
      }
      if (gui == "plot") {
        point <- qgumbel(p,loca,sca,lower.tail = TRUE)
        plotcurve(p, loca, sca)
      }
      if (gui == "rstudio") {
        loca <- argaddit$location
        sca <- argaddit$scale
        manipulate::manipulate(plotcurve(p, loca, sca),
                               p = manipulate::slider(0.01,0.9, p),
                               loca = manipulate::slider(xvq1,xvq, loca ),
                               sca = manipulate::slider(1,xvq, sca))
        point <- qgumbel(p,loca,sca,lower.tail = TRUE)
      }
    }
    else {
      xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
      xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
        xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
        xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
      }
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
        xvq <- 10*abs(loca)
        xvq1 <- -10*abs(loca)
      }
      plotcurve <- function(p,location, scale) {
        q <- qgumbel(p,location,scale,lower.tail = FALSE)
        curve(pgumbel(x,location,scale,lower.tail = FALSE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
              ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
              main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
        x <- seq(xvq1, q, by = 0.01)
        y <- seq(q, xvq,by = 0.01)
        fx <- pgumbel(x, location, scale,lower.tail = FALSE)
        fy <- pgumbel(y, location, scale,lower.tail = FALSE)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "red"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "gray90"
        )
        abline(v = location, lty = 2, col="black")
        pp <- round(p, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(q, digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", pp)
        axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
        axis(side = 1, at = as.character(c(qqaux,xvq)),labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.8,
               legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ ">" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, media = location, varen = scale)))
      }
      if (gui == "plot") {
        point <- qgumbel(p,loca, sca,lower.tail = FALSE)
        plotcurve(p, loca,sca)
      }
      if (gui == "rstudio") {
        loca <- argaddit$location
        sca <- argaddit$scale
        manipulate::manipulate(plotcurve(p, loca, sca),
                               p = manipulate::slider(0.01,0.9, p),
                               loca = manipulate::slider(xvq1,xvq, loca ),
                               sca = manipulate::slider(1,xvq, sca))
        point <- qgumbel(p,loca,sca,lower.tail = FALSE)
      }
    }
  }######ADD FUNÇÃO Q.
  if (dist == "poisson") {
      if (!any(names(argaddit) == "lambda")) {
        lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
        argaddit$lambda <- as.numeric(lambda)
      }
      if (lower.tail) {
      plotcurve <- function(p, lambda) {
        rmin <- 0
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
        rmax <- ceiling(lambda + 4 * sqrt(lambda))
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- p[1]:rmax
        pointx <- ppois(x, lambda = lambda)
        pointx1 <- ppois(x1, lambda = lambda)
        pointx2 <- ppois(x2, lambda = lambda)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Poisson.", domain = "R-leem"))
        lines(x2, pointx2, type = "h", lwd = 2)
        points(x2, pointx2, lwd = 2, pch = 19)
        lines(x1, pointx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
        points(x1, pointx1, lwd = 2, col = "red", pch = 19)
        abline(v = lambda, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <-round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmin)), labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~ Pr ~";" ~ lambda == lambd )~"<="~ p ~ "\n\n",
                                   list(p = qq, Pr = Pr, lambd = lambda))
        )
      }
      if (gui == "plot") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        plotcurve(point, lambda)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        manipulate::manipulate(plotcurve(point, lambda),
                               p = manipulate::slider(0, p + 30, p),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda)
        )
      }
    } else {
      plotcurve <- function(p, lambda) {
        rmin <- 0
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
        rmax <- ceiling(lambda + 4 * sqrt(lambda))
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- p[1]:rmax
        pointx <- ppois(x, lambda = lambda)
        pointx1 <- ppois(x1, lambda = lambda)
        pointx2 <- ppois(x2, lambda = lambda)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Poisson.", domain = "R-leem"))
        lines(x2, pointx2, type = "h", lwd = 2, col = "red")
        points(x2, pointx2, lwd = 2, col = "red", pch = 19)
        lines(x1, pointx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
        points(x1, pointx1, lwd = 2, pch = 19)
        abline(v = lambda, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmax)),labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~ Pr ~";" ~ lambda == lambd )~">"~ p ~ "\n\n",
                                   list(p = qq, Pr = Pr, lambd = lambda))
        )
      }
      if (gui == "plot") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        plotcurve(point, lambda = lambda)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        manipulate::manipulate(plotcurve(point, lambda),
                               p = manipulate::slider(p, p + 30, p),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda)
        )
      }
    }
    }
  if (dist == "beta") {
    if (!any(names(argaddit) == "alpha")){
      alpha <- readline(expression("Insert the 'alpha' argument: ",domain = "R-leem"))
      argaddit$alpha <- as.numeric(alpha)
    }
    if (!any(names(argaddit) == "beta")){
      beta <- readline(expression("Insert the 'beta' argument: ",domain = "R-leem"))
      argaddit$beta <- as.numeric(beta)
    }
    shape1 <- argaddit$alpha
    shape2 <- argaddit$beta
    if (lower.tail) {
      plotcurve <- function(p, shape1, shape2) {
        x <- qbeta(p, shape1, shape2)
        curve(pbeta(x, shape1, shape2), 0, 1, ylab = expression(F[X](x)),
              xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
        x <- seq(0, x[1], by = 0.01)
        y <- seq(x[1], 1, by = 0.01)
        fx <- pbeta(x, shape1, shape2)
        fy <- pbeta(y, shape1, shape2)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "gray90"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "red"
        )
        abline(v = shape1, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
        Pr <- round(qbeta(qq,  shape1 , shape2 , lower.tail = T), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 1 , col = "red", font = 2)
        abline(v = max(x), lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",
               legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1   ~ "; "~ beta == beta1) ~ "<=" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
        )
      }
      if (gui == "plot") {

        point <- qbeta(p, shape1 ,  shape2)
        plotcurve(p, shape1 , shape2)
      }
    }
    else {
      plotcurve <- function(p, shape1, shape2) {
        x<-qbeta(p, shape1, shape2)
        curve(pbeta(x, shape1, shape2 ),0, 1, ylab = expression(F[X](x)),
              xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
        x <- seq(0 , x[1], by = 0.01)
        y <- seq(x[1], 1, by = 0.01)
        fx <- pbeta(x, shape1, shape2)
        fy <- pbeta(y, shape1, shape2)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "red"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "gray90"
        )
        abline(v = argaddit$alpha, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
        Pr <- round(qbeta(qq, shape1, shape2, lower.tail = T), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = max(x), labels = max(x), col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(max(x), 1)),labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
        abline(v = max(x), lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",
               legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1 ~ "; "~ beta == beta1) ~">" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
        )
      }
      if (gui == "plot") {
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta
        point <- qbeta(p, shape1, shape2)
        plotcurve(p, shape1, shape2)
      }

    }
  }
  if (dist == "nbinom") {
    size <- argaddit$size
    prob <- argaddit$prob
    # Seguranças da distribuição nbinom .
    if (!any(names(argaddit) == "prob")) stop("Insira o argumento 'size'!", call. = FALSE)
    if (!any(names(argaddit) == "size")) stop("Insira o argumento 'prob'!", call. = FALSE)
    if (prob == 0) stop("Scale tem que ser difrente de 0!", call. = FALSE)
    # Cauda verdadeira.
    if (lower.tail) {
      # variação de valores para o eixo x;
      xvq <- 2*size
      xvq1 <- 2*size
      if ( size >= 0) {
        xvq <- 2*size
        xvq1 <- -2*size
      }
      if ( size == 0 ) {
        xvq <- 10
        xvq1 <- -10
      }
      # função plot.
      plotcurve <- function(p, s, pro){
        # Criando  o quantili.
        q <- qnbinom(p,s,pro,lower.tail = TRUE)
        x <- xvq1:xvq
        fx <- pnbinom(x,s,pro)
        xlim <- c(xvq1, xvq)
        ylim <- c(min(fx),max(fx)+(max(fx)/3))
        # plot
        plot.new()
        plot.window(xlim, ylim)
        # Eixos
        axis(1)
        axis(2)
        # Título e labels
        title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
        # prlote das linhas
        x1 <- xvq1:q
        fx1 <- pnbinom(x1,s,p)
        lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, fx1, lwd = 2, col = "red", pch = 10)
        x2 <- (q+1):xvq
        fx2 <- pnbinom(x2,s,p)
        lines(x2, fx2, type = "h", lwd = 2)
        points(x2, fx2, lwd = 2, pch = 10)
        qq <- round(q, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(p, rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.9,
               legend = substitute(Q(P ~ '<' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
      }
      # Configurando plotagem padrão.
      if (gui == "plot") {
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = TRUE)
        # Plotagem.
        plotcurve(p,size,prob)
      }
      if (gui == "rstudio") {
        #plotagem no rstudio
        manipulate::manipulate(plotcurve(p,size,prob),
                               p = manipulate::slider(0.01,0.9, p),
                               size = manipulate::slider(xvq1,xvq, size ),
                               prob = manipulate::slider(0.1,1,prob))
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = TRUE)
      }
    }
    # Caso Cauda Falsa.
    else {
      # variação de valores para o eixo x;
      xvq <- 2*size
      xvq1 <- 2*size
      if ( size >= 0) {
        xvq <- 2*size
        xvq1 <- -2*size
      }
      if ( size == 0 ) {
        xvq <- 10
        xvq1 <- -10
      }
      # função plot.
      plotcurve <- function(p, s, pro){
        # Criando  o quantili.
        q <- qnbinom(p,s,pro,lower.tail = FALSE)
        x <- xvq1:xvq
        fx <- pnbinom(x,s,pro, lower.tail = FALSE)
        xlim <- c(xvq1, xvq)
        ylim <- c(min(fx),max(fx)+(max(fx)/3))
        # plot
        plot.new()
        plot.window(xlim, ylim)
        # Eixos
        axis(1)
        axis(2)
        # Título e labels
        title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
        # prlote das linhas
        x1 <- xvq1:q
        fx1 <- pnbinom(x1,s,p,lower.tail = FALSE)
        lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, fx1, lwd = 2, col = "red", pch = 10)
        x2 <- (q+1):xvq
        fx2 <- pnbinom(x2,s,p,lower.tail = FALSE)
        lines(x2, fx2, type = "h", lwd = 2)
        points(x2, fx2, lwd = 2, pch = 10)
        qq <- round(q, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(p, rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.9,
               legend = substitute(Q(P ~ '>=' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
      }
      # Configurando plotagem padrão.
      if (gui == "plot") {
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = FALSE)
        # Plotagem.
        plotcurve(p,size,prob)
      }
      if (gui == "rstudio") {
        #plotagem no rstudio
        manipulate::manipulate(plotcurve(p,size,prob),
                               p = manipulate::slider(0.01,0.9, p),
                               size = manipulate::slider(xvq1,xvq, size ),
                               prob = manipulate::slider(0.1,1,prob))
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = FALSE)
      }
    }
  }############ INCORRECT
  if (dist == "geometric") {
    if (!any(names(argaddit) == "prob")){
      prob <- readline(expression("Insert the 'prob' argument: ",domain = "R-leem"))
      argaddit$prob <- as.numeric(prob)
    }
    if (lower.tail){
      plotcurve <- function(p, prob) {
        rmin <- -10*p
        rmax <- 10*p
        if (rmin < 0) rmin <- 0
        rmax <- ceiling(10*p)
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- (p + 1):rmax
        pointx <- pgeom(x, prob = prob)
        pointx1 <- pgeom(x1, prob = prob)
        pointx2 <- pgeom(x2, prob = prob)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
        lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, pointx1, lwd = 2, col = "red", pch = 19)
        lines(x2, pointx2, type = "h", lwd = 2)
        points(x2, pointx2, lwd = 2, pch = 19)
        abline(v = p, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <-round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmin)), lwd.ticks = 1,labels = F, col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~"<=" ~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
        )
      }
      if (gui == "plot") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        plotcurve(point, prob)
      }
      if (gui == "rstudio") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        manipulate::manipulate(plotcurve(point, prob),
                               p = manipulate::slider(0, p + 30, p),
                               prob = manipulate::slider(prob, prob + 200, prob)
        )
      }
    } else {
      plotcurve <- function(p, prob) {
        rmin <- -10*p
        rmax <- 10*p
        if (rmin < 0) rmin <- 0
        rmax <- ceiling(10*p)
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- (p + 1):rmax
        pointx <- pgeom(x, prob = prob)
        pointx1 <- pgeom(x1, prob = prob)
        pointx2 <- pgeom(x2, prob = prob)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
        lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2)
        points(x1, pointx1, lwd = 2, pch = 19)
        lines(x2, pointx2, type = "h", lwd = 2, col = "red")
        points(x2, pointx2, lwd = 2, col = "red", pch = 19)
        abline(v = prob, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmax)),lwd.ticks = 0 ,labels = F, col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~">"~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
        )
      }
      if (gui == "plot") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        plotcurve(point, prob = prob)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        manipulate::manipulate(plotcurve(point, lambda),
                               p = manipulate::slider(p, p + 30, p),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda)
        )
      }
    }
  }
  if (dist == "t-student"){
    nu <- argaddit$df
    if (!any(names(argaddit) == "df")) {
      df <- readline("Insert the value of degree of freedom (df): ")
      argaddit$df <- as.numeric(df)
    }
    if (lower.tail) {
      plotcurve <- function(p, nu) {
        x <- qt(p,nu)
        x <- seq(-6, x[1], by=0.01)
        y <- seq(x[1], 6, by=0.01)
        fx <- pt(x, df = nu)
        fy <- pt(y, df = nu)
        curve(pt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
              xlab="T", ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray"),
              main = gettext("Quantitative Function: T-Student.", domain = "R-leem"))
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        abline(v=0, lty=2)
        qq <- round(p, digits=2)
        qqaux <-round(qt(p,nu), digits=4)
        Pr <- round(qt(p, df = nu, lower.tail = T), digits=4)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, , labels = qqaux, tick = TRUE, lwd = 0,
             col="red", font = 2, lwd.ticks = 1, col.axis="red")
        axis(side=1, at=as.character(c(-6, qqaux)), tick = TRUE, lwd = 1,
             col="red", font = 2, lwd.ticks = 0, labels = FALSE)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(Q("P=" ~ p ~";"~ df == nu)~"<="~Pr~"\n\n", list(p = qq, Pr = Pr, nu = nu)))
      }
      if (gui == "plot" ) {
        nu <- argaddit$df
        point <- qt(p, df = nu)
        plotcurve(p, nu)
      }
      if (gui == "rstudio") {
        nu <- argaddit$df
        manipulate::manipulate(plotcurve(qaux, nuaux),
                               qaux = manipulate::slider(-6, 6, p),
                               nuaux = manipulate::slider(1, 200, nu))
        point <- qt(p, df = nu)
      }
    }
    else {
      #options(warn = - 1)
      war <- options(warn = - 1)
      on.exit(options(war))
      plotcurve <- function(p, nu) {
        x <- qt(p,nu)
        x <- seq(x[1], 6, by=0.01)
        y <- seq(-6, x[1], by=0.01)
        fx <- pt(x, df = nu)
        fy <- pt(y, df = nu)
        curve(pt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
              xlab="T", ylim = c(0, 1.2 * max(c(fx,fy))), panel.first = grid(col = "gray"),
              main = gettext("Quantitative Function: T-Student.", domain = "R-leem"))
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(p, digits=2)
        qqaux <-round(qt(p,nu, lower.tail = F), digits= 4) *-1
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels = qqaux, col.axis = "red", tick = TRUE, lwd = 0,
             col="red", font = 2, lwd.ticks = 1)
        axis(side=1, at=as.character(c(qqaux, 6)), tick = TRUE, lwd = 1,
             col="red", font = 2, lwd.ticks = 0, labels = FALSE)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(Q("P=" ~ p ~";"~ df == nu)~">"~Pr~"\n\n", list(p = qq, Pr = qqaux, nu = nu)))
      }
      if (gui == "plot") {
        nu <- argaddit$df
        point <- qt(p, df = nu, lower.tail = F) *-1
        plotcurve(p, nu)
      }
      if (gui == "rstudio") {
        nu <- argaddit$df
        manipulate::manipulate(plotcurve(p, df),
                               p = manipulate::slider(-6, 6, p),
                               df = manipulate::slider(1, 200, nu))
        point <- qt(p, nu)
      }
    }
  }
  if (dist == "binomial") {
    if (!any(names(argaddit) == "size")){
      size <- readline(expression("Insert the 'size' argument: ",domain = "R-leem"))
      argaddit$size <- as.numeric(size)
    }
    if (!any(names(argaddit) == "prob")){
    prob <- readline(expression("Insert the 'prob' argument: ",domain = "R-leem"))
    argaddit$prob <- as.numeric(prob)
  }
    size <- argaddit$size
    sucesso <- argaddit$prob
    if (lower.tail) {
      plotcurve <- function(p, size, prob) {
        q <- qbinom(p, size, prob)
        rmin <- 0
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
        x <- rmin:size
        x1 <- rmin:q
        x2 <- (q + 1):size
        probx <- dbinom(x, size = size, prob = prob)
        probx1 <- dbinom(x1, size = size, prob = prob)
        probx2 <- dbinom(x2, size = size, prob = prob)
        xlim <- c(rmin, size)
        ylim <- c(min(probx), max(probx) + 0.1)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X",
              main = gettext("Quantitative Function: Binomial.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
        points(x1, probx1, lwd = 2, col = "red", pch = 19)
        lines(x2, probx2, type = "h", lwd = 2)
        points(x2, probx2, lwd = 2, pch = 19)
        abline(v = size * prob, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qbinom(p, size = size, prob = prob, lower.tail = T), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(rmin, qqaux), labels = c("", qqaux),
          col = "red", font = 2, col.axis = "red"
        )
        axis(
          side = 1, at = qqaux, labels = TRUE, lwd.ticks = 1,
          col = "red", font = 2, col.axis= "red"
        )
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~ py ~"; " ~ size == n~"; " ~ p == prob)~"<="~Pr ~ "\n\n" ,
                                   list(py = p, Pr = Pr, n = size, prob = prob))
        )
      }
      if (gui == "plot") {
        point <- qbinom(p, size = size, prob = sucesso)
        plotcurve(p, size, prob = sucesso)
      }
    }
    else {
      plotcurve <- function(q, size, prob) {
        q <- qbinom(p, size, prob, lower.tail = F)
        rmin <- size * prob - 4 * sqrt(size * prob * (1 - prob))
        if (rmin < 0 || rmin>q) rmin <- 0 else rmin <- round(rmin)
        x <- rmin:size
        x1 <- rmin:q
        x2 <- (q + 1):size
        probx <- dbinom(x, size = size, prob = prob)
        probx1 <- dbinom(x1, size = size, prob = prob)
        probx2 <- dbinom(x2, size = size, prob = prob)
        xlim <- c(rmin, size)
        ylim <- c(min(probx), max(probx) + 0.1)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X",
              main = gettext("Quantitative Function: Binomial.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
        points(x1, probx1, lwd = 2, pch = 19)
        lines(x2, probx2, type = "h", lwd = 2, col = "red")
        points(x2, probx2, lwd = 2, pch = 19, col = "red")
        abline(v = size * prob, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qbinom(p, size = size, prob = prob, lower.tail = F), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(qqaux, size), labels = c(qqaux, ""), lwd.ticks = 0,
          col = "red", font = 2, col.axis= "red"
        )
        axis(
          side = 1, at = qqaux, labels = TRUE, lwd.ticks = 1,
          col = "red", font = 2, col.axis= "red"
        )
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~ py ~"; " ~ size == n~"; " ~ p == prob)~">"~Pr ~ "\n\n" ,
                                   list(py = p, Pr = Pr, n = size, prob = prob))
        )
      }
      if (gui == "plot") {
        point <- qbinom(p, size = size, prob = sucesso, lower.tail = F)
        plotcurve(p, size, prob = sucesso)
      }
    }
  }
  if (dist == "hyper") {
    if (!any(names(argaddit) == "m")){
      m <- readline(expression("Insert the 'm' argument: ",domain = "R-leem"))
      argaddit$m <- as.numeric(m)
    }
    if (!any(names(argaddit) == "n")){
      n <- readline(expression("Insert the 'n' argument: ",domain = "R-leem"))
      argaddit$n <- as.numeric(n)
    }
    if (!any(names(argaddit) == "k")){
      k <- readline(expression("Insert the 'k' argument: ",domain = "R-leem"))
      argaddit$k <- as.numeric(k)
    }
    size <- argaddit$m
    samples <- argaddit$n
    sucess <- argaddit$k
    if (lower.tail) {
      plotcurve <- function(p, size, samples, sucess) {
        q <- qhyper(p, m = size, n = samples, k = sucess)
        rmin <- 0
        x <- rmin:size
        x1 <- rmin:q
        x2 <- (q + 1):size
        probx <- dhyper(x, m = size, n = samples, k = sucess)
        probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
        probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
        xlim <- c(rmin, size)
        ylim <- c(min(probx), max(probx) + 0.1)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
        points(x1, probx1, lwd = 2, col = "red", pch = 19)
        lines(x2, probx2, type = "h", lwd = 2)
        points(x2, probx2, lwd = 2, pch = 19)
        abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qhyper(p, m = size, n = samples, k = sucess), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(rmin, qqaux), labels = c("", qqaux),
          col = "red", font = 2, col.axis = "red"
        )
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~"<="~ Pr ~ "\n\n",
                                   list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
        )
      }
      if (gui == "plot") {
        point <- qhyper(p, m = size, n = samples, k = sucess)
        plotcurve(p, size, samples, sucess)
      }
    }
    else {
      plotcurve <- function(p, size, samples, sucess) {
        q <- qhyper(p, m = size, n = samples, k = sucess)
        rmin <- 0
        if (rmin < 0 || rmin > q) rmin <- 0 else rmin <- round(rmin)
        x <- rmin:size
        x1 <- rmin:q
        x2 <- (q + 1):size
        probx <- dhyper(x, m = size, n = samples, k = sucess)
        probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
        probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
        xlim <- c(rmin, size)
        ylim <- c(min(probx), max(probx) + 0.1)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
        points(x1, probx1, lwd = 2, pch = 19)
        lines(x2, probx2, type = "h", lwd = 2, col = "red")
        points(x2, probx2, lwd = 2, pch = 19, col = "red")
        abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qhyper(p, m = size, n = samples, k = sucess, lower.tail = F), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(qqaux, size), labels = FALSE,
          col = "red", font = 2, col.axis = "red", lwd.ticks= 0
        )
        axis(
          side = 1, at = qqaux, labels = qqaux,
          col = "red", font = 2, col.axis = "red"
        )

        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~">"~ Pr ~ "\n\n",
                                   list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
        )
      }
      if (gui == "plot") {
        point <- qhyper(p, m = size, n = samples, k = sucess, lower.tail = F)
        plotcurve(p, size, samples, sucess)
      }
    }
  }
  point <- round(point, rounding)
  return(point)
}
