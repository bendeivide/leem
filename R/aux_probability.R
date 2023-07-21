###########################
## Auxiliar functions of P()
###########################
# Observations:
#    - `%<=X<=%`() internal function
################################################################################

################################################################################
## A-region (name: plot+p+name_distribution+ar+gui)
################################################################################
# OBS.: ar - A-region; gui: "plot", "rstudio", "tcltk"
#-------------------------------------------------------------------------------
#####################
# Normal distribution
#####################
# Plot
plotpnormalarplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fz <- dnorm(z,mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
        ylab = expression(f[X](X)),
        panel.first = grid(col="gray90"),
        main = main,
        cex=0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
          col="red" )
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pnorm(q[1], mean = mu,sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd=sigma, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
} # plotcurve (older)
# RStudio
plotpnormalarrstudio <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalarplot(q, mu, sigma, rounding, main)
}
# Tcl/tk
plotpnormalartcltk <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalarplot(q, mu, sigma, rounding, main)
}


########################
# T-Student distribution
########################
# Plot
plotptstudentarplot <- function(q, df, rounding, main = NULL){
  nu <- df
  # Auxiliar function
  llower <- if(abs(q[1]) > 6) abs(q[1] + 2) else 6
  lupper <- if(abs(q[2]) > 6) abs(q[2] + 2) else 6
  x <- seq(-llower, q[1], by=0.01)
  z <- seq(q[2], lupper, by=0.01)
  y <- seq(-llower, lupper, by=0.01)
  fx <- dt(x, df = nu)
  fz <- dt(z, df = nu)
  fy <- dt(y, df = nu)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }

  curve(dt(x, df = nu), -llower, lupper, ylab = expression(f[X](x)), xlab="X",
        ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
        main = main, cex = 0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z, rev(z)),
          c(fz, rep(0, length(fz))),
          col="red")
  qq <- round(q, digits=2)
  Pr <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(-llower, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], lupper)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qq, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
}
# RStudio
plotptstudentarrstudio <- function(q1,q2, df, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotptstudentarplot(q, df, rounding, main)
}
# Tcl/tk
## Soon...



######################
# Poisson distribution
######################
# Plot
plotppoissonarplot <- function(q, lambda, rounding, main = NULL){
  # readjusting the range
  ## ab-region
  if (is.double(q)) {
    if (attr(q, "region") == "region5") {
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region1") {
      q[1] <- q[1] - 1
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region6") {
      q[1] <- q[1] - 1
    }
    ## b-region
    if (attr(q, "region") == "region7") {
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region2") {
      q[1] <- q[1] + 1
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region8") {
      q[1] <- q[1] + 1
    }
    if (q[1] >= q[2]) stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
  }

  rmin <- if (q[1] < lambda) trunc(q[1] - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > lambda) ceiling(q[2] + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  points(x, probx, lwd = 2, pch = 19, panel.first = grid(col = "gray90"))
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  Pr <- round(ppois(q = q[1], lambda = lambda) + ppois(q = q[2] - 1, lambda = lambda, lower.tail = FALSE),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  lines(x1, probx1, type = "h", lwd = 2,col="red")
  points(x1, probx1, lwd = 2, pch = 19,col="red")
  lines(x2, probx2, type = "h", lwd = 2,col="red")
  points(x2, probx2, lwd = 2, pch = 19,col="red")
  # red x-axis
  # red x-axis
  axis(side=1, at=c(qqmin, qqmax), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(qqmax, rmax)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(x1), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(q), tick = TRUE, lwd = 0,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  # intervals
  abline(v = c(qqmin, qqmax), lty=2, col = "red")
  # rectangle
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  # title and legends
  if (qqmin < 0) {
    axis(side=1, at=as.character(qqmin), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")*","~~P(X <= t1)== 0*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~lambda == lambd,
                               list(lambd = lambda)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~lambda == lambd,
                               list(lambd = lambda)))
  }
}

# RStudio
plotppoissonarrstudio <- function(q1, q2, lambda, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotppoissonarplot(q,lambda, rounding, main)
}
# Tcl/tk
## Soon...


######################
# Binomial distribution
######################
# Plot
plotpbinomialarplot <- function(q, size, prob, rounding, main = NULL){
  # readjusting the range
  ## ab-region
  if (is.double(q)) {
    if (attr(q, "region") == "region5") {
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region1") {
      q[1] <- q[1] - 1
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region6") {
      q[1] <- q[1] - 1
    }
    ## b-region
    if (attr(q, "region") == "region7") {
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region2") {
      q[1] <- q[1] + 1
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region8") {
      q[1] <- q[1] + 1
    }
    if (q[1] >= q[2]) stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
  }

  rmin <- if (q[1] < size) trunc(q[1] - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > size) ceiling(q[2] + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  probx <- dbinom(x, size = size, prob = prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  points(x, probx, lwd = 2, pch = 19, panel.first = grid(col = "gray90"))
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  Pr <- round(pbinom(q = q[1], size = size, prob = prob) + pbinom(q = q[2] - 1, size = size, prob = prob, lower.tail = FALSE),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dbinom(x1, size = size, prob = prob)
  probx2 <- dbinom(x2, size = size, prob = prob)
  lines(x1, probx1, type = "h", lwd = 2,col="red")
  points(x1, probx1, lwd = 2, pch = 19,col="red")
  lines(x2, probx2, type = "h", lwd = 2,col="red")
  points(x2, probx2, lwd = 2, pch = 19,col="red")
  # red x-axis
  # red x-axis
  axis(side=1, at=c(qqmin, qqmax), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(qqmax, rmax)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(x1), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(q), tick = TRUE, lwd = 0,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  # intervals
  abline(v = c(qqmin, qqmax), lty=2, col = "red")
  # rectangle
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  # title and legends
  if (qqmin < 0) {
    axis(side=1, at=as.character(qqmin), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red",
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr, cex = 0.8,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                               list( N = size, P = prob)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                               list( N = size, P = prob)))
  }
}
#Rstudio
plotpbinomialarrstudio <- function(q1, q2, size, prob, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotpbinomialarplot(q, size, prob, rounding, main)
}
# Tcl/tk
## Soon...



##########################
# Chi-Squared distribution
##########################
# Plot
plotpchisqarplot <- function(q, df, ncp, rounding, main = NULL) {
  minimo <- if (q[1] <= ncp - 4 * df) ncp - 4 * df else 0
  maximo <- if (q[2] > ncp + 4 * df) q[2] + 4 * df else ncp + 4 * df
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fz <- dchisq(z, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  if(is.infinite(1.2 * max(fx,fy,fz))){
    auxmain <- c(0, 2.5 + df);
    auxrect <- c(2 + df, 3.5 + df)
  }else{
    auxrect <- max(fx,fy,fz)
    auxmain <- c(0, 1.2 * max(fx,fy,fz))
  }
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dchisq(x, df = df, ncp = ncp), minimo, maximo,
        ylim = auxmain,
        xlab="X",
        ylab = expression(f[X](x)),
        panel.first = grid(col="gray90"),
        main = main,
        cex=0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
          col="red" )
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pchisq(q[1], df = df, ncp = ncp, lower.tail = T) + pchisq(q[2], df = df, ncp=ncp, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.7,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
} # plotcurve (older)
# RStudio
plotpchisqarrstudio <- function(q1, q2, df, ncp, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqarplot(q, df, ncp, rounding, main)
}
# Tcl/tk
## Soon...




#####################
# F distribution
#####################
# Plot
plotpfarplot <- function(q, df1, df2, rounding, main = NULL) {
  minimo <- 0
  maximo <- 10
  if(q[2]>10){
    maximo <- q[2]+ 2*(df1/df2)
  }
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fz <- df(z, df1, df2)
  fy <- df(y, df1, df2)
  if(is.infinite(1.2 * max(fx,fy,fz))){
    auxmain <- c(0, 2.5 + (df1/df2));
    auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy,fz))
  }
  if (is.null(main)) {###Ajustar
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(df(x, df1, df2),
        minimo,
        maximo,
        ylim = auxmain,
        xlab="X",
        ylab = expression(f[X](X)),
        panel.first = grid(col="gray90"),
        main = main,
        cex.main=1)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)),
          c(fz,rep(0,length(fz))),
          col="red" )
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pf(q[1], df1, df2, lower.tail = T) + pf(q[2], df1, df2, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
} # plotcurve (older)
# RStudio
plotpfarrstudio <- function(q1, q2, df1, df2, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfarplot(q, df1, df2, rounding, main)
}

#####################
# Gumbel distribution
#####################
# Plot
plotpgumbelarplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
  maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fz <- dgumbel(z, location, scale)
  fy <- dgumbel(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dgumbel(x, location, scale), minimo, maximo,
        ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
        ylab = expression(f[X](X)),
        panel.first = grid(col="gray90"),
        main = main,
        cex=0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
          col="red" )
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pgumbel(q[1], location, scale, lower.tail = T) + pgumbel(q[2], location, scale, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
} # plotcurve (older)
# RStudio
plotpgumbelarrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgumbelarplot(q, location, scale, rounding, main)
}


####################
# Beta distribution
####################
plotpbetaarplot <- function(q, shape1, shape2, rounding, main = NULL) {

  x <- seq(0, q[1], by = 0.01)
  z <- seq(q[2],1, by = 0.01)
  y <-seq(q[1], q[2], by = 0.01)
  fx <- dbeta(x, shape1, shape2)
  fz <- dbeta(z, shape1, shape2)
  fy <- dbeta(y, shape1, shape2)

  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  if(is.infinite(1.2 * max(fx,fy,fz))){
    auxmain <- c(0, 2.5 + (shape1/shape2));
    auxrect <- c(2 + shape1/shape2, 3.5 + shape1/shape2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy,fz))
  }

  curve(dbeta(x, shape1, shape2), 0, 1,
        ylim = auxmain,
        xlab="X",
        ylab = expression(f[X](x)),
        panel.first = grid(col="gray90"), main = main)

  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
          col="red" )
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pbeta(q[1], shape1, shape2, lower.tail = T) + pbeta(q[2], shape1, shape2, lower.tail = F), digits=rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(0, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], 1)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                               list(alphav = shape1, betav = shape2)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                               list(alphav = shape1, betav = shape2)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                               list(alphav = shape1, betav = shape2)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                               list(alphav = shape1, betav = shape2)))
  }
}
####################
# Exponential
####################
plotpexparplot <- function(q, rate, rounding, main) {
  rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
  x1 <- seq(0, q[1], by = 0.01)
  x2 <- seq(q[2], rmax, by = 0.01)
  y <- seq(0, rmax, by = 0.01)
  probx1 <- dexp(x1, rate = rate)
  probx2 <- dexp(x2, rate = rate)
  proby <- dexp(y, rate = rate)

  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }

  if(is.infinite(1.2 *max(probx1, probx2, proby))){
    auxmain <- c(0, 2.5 + rate);
    auxrect <- c(2 + rate, 3.5 + rate)
  }else{
    auxrect <- max(probx1, probx2, proby)
    auxmain <- c(0, 1.2 * max(probx1, probx2, proby))
  }


  curve(dexp(x, rate), 0, rmax,
        ylab = expression(p[x](q)),
        xlab = "x", ylim = auxmain,
        panel.first = grid(col = "gray90"),
        main = main)
  polygon(c(y, rev(y)),
          c(proby, rep(0,length(proby))),
          col = "gray90")
  polygon(c(x1, rev(x1)),
          c(probx1, rep(0,length(probx1))),
          col = "red")
  polygon(c(x2, rev(x2)),
          c(probx2, rep(0,length(probx2))),
          col = "red")
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pexp(qq[1], rate = rate, lower.tail = T) +
                pexp(qq[2], rate = rate, lower.tail = F), rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)
  rate2 <- gsub("\\.", ",", rate)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(0, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], rmax)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")

  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
}

################################################################################
## B-region (name: plot+p+name_distribution+br+gui)
################################################################################
# OBS.: br - B-region; gui: "plot", "rstudio", "tcltk"
#-------------------------------------------------------------------------------
#####################
# Normal distribution
#####################
# Plot
plotpnormalbrplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylab = expression(f[X](x)), xlab = "X",
        ylim = c(0, 1.2 * max(fx,fy)),
        panel.first = grid(col="gray90"),
        main = main,
        cex = 0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pnorm(q[2], mean = mu,sd = sigma, lower.tail = T) - pnorm(q[1], mean = mu, sd=sigma, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
} # plotcurve (older)
# RStudio and tcltk
plotpnormalbrrstudio <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalbrplot(q, mu, sigma, rounding, main)
}
# Tcl/tk
plotpnormalbrtcltk <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalbrplot(q, mu, sigma, rounding, main)
}

########################
# T-Student distribution
########################
# Plot
plotptstudentbrplot <- function(q, df, rounding, main = NULL){
  nu <- df
  llower <- if(abs(q[1]) > 6) abs(q[1] + 2) else 6
  lupper <- if(abs(q[2]) > 6) abs(q[2] + 2) else 6
  x <- seq(q[1], q[2], by=0.01)
  y <- seq(-llower, lupper, by=0.01)
  fx <- dt(x, df = nu)
  fy <- dt(y, df = nu)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))

    }
  }
  curve(dt(x, df = nu), -llower, lupper, ylab = expression(f[X](x)), xlab="X",
        ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
        main = main, cex = 0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pt(q[2], df = nu) - pt(q[1], df = nu), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>t1)+P(X<t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>=t1)+P(X<=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>=t1)+P(X<t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
  if (attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>t1)+P(X<=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(-llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute("Parameters:"~nu == df,
                               list(df = nu)))
  }
}
# RStudio
plotptstudentbrrstudio <- function(q1, q2, df, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotptstudentbrplot(q, df, rounding, main)
}
# Tcl/tk
## Soon...
######################
# Poisson distribution
######################
# Plot
plotppoissonbrplot <- function(q, lambda, rounding, main = NULL){
  # readjusting the range
  ## ab-region
  if (is.double(q)) {
    if (attr(q, "region") == "region5") {
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region1") {
      q[1] <- q[1] - 1
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region6") {
      q[1] <- q[1] - 1
    }
    ## b-region
    if (attr(q, "region") == "region7") {
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region2") {
      q[1] <- q[1] + 1
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region8") {
      q[1] <- q[1] + 1
    }
    if (q[1] >= q[2]) {
      saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
      cat(crayon::silver(saida))
      stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
    }
  }

  rmin <- if (q[1] < lambda) trunc(q[1] - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > lambda) ceiling(q[2] + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  points(x, probx, lwd = 2, pch = 19, panel.first = grid(col = "gray90"))
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  Pr <- round(ppois(q = q[2], lambda = lambda) - ppois(q = q[1], lambda = lambda),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dpois(x1, lambda = lambda)
  lines(x1, probx1, type = "h", lwd = 2,col="red")
  points(x1, probx1, lwd = 2, pch = 19,col="red")
  # red x-axis
  # red x-axis
  axis(side=1, at=c(qqmin, qqmax), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(qq), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  # intervals
  abline(v = c(qqmin, qqmax), lty=2, col = "red")
  # rectangle
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  # title
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~lambda == lambd,
                             list(lambd = lambda)))
}
# RStudio
plotppoissonbrrstudio <- function(q1, q2, lambda, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotppoissonbrplot(q, lambda, rounding, main)
}
# Tcl/tk
## Soon...


######################
# Binomial distribution
######################
# Plot
plotpbinomialbrplot <- function(q, size, prob, rounding, main = NULL){
  # readjusting the range
  ## ab-region
  if (is.double(q)) {
    if (attr(q, "region") == "region5") {
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region1") {
      q[1] <- q[1] - 1
      q[2] <- q[2] + 1
    }
    if (attr(q, "region") == "region6") {
      q[1] <- q[1] - 1
    }
    ## b-region
    if (attr(q, "region") == "region7") {
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region2") {
      q[1] <- q[1] + 1
      q[2] <- q[2] - 1
    }
    if (attr(q, "region") == "region8") {
      q[1] <- q[1] + 1
    }
    if (q[1] >= q[2]) {
      saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
      cat(crayon::silver(saida))
      stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
    }
  }

  rmin <- if (q[1] < size) trunc(q[1] - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > size) ceiling(q[2] + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  probx <- dbinom(x, size = size, prob = prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  points(x, probx, lwd = 2, pch = 19, panel.first = grid(col = "gray90"))
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  Pr <- round(pbinom(q = q[2], size = size, prob = prob) - pbinom(q = q[1], size = size, prob = prob),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dbinom(x1, size = size,prob = prob)
  lines(x1, probx1, type = "h", lwd = 2,col="red")
  points(x1, probx1, lwd = 2, pch = 19,col="red")
  # red x-axis
  # red x-axis
  axis(side=1, at=c(qqmin, qqmax), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(qq), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  # intervals
  abline(v = c(qqmin, qqmax), lty=2, col = "red")
  # rectangle
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x")))
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                             list( N = size, P = prob)))
}
# RStudio
plotpbinomialbrrstudio <- function(q1, q2, size, prob, rouding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotpbinomialbrplot(q, size, prob, rouding, main)
}
# Tcl/tk
## Soon...

##########################
# Chi-Squared distribution
##########################
# Plot
plotpchisqbrplot <- function(q, df, ncp, rounding, main = NULL) {
  minimo <- if (q[1] <= ncp - 4 * df) ncp - 4 * df else 0
  maximo <- if (q[2] > ncp + 4 * df) q[2] + 4 * df else ncp + 4 * df
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + df);
    auxrect <- c(2 + df, 3.5 + df)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dchisq(x, df = df, ncp = ncp), minimo, maximo,
        ylab = expression(f[X](x)), xlab = "X",
        ylim = auxmain,
        panel.first = grid(col="gray90"),
        main = main,
        cex = 0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pchisq(q[2], df = df, ncp = ncp, lower.tail = T) - pchisq(q[1], df = df, ncp = ncp, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                               list(ncpv = ncp, dfv = df)))
  }
} # plotcurve (older)
# RStudio and tcltk
plotpchisqbrrstudio <- function(q1, q2, df, ncp, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqbrplot(q, df, ncp, rounding, main)
}
# Tcl/tk
## Soon...



#####################
# F distribution
#####################
# Plot
plotpfbrplot <- function(q, df1, df2, rounding, main = NULL) {
  minimo <- 0
  maximo <- 10
  if(q[2]>10){
    maximo <- q[2]+ 2*(df1/df2)
  }
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fy <- df(y, df1, df2)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + (df1/df2));
    auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(df(x, df1, df2), minimo, maximo,
        ylab = expression(f[X](x)), xlab = "X",
        ylim = auxmain,
        panel.first = grid(col="gray90"),
        main = main,
        cex.main = 1)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pf(q[2], df1, df2, lower.tail = T) - pf(q[1], df1, df2, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                               list(df1v = df1, df2v = df2)))
  }
} # plotcurve (older)
# RStudio and tcltk
plotpfbrrstudio <- function(q1, q2, df1, df2, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfbrplot(q, df1, df2, rounding, main)
}

#####################
# Gumbel distribution
#####################
# Plot
plotpgumbelbrplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
  maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fy <- dgumbel(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dgumbel(x, location, scale), minimo, maximo,
        ylab = expression(f[X](x)), xlab = "X",
        ylim = c(0, 1.2 * max(fx,fy)),
        panel.first = grid(col="gray90"),
        main = main,
        cex = 0.8)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pgumbel(q[2], location, scale, lower.tail = T) - pgumbel(q[1], location, scale, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))
  }
}

# RStudio and tcltk
plotpgumbelbrrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgumbelbrplot(q, location, scale, rounding, main)
}

####################
# Beta distribution
####################
plotpbetabrplot <- function(q, shape1, shape2, rounding, main = NULL) {

  x <- seq(q[1],q[2], by = 0.01)
  y <- seq(0, 1, by = 0.01)
  fx <- dbeta(x, shape1, shape2)
  fy <- dbeta(y, shape1, shape2)

  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }

  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + (shape1/shape2));
    auxrect <- c(2 + shape1/shape2, 3.5 + shape1/shape2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }

  curve(dbeta(x, shape1, shape2), 0, 1,
        ylim = auxmain,
        xlab="X",
        ylab = expression(f[X](x)),
        panel.first = grid(col="gray90"), main = main)

polygon(c(y, rev(y)),
        c(fy, rep(0, length(fy))),
        col="gray90")
polygon(c(x, rev(x)),
        c(fx, rep(0, length(fx))),
        col="red")
qq <- round(q, digits=2)
qqaux <- qq
Pr <- round(pbeta(q[2], shape1,shape2, lower.tail = T) - pbeta(q[1], shape1, shape2, lower.tail = T), digits=rounding)
Pr <- gsub("\\.", ",", Pr)
qq <- gsub("\\.", ",", qq)
aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
axis(side=1, at=qq, lwd = 0,
     col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
axis(side=1, at=qqaux, labels=FALSE,
     col="red", font = 2, col.axis = "red")
abline(v = qqaux, lty=2, col = "red")
rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
if (attr(q, "region") == "region2") {
  legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                   legend = substitute(P(X>t1)+P(X<t2)==Pr,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                             list(alphav = shape1, betav = shape2)))
}
if (attr(q, "region") == "region4") {
  legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                   legend = substitute(P(X>=t1)+P(X<=t2)==Pr,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                             list(alphav = shape1, betav = shape2)))
}
if (attr(q, "region") == "region7") {
  legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                   legend = substitute(P(X>=t1)+P(X<t2)==Pr,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                             list(alphav = shape1, betav = shape2)))
}
if ( attr(q, "region") == "region8") {
  legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                   legend = substitute(P(X>t1)+P(X<=t2)==Pr,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                             list(alphav = shape1, betav = shape2)))
}
}

####################
# Exponential
####################
plotpexpbrplot <- function(q, rate, rounding, main = NULL){
  rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(0, rmax, by = 0.01)
  probx <- dexp(x, rate = rate)
  proby <- dexp(y, rate = rate)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  if(is.infinite(1.2 *max(probx, proby))){
    auxmain <- c(0, 2.5 + rate);
    auxrect <- c(2 + rate, 3.5 + rate)
  }else{
    auxrect <- max(probx, proby)
    auxmain <- c(0, 1.2 * max(probx, proby))
  }


  # Curve
  curve(dexp(x, rate), 0, rmax,
        ylab = expression(p[x](q)),
        xlab = "x", ylim = auxmain,
        panel.first = grid(col = "gray90"),
        main = main)


  polygon(c(y, rev(y)),
          c(proby, rep(0,length(proby))),
          col = "gray90")

  polygon(c(x, rev(x)),
          c(probx, rep(0,length(probx))),
          col = "red")

  abline(v = 1 / rate, lty = 2)

  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pexp(qq[2], rate = rate, lower.tail = T) -
                pexp(qq[1], rate = rate, lower.tail = T), rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")

  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
  if (attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~rate == rat,
                               list(rat = rate)))
  }
}

################################################################################
## lower.tail = TRUE (name: plot+q+name_distribution+ltt+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltt - lower.tail == TRUE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------
#####################
# Normal distribution
#####################

# Plot
plotpnormallttplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                       list(t1 = q, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                             list(media = mu, varen = sigma)))
} # plotcurve (older)


########################
# T-Student distribution
########################
# Plot
plotptstudentlttplot <- function(q, df, rounding, main = NULL){
  nu <- df
  lim <- if(abs(q) > 6) abs(q + 2) else 6
  x <- seq(-lim, q, by=0.01)
  y <- seq(q, lim, by=0.01)
  fx <- dt(x, df = nu)
  fy <- dt(y, df = nu)
  if(is.null(main)){
    main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dt(x, df = nu), -lim, lim, ylab = expression(f[X](X)),
        xlab="X", ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
        main = main, cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pt(qq, df = nu, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(-lim, qqaux)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  axis(side=1, at=as.character(qqaux), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  # Insert red horizontal line (X-axis)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(Fx(q)==P(X<=~q)*"="~Pr,
                                       list(q = qq, Pr = Pr)))
  legend(-lim, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~nu == df,
                             list(df = nu)))
}



######################
# Poisson distribution
######################
# Plot
plotppoissonlttplot <- function(q, lambda, rounding, main = NULL){
  rmin <- if (q < lambda) trunc(q - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > lambda) ceiling(q + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dpois(x, lambda = lambda)
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                          list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2, panel.first = grid(col = "gray90"))
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(ppois(qq, lambda = lambda, lower.tail = T), rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=as.character(q), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(side = 1, at = c(rmin,q), labels = FALSE,col = "red",col.axis = "red",  font = 2, lwd.ticks = 0, lwd = 1)
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",
                   legend = substitute(F[X](q)~"="~P(X<= q) == Pr,
                                       list(q = qq, Pr = Pr)), cex=0.8)
  legend(rmin, legaux$text$y, bty="n", bg = "white",
         legend = substitute("Parameters:"~lambda == lambd,
                             list(lambd = lambda)), cex=0.8)
}



#######################
# Binomial distribution
#######################
# Plot
plotpbinomiallttplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dbinom(x, size = size, prob = prob)
  probx1 <- dbinom(x1, size = size, prob = prob)
  probx2 <- dbinom(x2, size = size, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",main=substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                                                               list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2)
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pbinom(q, size = size, prob = prob, lower.tail = T), rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=as.character(q), lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(side = 1, at = c(rmin,q), labels = FALSE,col = "red",col.axis = "red",  font = 2, lwd.ticks = 0, lwd = 1)
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(F[X](q)~"="~P(X<= q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                             list( N = size, P = prob)))
}



##########################
# Chi-Squared distribution
##########################

# Plot
plotpchisqlttplot <- function(q, df, ncp, rounding, main = NULL) {
  minimo <- if (q <=  ncp - 4 * df) q - 4 * df else 0
  maximo <- if (q > ncp + 4 * df) q + 4 * df else ncp + 4 * df
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + df);
    auxrect <- c(2 + df, 3.5 + df)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2} *","~~Fx(t1)== integral(f[X](x^2)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dchisq(x, df = df, ncp = ncp), minimo, maximo,
        ylim = auxmain,
        ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pchisq(qq,  df = df, ncp = ncp, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                       list(t1 = q, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                             list(ncpv = ncp, dfv = df)))
}



#####################
# F distribution
#####################

# Plot
plotpflttplot <- function(q, df1, df2, rounding, main = NULL) {
  minimo <- 0
  maximo <- 10
  if(q>10){
    maximo <- q+ 2*(df1/df2)
  }
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fy <- df(y, df1, df2)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + (df1/df2));
    auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/ xB(frac(d[1],2),frac(d[2],2))*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(df(x, df1, df2), minimo, maximo,
        ylim = auxmain, ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex.main = 1)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pf(qq,  df1, df2, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                       list(t1 = q, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                             list(df1v = df1, df2v = df2)))
} # plotcurve (older)


#####################
# Gumbel distribution
#####################

# Plot
plotpgumbellttplot <- function(q, location, scale, rounding, main = NULL){
    minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
    maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgumbel(x, location, scale)
    fy <- dgumbel(y, location, scale)
    if (is.null(main)) {
      main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
    }
    curve(dgumbel(x, location, scale), minimo, maximo,
          ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main,
          cex = 0.8)
    polygon(c(x, rev(x)),
            c(fx, rep(0, length(fx))),
            col="red")
    polygon(c(y, rev(y)),
            c(fy, rep(0, length(fy))),
            col="gray90")
    # Insert vertical line over the mean
    qq <- round(q, digits=2)
    qqaux <-round(q, digits=2)
    Pr <- round(pgumbel(qq,  location, scale, lower.tail = TRUE), digits=rounding)
    #Pr <- gsub("\\.", ",", Pr)
    #qq <- gsub("\\.", ",", qq)
    # Insert red q point
    aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
    axis(side=1, at=qq, lwd = 0,
         col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
    axis(side=1, at=qqaux, labels=FALSE,
         col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
    # Insert red horizontal and vertical line (X-axis)
    axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 0, labels = FALSE)
    abline(v = qqaux, lty=2, col = "red")
    rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                         list(t1 = q, Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                               list(scalev = scale, locationv = location)))

}

#####################
# Beta distribution
#####################
plotpbetalttplot <- function(q, shape1, shape2, rounding, main) {
  x <- seq(0, q, by = 0.01)
  y <- seq(0, 1, by = 0.01)
  fx <- dbeta(x, shape1, shape2)
  fy <- dbeta(y, shape1, shape2)

  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + (shape1/shape2));
    auxrect <- c(2 + shape1/shape2, 3.5 + shape1/shape2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }

  curve(dbeta(x, shape1, shape2), 0,1 ,
        ylim = auxmain, ylab = expression(f[X](x)),xlab = "X",panel.first = grid(col = "gray90"),
        main = main)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")


  qq <- round(q, digits=2)
  Pr <- round(pbeta(qq,  shape1, shape2), digits=rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)

  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(0, q)), labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(0, q)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = q, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")

  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                       list(t1 = q, Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                             list(q = qq, Pr = Pr, alphav = shape1, betav= shape2)))
}

####################
# Exponential
####################
plotpexplttplot <- function(q, rate, rounding, main = NULL) {
  rmin <- 0
  rmax <- q + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
  x <- rmin:rmax
  x1 <- seq(rmin, q, by = 0.01)
  x2 <- seq(q, rmax, by = 0.01)
  probx <- dexp(x, rate = rate)
  probx1 <- dexp(x1, rate = rate)
  probx2 <- dexp(x2, rate = rate)

  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  if(is.infinite(1.2 *max(probx, probx1, probx2))){
    auxmain <- c(0, 2.5 + rate);
    auxrect <- c(2 + rate, 3.5 + rate)
  }else{
    auxrect <- max(probx, probx1, probx2)
    auxmain <- c(0, 1.2 *max(probx, probx1, probx2))
  }

  curve(dexp(x, rate), rmin, rmax,
        ylab = expression(f[x](X)),
        xlab = "x", ylim = auxmain,
        panel.first = grid(col = "gray90"),
        main = main)
  polygon(c(x2, rev(x2)),
          c(probx2, rep(0,length(probx2))),
          col = "gray90")
  polygon(c(x1, rev(x1)),
          c(probx1, rep(0,length(probx1))),
          col = "red")
  abline(v = 1 / rate, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pexp(qq, rate = rate, lower.tail = T), rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)
  rate2 <- gsub("\\.", ",", rate)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                   legend = substitute(P(X<=t1)==Pr,
                                       list(t1=q, Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
         legend = substitute("Parameters:"~rate == rat,
                             list(rat = rate)))
}

################################################################################
## lower.tail == FALSE (name: plot+q+name_distribution+ltf+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltf - lower.tail == FALSE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------
#####################
# Normal distribution
#####################
# Plot
plotpnormalltfplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <= mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    main = substitute(atop(bold("Probability function plot: Normal"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  abline(v = qqaux, lty=2, col = "red")

  axis(side=1, at=as.character(qqaux), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)

  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qqaux, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)

  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~mu ==  mean ~ "," ~ sigma == varen,
                             list(mean = mu, varen = sigma)))
}



########################
# T-Student distribution
########################
# Plot
plotptstudentltfplot <- function(q, df, rounding, main = NULL){
  nu <- df
  lim <- if(abs(q) > 6) abs(q + 2) else 6
  x <- seq(q, lim, by=0.01)
  y <- seq(-lim, q, by=0.01)
  fx <- dt(x, df = nu)
  fy <- dt(y, df = nu)
  if(is.null(main)){
    main <- substitute(atop(bold("Probability function plot: T-student"), f[X](x) == frac(1, root(nu)*B*(frac(1,2)*","*frac(nu,2)))*(1+frac("t"^2, nu))^{-(nu+1)/2}*","~~S[X](t1)== 1 - F[X](t1)~ "="*1 - integral(f[X](x)*"dx", -infinity, t1)~"="*P(X>= t1) == integral(f[X](x)*"dx", t1, infinity)), list(t1 = q, x = "x"))
  }
  curve(dt(x, df = nu), -lim, lim, ylab = expression(f[X](x)),
        xlab="X", ylim = c(0, 1.2 * max(c(fx,fy))), panel.first = grid(col = "gray90"),
        main = main, cex = 0.8)

  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pt(qq, df = nu, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux, tick = FALSE,
       col="red", font = 2, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=c(lim, qqaux), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  # Insert red horizontal and vertical line (X-axis)

  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(-lim, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~nu == df,
                             list(df = nu)))
}



######################
# Poisson distribution
######################
# Plot
plotppoissonltfplot <- function(q, lambda, rounding, main = NULL){
  rmin <- if (q < lambda) trunc(q - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > lambda) ceiling(q + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dpois(x, lambda = lambda)
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x %*% e^-symbol(lambda), x*"!")*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                          list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(ppois(qq, lambda = lambda, lower.tail = F), rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q + 1, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q + 1),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(side = 1, at = as.character(c(q+1,rmax)), labels = FALSE,col = "red",col.axis = "red",  tick = TRUE,
       lwd.ticks = 0, lwd = 1)
  abline(v = qqaux+1, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X >= q2) == Pr,
                                       list(q = qq, Pr = Pr, q2 = qq + 1)),cex=0.8)
  legend(rmin, legaux$text$y, bty="n", bg = "white",
         legend = substitute("Parameters:"~lambda == lambd,
                             list(lambd = lambda)), cex=0.8)
}



#######################
# Binomial distribution
#######################
# Plot
plotpbinomialltfplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dbinom(x, size = size, prob = prob)
  probx1 <- dbinom(x1, size = size, prob = prob)
  probx2 <- dbinom(x2, size = size, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X", main = substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*p^x*(1-p)^{n-x}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                                                                  list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pbinom(qq, size = size, prob = prob, lower.tail = F), digits=rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q + 1, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q + 1),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(side = 1, at = as.character(c(q+1,rmax)), labels = FALSE,col = "red",col.axis = "red",  tick = TRUE,
       lwd.ticks = 0, lwd = 1)
  abline(v = qqaux+1, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X >= q2) == Pr,
                                       list(q = qq, Pr = Pr, q2 = qq + 1)), cex=0.8)
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                             list( N = size, P = prob)))
}




##########################
# Chi-Squared distribution
##########################
# Plot
plotpchisqltfplot <- function(q, df, ncp, rounding, main = NULL) {
  minimo <- if (q <=  ncp - 4 * df) q - 4 * df else 0
  maximo <- if (q > ncp + 4 * df) q + 4 * df else ncp + 4 * df
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + df);
    auxrect <- c(2 + df, 3.5 + df)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Chi-Squared"), f[X](x^2) == frac(1, 2^{k/2}*gamma(k/2))*(x[k]^2)^{k/2-1}*e^{-x[k]^2/2}*","~~S[X](t1)~"="~1-Fx(t1)~"="~1-integral(f[X](x^2)*"dx", -infinity, t1)~"="~P(X>5)~"="~integral(f[X](x^2)*"dx", t1, infinity)), list(t1 = q, x = "x"))
  }
  curve(dchisq(x, df = df, ncp = ncp), minimo, maximo,
        ylim = auxmain,
        ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pchisq(qq,  df = df, ncp = ncp, lower.tail = FALSE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qqaux, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(Fx(t1)==P(X>t1)*"="~Pr,
                                       list(t1 = q, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~ncp == ncpv ~ "," ~ df == dfv,
                             list(ncpv = ncp, dfv = df)))
}



#####################
# F distribution
#####################
# Plot
plotpfltfplot <- function(q, df1, df2, rounding, main = NULL) {
  minimo <- 0
  maximo <- 10
  if(q>10){
    maximo <- q+ 2*(df1/df2)
  }
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fy <- df(y, df1, df2)
  if (is.null(main)) {
    if(is.infinite(1.2 * max(fx,fy))){
      auxmain <- c(0, 2.5 + (df1/df2));
      auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
    }else{
      auxrect <- max(fx,fy)
      auxmain <- c(0, 1.2 * max(fx,fy))
    }
    main = substitute(atop(bold("Probability function plot: F"), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(df(x, df1, df2), minimo, maximo,
        ylim = auxmain, ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex.main = 1)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pf(qq,  df1, df2, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  abline(v = qqaux, lty=2, col = "red")

  axis(side=1, at=as.character(qqaux), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)

  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qqaux, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)

  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                             list(df1v = df1, df2v = df2)))
}




#####################
# Gumbel distribution
#####################
plotpgumbelltfplot <- function(q, location, scale, rounding, main = NULL){
  minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
  maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fy <- dgumbel(y, location, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Gumbel"), f[X](x) == frac(1, symbol(beta))*~e^{-(z+e^-z)}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dgumbel(x, location, scale), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main,
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pgumbel(qq,  location, scale, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  abline(v = qqaux, lty=2, col = "red")

  axis(side=1, at=as.character(qqaux), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)

  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qqaux, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)

  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~location == locationv ~ "," ~ scale == scalev,
                             list(scalev = scale, locationv = location)))
}

#####################
# Beta distribution
#####################
plotpbetaltfplot <- function(q, shape1 , shape2, rounding, main = NULL ) {
x <- seq(q, 1, by=0.01)
y <- seq(0, 1, by=0.01)
fx <- dbeta(x, shape1, shape2)
fy <- dbeta(y, shape1, shape2)

if (is.null(main)) {
  main <- substitute(atop(bold("Probability function plot: Beta"), f[X](x) == frac(x^{alpha-1}*(1-x)^{beta-1}, B(alpha,beta))*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
if(is.infinite(1.2 * max(fx,fy))){
  auxmain <- c(0, 2.5 + (shape1/shape2));
  auxrect <- c(2 + shape1/shape2, 3.5 + shape1/shape2)
}else{
  auxrect <- max(fx,fy)
  auxmain <- c(0, 1.2 * max(fx,fy))
}
curve(dbeta(x, shape1, shape2), 1, 0,
      ylim = auxmain  ,ylab = expression(f[X](x)), xlab="X",
      panel.first = grid(col = "gray90"),
      main = main)
polygon(c(y, rev(y)),
        c(fy, rep(0, length(fy))),
        col="gray90")
polygon(c(x, rev(x)),
        c(fx, rep(0, length(fx))),
        col="red")

qqaux <-round(q, digits=2)
Pr <- pbeta(q, shape1, shape2, lower.tail = F)
aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
axis(side=1, at=q, lwd = 0,
     col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
axis(side=1, at=as.character(c(q, 1)), labels=FALSE,
     col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
# Insert red horizontal and vertical line (X-axis)
axis(side=1, at=as.character(c(q, 1)), tick = TRUE, lwd = 1,
     col="red", font = 2, lwd.ticks = 0, labels = FALSE)
abline(v = q, lty=2, col = "red")
abline(v = qqaux, lty = 2, col = "red")
abline(v = qqaux, lty=2, col = "red")
rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                 legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                     list(q = q, Pr = Pr)))
legend(0, legaux$text$y, bty="n", bg = "white", cex=0.8,
       legend = substitute("Parameters:"~alpha == alphav ~ "," ~ beta == betav,
                           list(alphav = shape1, betav = shape2)))
}

####################
# Exponential
####################
plotpexpltfplot <- function(q, rate, rounding, main = NULL) {
  rmin <- 0
  rmax <- (q + ceiling(1 / rate + 7 * sqrt(1 / rate^2)))
  x <- rmin:rmax
  x1 <- seq(rmin, q, by = 0.01)
  x2 <- seq(q, rmax, by = 0.01)
  probx <- dexp(x, rate = rate)
  probx1 <- dexp(x1, rate = rate)
  probx2 <- dexp(x2, rate = rate)

  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Exponential"), f[X](x) == lambda*e^{-lambda*x}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }

  if(is.infinite(1.2 *max(probx, probx1, probx2))){
    auxmain <- c(0, 2.5 + rate);
    auxrect <- c(2 + rate, 3.5 + rate)
  }else{
    auxrect <- max(probx, probx1, probx2)
    auxmain <- c(0, 1.2 *max(probx, probx1, probx2))
  }


  curve(dexp(x, rate), rmin, rmax,
        ylab = expression(p[x](q)),
        xlab = "x", ylim = auxmain,
        panel.first = grid(col = "gray90"),
        main = main)
  polygon(c(x2, rev(x2)),
          c(probx2, rep(0,length(probx2))),
          col = "red")
  polygon(c(x1, rev(x1)),
          c(probx1, rep(0,length(probx1))),
          col = "gray90")
  abline(v = rate, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pexp(qq, rate = rate, lower.tail = F), rounding)
  Pr <- gsub("\\.", ",", Pr)
  qq <- gsub("\\.", ",", qq)
  axis(
    side = 1, at = qqaux, labels = qqaux,
    col = "red", font = 2, col.axis = "red"
  )
  abline(v = qqaux, lty = 2, col = "red")
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(q, 1)), labels=FALSE,
       col="red", font = 2, col.axis = "red", tick = TRUE,lwd.ticks = 1)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(q, 1)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = q, lty=2, col = "red")
  abline(v = qqaux, lty = 2, col = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq, Pr = Pr)))
  legend(0, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~rate == rat,
                             list(rat = rate)))
}

################################################################################
## lower.tail == NULL (name: plot+q+name_distribution+ltn+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltf - lower.tail == FALSE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------

######################
# Poisson distribution
######################
# Plot
plotppoissonltnplot <- function(q, lambda, rounding, main = NULL){
  rmin <- if (q < lambda) trunc(q - 4 * sqrt(lambda)) else trunc(lambda - 4 * sqrt(lambda))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > lambda) ceiling(q + 4 * sqrt(lambda)) else ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dpois(x, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Poisson"), p[X](x) == frac(symbol(lambda)^x * e^-symbol(lambda), x*"!")*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dpois(qq, lambda = lambda), rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",
                   legend = substitute(P[X](q)~"="~P(X == q) == Pr,
                                       list(q = qq, Pr = Pr)), cex=0.8)
  legend(rmin, legaux$text$y, bty="n", bg = "white",
         legend = substitute("Parameters:"~lambda == lambd,
                             list(lambd = lambda)), cex=0.8)
}

######################
# F distribution
######################
# Plot
plotpbinomialltnplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dbinom(x, size, prob)
  probx2 <- dbinom(x2, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Binomial"), p[X](x) == frac(n*"!",x*"!"*(n-x)*"!")*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dbinom(qq, size, prob), rounding)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=q, lwd = 0,
       col="red", font = 2, tick = TRUE, col.axis = "red", pos = aux2)
  axis(
    side = 1,
    at = as.character(q),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1], 1.03 * max(probx), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red",
                   legend = substitute(P[X](q)~"="~P(X == q) == Pr,
                                       list(q = qq, Pr = Pr)), cex=0.8)
  legend(rmin, legaux$text$y, bty="n", bg = "white",
         legend = substitute("Parameters:"~ n == N ~ "," ~ p == P,
                             list( N = size, P = prob)),cex = 0.8)
}


