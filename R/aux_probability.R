# Auxiliar functions of P()
# Observations:
#    - `%>X>%`() internal function
# Continuous Distributions
## A-region (name: plot+p+name_distribution+ar+gui)
# OBS.: ar - A-region; gui: "plot", "rstudio", "tcltk"

# Normal distribution
## Plot
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
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region3") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region5") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region6") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
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
  ##qq <- gsub("\\.", ",", qq)
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
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma, parametros = parametros)))
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
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma, parametros = parametros)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma,
                                    parametros = parametros)))
  }
} # plotcurve (older)
## RStudio
plotpnormalarrstudio <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalarplot(q, mu, sigma, rounding, main)
}
## Tcl/tk
plotpnormalartcltk <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnormalarplot(q, mu, sigma, rounding, main)
}



# Student's t distribution
## Plot
plotptstudentarplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  nu <- df
  # Auxiliar function
  llower <- if (nu <= 2) q[1] - 8 - 2 * abs(ncp) else q[1]  - 4 * .erro_padrao_t_nc(nu, ncp)
  lupper <- if (nu <= 2) q[2] + 8 + 2 * abs(ncp) else q[2] + 4 * .erro_padrao_t_nc(nu, ncp)
  x <- seq(llower, q[1], by=0.01)
  z <- seq(q[2], lupper, by=0.01)
  y <- seq(llower, lupper, by=0.01)
  fx <- dt(x, df = nu, ncp)
  fz <- dt(z, df = nu, ncp)
  fy <- dt(y, df = nu, ncp)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region3") {
      titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region5") {
      titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region6") {
      titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
  }

  curve(dt(x, df = nu, ncp), llower, lupper, ylab = expression(f[X](x)), xlab="X",
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
  Pr <- round(pt(q[1], df = nu, ncp, lower.tail = T) + pt(q[2], df = nu, ncp, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
    parametro <- gettext("Parameters:", domain = "R-leem")
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu == df,
                               list(df = nu, parametro = parametro)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu == df,
                               list(df = nu, parametro = parametro)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu == df,
                               list(df = nu, parametro = parametro)))
  }
  if (attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu == df,
                               list(df = nu, parametro = parametro)))
  }
}
## RStudio
plotptstudentarrstudio <- function(q1,q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotptstudentarplot(q, df, ncp, rounding, main)
}
## Tcl/tk
plotptstudentartcltk <- function(q1,q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotptstudentarplot(q, df, ncp, rounding, main)
}



# Chi-Squared distribution
## Plot
plotpchisqarplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  sig4n <- df + ncp - 4 * sqrt(2 * (df + 2 * ncp)) # mu - 4 * sigma
  sig4p <- df + ncp + 5 * sqrt(2 * (df + 2 * ncp)) # mu + 5 * sigma
  minimo <- if (sig4n < 0 | sig4n > q[1]) 0 else sig4n
  maximo <- if (sig4p < q[2]) q[2] + sig4p else sig4p
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
    titulo <- gettext("Probability function plot: Chi-Squared", domain = "R-leem")
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
  }
  parametro <- gettext("Parameters:", domain = "R-leem")
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
  ##qq <- gsub("\\.", ",", qq)
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
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  if (attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  # Legend
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute(parametro~nu == dfv,
                             list(dfv = df, parametro = parametro)))
} # plotcurve (older)
## RStudio
plotpchisqarrstudio <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqarplot(q, df, ncp, rounding, main)
}
## Tcl/tk
plotpchisqartcltk <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqarplot(q, df, ncp, rounding, main)
}


# F distribution
## Plot
plotpfarplot <- function(q, df1, df2, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  if (df2 <= 4) {
    maximo <- q[2] + ncp + 6 * (df1 / df2)
  } else {
    maximo <- q[2] + 6 * .desvio_padrao_f_nc(df1, df2, ncp)
  }
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- df(x, df1, df2, ncp)
  fz <- df(z, df1, df2, ncp)
  fy <- df(y, df1, df2, ncp)
  if (is.infinite(1.2 * max(fx, fy, fz)))  {
    auxmain <- c(0, 2.5 + (df1 / df2));
    auxrect <- c(2 + df1 / df2, 3.5 + df1 / df2)
  } else {
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy,fz))
  }
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: F", domain = "R-leem")
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
  }
  curve(df(x, df1, df2, ncp),
        minimo,
        maximo,
        ylim = auxmain,
        xlab="X",
        ylab = expression(f[X](X)),
        panel.first = grid(col="gray90"),
        main = main,
        cex.main=1, n = 300)
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
  Pr <- round(pf(q[1], df1, df2, ncp,  lower.tail = T) + pf(q[2], df1, df2, ncp, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                                         list(df1v = df1, df2v = df2, parametro = parametro)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
} # plotcurve (older)
## RStudio
plotpfarrstudio <- function(q1, q2, df1, df2, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfarplot(q, df1, df2, ncp, rounding, main)
}
## Tcl/tk
plotpfartcltk <- function(q1, q2, df1, df2, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfarplot(q, df1, df2, ncp, rounding, main)
}

# Gumbel distribution
## Plot
plotpgumbelarplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
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
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpgumbelarrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgumbelarplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Beta distribution
## Plot
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
  qq <- round(q, digits= rounding)
  qqaux <- qq
  Pr <- round(pbeta(q[1], shape1, shape2, lower.tail = T) + pbeta(q[2], shape1, shape2, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpbetaarrstudio <- function(q1, q2, shape1, shape2, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpbetaarplot(q, shape1, shape2, rounding, main)
}
## Tcl/tk
## Soon...


# Exponential distribution
## Plot
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
#Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpexparrstudio <- function(q1, q2, rate, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpexparplot(q, rate, rounding, main)
}
## Tcl/tk
## Soon...


# Gamma distribution
## Plot
plotpgammaarplot <- function(q, shape, rate, scale, rounding, main = NULL) {
  if (is.na(rate)){
    rate <- 1/scale
    auxarg <- scale
    minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
    maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
    x <- seq(minimo, q[1], by = 0.01)
    z <- seq(q[2], maximo, by = 0.01)
    y <-seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = auxarg)
    fz <- dgamma(z, shape, scale = auxarg)
    fy <- dgamma(y, shape, scale = auxarg)
    Pr <- round(pgamma(q[1], shape, scale = auxarg, lower.tail = T) +
                  pgamma(q[2], shape, scale = auxarg, lower.tail = F), digits=rounding)

      if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }

    curve(dgamma(x, shape, scale = auxarg), minimo, maximo,
          ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
          ylab = expression(f[X](X)),
          panel.first = grid(col="gray90"),
          main = main,
          cex=0.8)
  }

  if (is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg))  q[1] - 4 * sqrt(auxarg) (auxarg) else 0
    maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(minimo, q[1], by = 0.01)
    z <- seq(q[2], maximo, by = 0.01)
    y <-seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate = auxarg)
    fz <- dgamma(z, shape, rate = auxarg)
    fy <- dgamma(y, shape, rate = auxarg)
    Pr <- round(pgamma(q[1], shape, rate = auxarg, lower.tail = T) +
                  pgamma(q[2], shape, rate = auxarg, lower.tail = F), digits=rounding)


    if (is.null(main)) {
      if (attr(q, "region") == "region1") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region3") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region5") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region6") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
    }

    curve(dgamma(x, shape, rate = auxarg), minimo, maximo,
          ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
          ylab = expression(f[X](X)),
          panel.first = grid(col="gray90"),
          main = main,
          cex=0.8)
  }

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
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
}
## RStudio
plotpgammaarrstudio <- function(q1, q2, shape, rate, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgammaarplot(q, shape, rate, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Cauchy distribution
# Plot
plotpcauchyarplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fz <- dcauchy(z, location, scale)
  fy <- dcauchy(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dcauchy(x, location, scale), minimo, maximo,
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
  Pr <- round(pcauchy(q[1], location, scale, lower.tail = T) + pcauchy(q[2], location, scale, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
plotpcauchyarrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpcauchyarplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Logistic distribution
## Plot
plotplogisarplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fz <- dlogis(z, location, scale)
  fy <- dlogis(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dlogis(x, location, scale), minimo, maximo,
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
  Pr <- round(plogis(q[1], location, scale, lower.tail = T) + plogis(q[2], location, scale, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotplogisarrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotplogisarplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...

# Logarithmic Normal distribution
## Plot
plotplnormalarplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fz <- dlnorm(z,meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dlnorm(x, meanlog = mu, sdlog = sigma), minimo, maximo,
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
  Pr <- round(plnorm(q[1], meanlog = mu, sdlog = sigma, lower.tail = T) + plnorm(q[2], meanlog = mu, sdlog = sigma, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotplnormalarrstudio <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotplnormalarplot(q, mu, sigma, rounding, main)
}
## Tcl/tk
## Soon...

# Studentized Range distribution
## Plot
plotptukeyarplot <- function(q, nmeans, df, nranges, rounding, main = NULL) {
#   minimo <- if (q[1] <= nranges - 4 * nranges) q[1] - 4 * nranges else nranges - 4 * nranges
#   maximo <- if (q[2] > nranges + 4 * nranges) q[2] + 4 * nranges else nranges + 4 * nranges
#   x <- seq(minimo, q[1], by = 0.01)
#   z <- seq(q[2], maximo, by = 0.01)
#   y <-seq(minimo, maximo, by = 0.01)
#   fx <- ptukey(x, nmeans, df, nranges)
#   fz <- ptukey(z, nmeans, df, nranges)
#   fy <- ptukey(y, nmeans, df, nranges)
#   if (is.null(main)) {
#     if (attr(q, "region") == "region1") {
#       main <- substitute(atop(bold("Probability function plot: Studentized Range"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(nu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
#     }
#     if (attr(q, "region") == "region3") {
#       main <- substitute(atop(bold("Probability function plot: Studentized Range"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(nu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
#     }
#     if (attr(q, "region") == "region5") {
#       main <- substitute(atop(bold("Probability function plot: Studentized Range"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(nu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
#     }
#     if (attr(q, "region") == "region6") {
#       main <- substitute(atop(bold("Probability function plot: Studentized Range"), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(nu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
#     }
#   }
#   plot(ptukey(x, nmeans, df, nranges), minimo, maximo,
#         ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
#         ylab = expression(f[X](X)),
#         panel.first = grid(col="gray90"),
#         main = main,
#         cex=0.8)
#   polygon(c(y, rev(y)),
#           c(fy, rep(0, length(fy))),
#           col="gray90")
#   polygon(c(x, rev(x)),
#           c(fx, rep(0, length(fx))),
#           col="red")
#   polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
#           col="red" )
#   qq <- round(q, digits=2)
#   qqaux <- qq
#   Pr <- round(ptukey(q[1], nmeans, df, nranges, lower.tail = T) + ptukey(q[2], nmeans, df, nranges, lower.tail = F), digits=rounding)
#   #Pr <- gsub("\\.", ",", Pr)
#   ##qq <- gsub("\\.", ",", qq)
#   aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#   axis(side=1, at=qq, lwd = 0,
#        col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
#   axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
#        col="red", font = 2, lwd.ticks = 0, labels = FALSE)
#   axis(side=1, at=as.character(qq[1]), tick = TRUE, lwd = 1,
#        col="red", font = 2, lwd.ticks = 1, labels = FALSE)
#   axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
#        col="red", font = 2, lwd.ticks = 0, labels = FALSE)
#   axis(side=1, at=as.character(qq[2]), tick = TRUE, lwd = 1,
#        col="red", font = 2, lwd.ticks = 1, labels = FALSE)
#   abline(v = qqaux, lty=2, col = "red")
#   rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
#   if (attr(q, "region") == "region1") {
#     legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
#                      legend = substitute(P(X<t1)+P(X>t2)==Pr,
#                                          list(t1=qq[1],t2=qq[2], Pr = Pr)))
#     legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
#            legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
#                                list(media = mu, varen = sigma)))
#   }
#   if (attr(q, "region") == "region3") {
#     legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
#                      legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
#                                          list(t1=qq[1],t2=qq[2], Pr = Pr)))
#     legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
#            legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
#                                list(media = mu, varen = sigma)))
#   }
#   if (attr(q, "region") == "region5") {
#     legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
#                      legend = substitute(P(X<=t1)+P(X>t2)==Pr,
#                                          list(t1=qq[1],t2=qq[2], Pr = Pr)))
#     legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
#            legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
#                                list(media = mu, varen = sigma)))
#   }
#   if ( attr(q, "region") == "region6") {
#     legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
#                      legend = substitute(P(X<t1)+P(X>=t2)==Pr,
#                                          list(t1=qq[1],t2=qq[2], Pr = Pr)))
#     legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
#            legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
#                                list(media = mu, varen = sigma)))
#   }
# }
# plotptukeyarrstudio <- function(q1, q2, nmeans, df, nranges, rounding, main = NULL, q) {
#   q[1] <- q1
#   q[2] <- q2
#   plotptukeyarplot(q, nmeans, df, nranges, rounding, main)
}
## RStudio
## Soon...
## Tcl/tk
## Soon...


# Weibull distribution
## Plot
plotpweibullarplot <- function(q, shape, scale, rounding, main = NULL) {
  minimo <- if (q[1] <= shape - 4 * shape) q[1] - 4 * shape else 0
  maximo <- if (q[2] > shape + 4 * shape) q[2] + 4 * shape else shape + 4 * shape
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fz <- dweibull(z, shape, scale)
  fy <- dweibull(y, shape, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region1") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region3") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region5") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region6") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dweibull(x, shape, scale), minimo, maximo,
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
  Pr <- round(pweibull(q[1], shape, scale, lower.tail = T) + pweibull(q[2], shape, scale, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",  cex = 0.8,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = 0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
}
## RStudio
plotpweibullarrstudio <- function(q1, q2, shape, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpweibullarplot(q, shape, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Discrete Distributions


# Poisson distribution
## Plot
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
  Pr <- round(ppois(q = q[1], lambda = lambda) + ppois(q = q[2], lambda = lambda, lower.tail = FALSE),
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
## RStudio
plotppoissonarrstudio <- function(q1, q2, lambda, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotppoissonarplot(q,lambda, rounding, main)
}
## Tcl/tk
## Soon...


# Binomial distribution
## Plot
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
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
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
## Rstudio
plotpbinomialarrstudio <- function(q1, q2, size, prob, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotpbinomialarplot(q, size, prob, rounding, main)
}
## Tcl/tk
## Soon...


# Negative Binomial distribution
## Plot
plotpnbinomarplot <- function(q, size, prob, rounding, main = NULL){
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
  probx <- dnbinom(x, size, prob)

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
  Pr <- round(pnbinom(q = q[1], size, prob) + pnbinom(q = q[2] - 1, size, prob, lower.tail = FALSE),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dnbinom(x1, size, prob)
  probx2 <- dnbinom(x2, size, prob)
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
          main = substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~P(X <= t1)== 0*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~n == sizev ~ "," ~ p == probv,
                               list(sizev = size, probv = prob)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~n == sizev ~ "," ~ p == probv,
                               list(sizev = size, probv = prob)))
  }
}
## Rstudio
plotpnbinomarrstudio <- function(q1, q2, size, prob, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnbinomarplot(q, size, prob, rounding, main)
}
## Tcl/tk
## Soon...


# Hypergeometric distribution
## Plot
plotphyperarplot <- function(q, m, n, k, rounding, main = NULL){
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

  rmin <- if (q[1] < k) trunc(q[1] - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > k) ceiling(q[1] + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
  x <- rmin:rmax
  probx <- dhyper(x, m, n, k)

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
  Pr <- round((phyper(q = q[1], m, n, k, lower.tail = T)) + (phyper(q = q[2], m, n, k, lower.tail = F)),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dhyper(x1, m, n, k)
  probx2 <- dhyper(x2, m, n, k)
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
          main = substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~P(X <= t1)== 0*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                               list(mv = m, nv = n, kv = k)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                               list(mv = m, nv = n, kv = k)))
  }
}
## Rstudio
plotphyperarrstudio <- function(q1, q2, m, n, k, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotphyperarplot(q, m, n, k, rounding, main)
}
## Tcl/tk
## Soon...


# Geometric distribution
## Plot
plotpgeomarplot <- function(q, prob, rounding, main = NULL){
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

  rmin <- if (q[1] < 10*prob) trunc(q[1] - 4 * sqrt(10*prob)) else prob - 4 * sqrt(10*prob)
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > 10*prob) ceiling(q[2] + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
  x <- rmin:rmax
  probx <- dgeom(x, prob)

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
  Pr <- round(pgeom(q = q[1], prob) + pgeom(q = q[2], prob, lower.tail = FALSE),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dgeom(x1, prob)
  probx2 <- dgeom(x2, prob)
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
          main = substitute(atop(bold("Probability function plot: Geometric"), p[X](x) == q^{k-1}*p[k]*","~~P(X <= t1)== 0*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~prob == probv,
                               list(probv = prob)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Geometric"), p[X](x) ==  q^{k-1}*p[k]*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~prob == probv,
                               list(probv = prob)))
  }
}
## Rstudio
plotpgeomarrstudio <- function(q1, q2, prob, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgeomarplot(q, prob, rounding, main)
}
## Tcl/tk
## Soon...


# Uniform distribution
## Plot
plotpunifarplot <- function(q, min, max, rounding, main = NULL){
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

  rmin <- if (q[1] < min) trunc(q[1] - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > max) ceiling(q[2] + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
  x <- rmin:rmax
  probx <- dunif(x, min, max)

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
  Pr <- round(punif(q = q[1], min, max) + punif(q = q[2] - 1, min, max, lower.tail = FALSE),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dunif(x1, min, max)
  probx2 <- dunif(x2, min,max)
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
          main = substitute(atop(bold("Probability function plot: Uniform"), p[X](x) == frac(1,b-a)*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red",cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                               list( A = min, B = max)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Uniform"), p[X](x) ==  frac(1,b-a)*","~~P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                               list( A = min, B = max)))
  }
}
## Rstudio
plotpunifarrstudio <- function(q1, q2, min, max, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpunifarplot(q, min, max, rounding, main)
}
## Tcl/tk
## Soon...


# Wilcoxon distribution
## Plot
plotpwilcoxarplot <- function(q, m, n, rounding, main = NULL){
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

  rmin <- if (q[1] < m+n) trunc(q[1] - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > m+n) ceiling(q[1] + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
  x <- rmin:rmax
  probx <- dwilcox(x, m, n)

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
  Pr <- round((pwilcox(q = q[1], m, n, lower.tail = T)) + (pwilcox(q = q[2], m, n, lower.tail = F)),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dwilcox(x1, m, n)
  probx2 <- dwilcox(x2, m, n)
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
          main = substitute(atop(bold("Probability function plot: Wilcoxon"), P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                               list(mv = m, nv = n)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Wilcoxon"), P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                               list(mv = m, nv = n)))
  }
}
## Rstudio
plotpwilcoxarrstudio <- function(q1, q2, m, n, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpwilcoxarplot(q, m, n, rounding, main)
}
## Tcl/tk
## Soon...



# Signed Wilcoxon distribution
## Plot
plotpswilcoxarplot <- function(q, n, rounding, main = NULL){
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

  rmin <- if (q[1] < n) trunc(q[1] - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > n) ceiling(q[1] + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  probx <- dsignrank(x, n)

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
  Pr <- round((psignrank(q = q[1], n, lower.tail = T)) + (psignrank(q = q[2], n, lower.tail = F)),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) qqmin else rmin:qqmin
  x2 <- qqmax:rmax
  probx1 <- dsignrank(x1, n)
  probx2 <- dsignrank(x2, n)
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
          main = substitute(atop(bold("Probability function plot: Signed Wilcoxon"), P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")))
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ n == nv,
                               list(nv = n)))
  } else{
    axis(side=1, at=as.character(c(rmin, qqmin)), tick = TRUE, lwd = 1,
         col="red", font = 2, lwd.ticks = 1, labels = FALSE)
    title(ylab = expression(p[X](x)), xlab = "X",
          main = substitute(atop(bold("Probability function plot: Signed Wilcoxon"), P(X <= t1)== sum(p[X](x), x <= t1, "")*","~~P(X >= t2)== sum(p[X](x), x >= t2, infinity)), list(t1 = qqmin, t2 = qqmax, x = "x")), cex = 1)
    # legends
    legaux <- legend("topleft", bty="n", fill="red", cex = 0.8,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qqmin,t2=qqmax, Pr = Pr)))
    legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
           legend = substitute("Parameters:"~ n == nv,
                               list(nv = n)))
  }
}
## Rstudio
plotpswilcoxarrstudio <- function(q1, q2, n, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpswilcoxarplot(q, n, rounding, main)
}


# Auxiliar functions of P()
# Observations:
#    - `%<X<%`() internal function
# B-region (name: plot+p+name_distribution+br+gui)
# OBS.: br - B-region; gui: "plot", "rstudio", "tcltk"

# Continuous Distributions

# Normal distribution
## Plot
plotpnormalbrplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region4") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region7") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo),
                              f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region8") {
      titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
      main <- substitute(atop(bold(titulo), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
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
  ##qq <- gsub("\\.", ",", qq)
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
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma,
                                    parametros = parametros)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma,
                                    parametros = parametros)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma,
                                    parametros = parametros)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma,
                                    parametros = parametros)))
  }
} # plotcurve (older)
# RStudio
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

# Student's t distribution
## Plot
plotptstudentbrplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  nu <- df
  #Auxiliary Arguments
  llower <- if (nu <= 2) q[1] - 8 - 2 * abs(ncp) else q[1]  - 4 * .erro_padrao_t_nc(nu, ncp)
  lupper <- if (nu <= 2) q[2] + 8 + 2 * abs(ncp) else q[2] + 4 * .erro_padrao_t_nc(nu, ncp)
  x <- seq(q[1], q[2], by=0.01)
  y <- seq(llower, lupper, by=0.01)
  fx <- dt(x, df = nu, ncp)
  fy <- dt(y, df = nu, ncp)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      titulo <- gettext("Probability function plot: Student's t")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region4") {
      titulo <- gettext("Probability function plot: Student's t")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region7") {
      titulo <- gettext("Probability function plot: Student's t")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region8") {
      titulo <- gettext("Probability function plot: Student's t")
      main <- substitute(atop(bold(titulo),
                              f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))

    }
  }
  curve(dt(x, df = nu, ncp), llower, lupper, ylab = expression(f[X](x)), xlab="X", n = 300,
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
  Pr <- round(pt(q[2], df = nu, ncp) - pt(q[1], df = nu, ncp), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  parametros <- gettext("Parameters:", domain = "R-leem")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>t1)+P(X<t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametros~nu == df,
                               list(df = nu, parametros = parametros)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>=t1)+P(X<=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute(parametros~nu == df,
                               list(df = nu, parametros)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>=t1)+P(X<t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute(parametros~nu == df,
                               list(df = nu, parametros = parametros)))
  }
  if (attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(X>t1)+P(X<=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(llower, legaux$text$y, bty="n", bg = "white",cex=0.8,
           legend = substitute(parametros~nu == df,
                               list(df = nu, parametros = parametros)))
  }
}
## RStudio
plotptstudentbrrstudio <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotptstudentbrplot(q, df, ncp, rounding, main)
}
## Tcl/tk
plotptstudentbrtcltk <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotptstudentbrplot(q, df, ncp, rounding, main)
}

# Chi-Squared distribution
## Plot
plotpchisqbrplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  sig4n <- df + ncp - 4 * sqrt(2 * (df + 2 * ncp)) # mu - 4 * sigma
  sig4p <- df + ncp + 5 * sqrt(2 * (df + 2 * ncp)) # mu + 5 * sigma
  minimo <- if (sig4n < 0 | sig4n > q[1]) 0 else sig4n
  maximo <- if (sig4p < q[2]) q[2] + sig4p else sig4p
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
    titulo <- gettext("Probability function plot: Chi-Squared", domain = "R-leem")
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo))
    }
  }
  parametro <- gettext("Parameters:", domain = "R-leem")
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
  ##qq <- gsub("\\.", ",", qq)
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
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
  }
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute(parametro~nu == dfv,
                             list(dfv = df, parametro = parametro)))
} # plotcurve (older)
## RStudio
plotpchisqbrrstudio <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqbrplot(q, df, ncp, rounding, main)
}
## Tcl/tk
plotpchisqbrtcltk <- function(q1, q2, df, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpchisqbrplot(q, df, ncp, rounding, main)
}

# F distribution
## Plot
plotpfbrplot <- function(q, df1, df2, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  maximo <- 10
  if(q[2]>10){
    maximo <- q[2]+ 2*(df1/df2)
  }
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- df(x, df1, df2, ncp)
  fy <- df(y, df1, df2, ncp)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + (df1/df2));
    auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
  }else{
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: F", domain = "R-leem")
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                         list(t1 = q[1], t2 = q[2], x = "x", titulo = titulo))
    }
  }
  curve(df(x, df1, df2, ncp), minimo, maximo,
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
  Pr <- round(pf(q[2], df1, df2, ncp, lower.tail = T) - pf(q[1], df1, df2, ncp, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=qqaux, labels=FALSE,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * auxrect, par("usr")[2], par("usr")[4], col = "gray")
  parametro <- gettext("Parameters:", domain = "R-leem")
  if (attr(q, "region") == "region2") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                               list(df1v = df1, df2v = df2, parametro = parametro)))
  }
} # plotcurve (older)
## RStudio
plotpfbrrstudio <- function(q1, q2, df1, df2, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfbrplot(q, df1, df2, ncp, rounding, main)
}
## Tcl/tk
plotpfbrtcltk <- function(q1, q2, df1, df2, ncp = 0, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpfbrplot(q, df1, df2, ncp, rounding, main)
}

# Gumbel distribution
## Plot
plotpgumbelbrplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
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
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpgumbelbrrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgumbelbrplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...

# Beta distribution
## Plot
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
#qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpbetabrrstudio <- function(q1, q2, shape1, shape2, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpbetabrplot(q, shape1, shape2, rounding, main)
}
## Tcl/tk
## Soon...

# Exponential distribution
## Plot
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
## RStudio
plotpexpbrrstudio <- function(q1, q2, rate, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpexpbrplot(q, rate, rounding, main)
}
## Tcl/tk
## Soon...

# Gamma distribution
## Plot
plotpgammabrplot <- function(q, shape, rate, scale, rounding, main = NULL) {
  if (is.na(rate)){
    rate <- 1/scale
    auxarg <- scale
    minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
    maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
    x <- seq(q[1], q[2], by = 0.01)
    y <- seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = auxarg)
    fy <- dgamma(y, shape, scale = auxarg)
    Pr <- round(pgamma(q = q[2], shape, scale = auxarg) - pgamma(q = q[1], shape, scale = auxarg),rounding)

    if (is.null(main)) {
      if (attr(q, "region") == "region2") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region4") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region7") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region8") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
    }

    curve(dgamma(x, shape, scale = auxarg), minimo, maximo,
          ylim = c(0, 1.2 * max(fx,fy)),xlab="X",
          ylab = expression(f[X](X)),
          panel.first = grid(col="gray90"),
          main = main,
          cex=0.8)
  }

  if (is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg))  q[1] - 4 * sqrt(auxarg) (auxarg) else 0
    maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(q[1], q[2], by = 0.01)
    y <- seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate = auxarg)
    fy <- dgamma(y, shape, rate = auxarg)
    Pr <- round(pgamma(q = q[2], shape, rate = auxarg) - pgamma(q = q[1], shape, rate = auxarg),rounding)


    if (is.null(main)) {
      if (attr(q, "region") == "region2") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region4") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region7") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
      if (attr(q, "region") == "region8") {
        main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) ==frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
      }
    }

    curve(dgamma(x, shape, rate = auxarg), minimo, maximo,
          ylim = c(0, 1.2 * max(fx,fy)),xlab="X",
          ylab = expression(f[X](X)),
          panel.first = grid(col="gray90"),
          main = main,
          cex=0.8)
  }


  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <- qq
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                               list(shapev = shape, ratev = rate, scalev = scale)))
  }
}
## RStudio
plotpgammabrrstudio <- function(q1, q2, shape, rate, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgammabrplot(q, shape, rate, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Cauchy distribution
## Plot
plotpcauchybrplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fy <- dcauchy(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dcauchy(x, location, scale), minimo, maximo,
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
  Pr <- round(pcauchy(q[2], location, scale, lower.tail = T) - pcauchy(q[1], location, scale, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotpcauchybrrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpcauchybrplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...



# Logistic distribution
## Plot
plotplogisbrplot <- function(q, location, scale, rounding, main = NULL) {
  minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
  maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fy <- dlogis(y, location, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dlogis(x, location, scale), minimo, maximo,
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
  Pr <- round(plogis(q[2], location, scale, lower.tail = T) - plogis(q[1], location, scale, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotplogisbrrstudio <- function(q1, q2, location, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotplogisbrplot(q, location, scale, rounding, main)
}
## Tcl/tk
## Soon...


# Logarithmic Normal distribution
## Plot
plotplnormalbrplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dlnorm(x, meanlog = mu, sdlog = sigma), minimo, maximo,
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
  Pr <- round(plnorm(q[2], meanlog = mu, sdlog = sigma, lower.tail = T) - plnorm(q[1], meanlog = mu, sdlog = sigma, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
## RStudio
plotplnormalbrrstudio <- function(q1, q2, mu, sigma, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotplnormalbrplot(q, mu, sigma, rounding, main)
}
## Tcl/tk
## Soon...

# Studentized Range distribution
## Plot
## Soon...

## RStudio
## Soon...

## Tcl/tk
## Soon...

# Weibull distribution
## Plot
plotpweibullbrplot <- function(q, shape, scale, rounding, main = NULL) {
  minimo <- if (q[1] <= shape - 4 * shape) q[1] - 4 * shape else 0
  maximo <- if (q[2] > shape + 4 * shape) q[2] + 4 * shape else shape + 4 * shape
  x <- seq(q[1], q[2], by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fy <- dweibull(y, shape, scale)
  if (is.null(main)) {
    if (attr(q, "region") == "region2") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region4") {
      main <- substitute(atop(bold("Probability function plot: Weibul"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region7") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
    if (attr(q, "region") == "region8") {
      main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)), list(t1 = q[1], t2 = q[2], x = "x"))
    }
  }
  curve(dweibull(x, shape, scale), minimo, maximo,
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
  Pr <- round(pweibull(q[2], shape, scale, lower.tail = T) - pweibull(q[1], shape, scale, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
  if ( attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty="n", fill="red",cex=0.8,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
           legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                               list(shapev = shape, scalev = scale)))
  }
}
## RStudio
plotpweibullbrrstudio <- function(q1, q2, shape, scale, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpweibullbrplot(q, shape, scale, rounding, main)
}
## Tcl/tk
## Soon...

# Discrete Distributions


# Poisson distribution
## Plot
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
## RStudio
plotppoissonbrrstudio <- function(q1, q2, lambda, rounding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotppoissonbrplot(q, lambda, rounding, main)
}
## Tcl/tk
## Soon...



# Binomial distribution
## Plot
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
## RStudio
plotpbinomialbrrstudio <- function(q1, q2, size, prob, rouding, main = NULL, q){
  q[1] <- q1
  q[2] <- q2
  plotpbinomialbrplot(q, size, prob, rouding, main)
}
# Tcl/tk
## Soon...


# Negative Binomial distribution
## Plot
plotpnbinombrplot <- function(q, size, prob, rounding, main = NULL){

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
  probx <- dnbinom(x, size, prob)

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
  Pr <- round(pnbinom(q = q[2], size, prob) - pnbinom(q = q[1], size, prob),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dnbinom(x1, size, prob)
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
    main <- substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~n == sizev ~ "," ~ p == probv,
                             list(sizev = size, probv = prob)))
}
## RStudio
plotpnbinombrrstudio <- function(q1, q2, size, prob, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpnbinombrplot(q, size, prob, rounding, main)
}
# Tcl/tk
## Soon...


# Hypergeometric distribution
## Plot
plotphyperbrplot <- function(q, m, n, k, rounding, main = NULL){
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
  rmin <- if (q[1] < k) trunc(q[1] - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > k) ceiling(q[1] + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
  x <- rmin:rmax
  probx <- dhyper(x, m, n, k)

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
  Pr <-  round(phyper(q = q[2], m, n, k) - phyper(q = q[1], m, n, k), digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dhyper(x1, m, n, k)
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
    main <- substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                             list(mv = m, nv = n, kv = k)))
}
## RStudio
plotphyperbrrstudio <- function(q1, q2, m, n, k, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotphyperbrplot(q, m, n, k, rounding, main)
}
## Tcl/tk
## Soon...


# Geometric distribution
## Plot
plotpgeombrplot <- function(q, prob, rounding, main = NULL){
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

  rmin <- if (q[1] < 10*prob) trunc(q[1] - 4 * sqrt(10*prob)) else trunc(10*prob - 4 * sqrt(10*prob))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > 10*prob) ceiling(q[2] + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
  x <- rmin:rmax
  probx <- dgeom(x, prob)

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
  Pr <- round(pgeom(q = q[2], prob) - pgeom(q = q[1], prob),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dgeom(x1, prob)
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
    main <- substitute(atop(bold("Probability function plot: Geometric"), p[X](x) == q^{k-1}*p[k]*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~prob == probv,
                             list(probv = prob)))
}
## RStudio
plotpgeombrrstudio <- function(q1, q2, prob, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpgeombrplot(q, prob, rounding, main)
}
## Tcl/tk
## Soon...

# Uniform distribution
## Plot
plotpunifbrplot <- function(q, min, max, rounding, main = NULL){
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

  rmin <- if (q[1] < min) trunc(q[1] - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > max) ceiling(q[2] + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
  x <- rmin:rmax
  probx <- dunif(x, min, max)

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
  Pr <- round(punif(q = q[2], min, max) - punif(q = q[1], min, max),
              digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dunif(x1, min, max)
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
        main = substitute(atop(bold("Probability function plot: Uniform"), p[X](x) == frac(1,b-a)*","~~P(t1<=~X<=~t2)== sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x")))
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                             list( A = min, B = max)))
}
## RStudio
plotpunifbrrstudio <- function(q1, q2, min, max, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpunifbrplot(q, min, max, rounding, main)
}
## Tcl/tk
## Soon...


# Wilcoxon distribution
## Plot
plotpwilcoxbrplot <- function(q, m, n, rounding, main = NULL){
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
  rmin <- if (q[1] < m+n) trunc(q[1] - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > m+n) ceiling(q[1] + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
  x <- rmin:rmax
  probx <- dwilcox(x, m, n)

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
  Pr <-  round(pwilcox(q = q[2], m, n) - pwilcox(q = q[1], m, n), digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dwilcox(x1, m, n)
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
    main <- substitute(atop(bold("Probability function plot: Wilcoxon"), p[X](x) == P(t1<=~X<=~t2)*"="* sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                             list(mv = m, nv = n)))
}
## RStudio
plotpwilcoxbrrstudio <- function(q1, q2, m, n, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpwilcoxbrplot(q, m, n, rounding, main)
}
## Tcl/tk
## Soon...


# Signed Wilcoxon distribution
## Plot
plotpswilcoxbrplot <- function(q, n, rounding, main = NULL){
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
  rmin <- if (q[1] < n) trunc(q[1] - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q[2] > n) ceiling(q[1] + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  probx <- dsignrank(x, n)

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
  Pr <-  round(psignrank(q = q[2], n) - psignrank(q = q[1], n), digits = rounding)
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- qqmin:qqmax
  probx1 <- dsignrank(x1, n)
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
    main <- substitute(atop(bold("Probability function plot: Signed Wilcoxon"), p[X](x) == P(t1<=~X<=~t2)*"="* sum(p[X](x), x == t1, t2)), list(t1 = qqmin, t2 = qqmax, x = "x"))
  }
  title(ylab = expression(p[X](x)), xlab = "X",
        main = main, cex = 1)
  # legends
  legaux <- legend("topleft", bty="n", fill="red", cex=0.8,
                   legend = substitute(P(t1<=~X<=~t2)==Pr,
                                       list(t1=qqmin,t2=qqmax, Pr = Pr)))
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~n == nv,
                             list(nv = n)))
}
## RStudio
plotpswilcoxbrrstudio <- function(q1, q2, n, rounding, main = NULL, q) {
  q[1] <- q1
  q[2] <- q2
  plotpswilcoxbrplot(q, n, rounding, main)
}
## Tcl/tk
## Soon...

# lower.tail = TRUE (name: plot+q+name_distribution+ltt+type_distribution)
# OBS.: lt - lower.tail; ltt - lower.tail == TRUE;
#       type_distribution: cdf - cumulative distribution function;
#                          pdf - probability density function

# Continuous Distributions

# Normal distribution
## Plot
plotpnormallttplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)

  if (is.null(main)) {
    titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
    main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x", titulo = titulo))
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
  ##qq <- gsub("\\.", ",", qq)
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
  paramet <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute(paramet~mu == media ~ "," ~ sigma == varen,
                             list(media = mu, varen = sigma, paramet = paramet)))
}

# Student's t distribution
## Plot
plotptstudentlttplot <- function(q, df, ncp = 0, rounding, main = NULL){
  nu <- df
  lim <- if (abs(q) > 6) abs(q) + 2 + abs(ncp) * 3  else 6 + abs(ncp) * 3
  lim <- if(nu < 3) lim + nu * 20 else lim
  x <- seq(-lim, q, by=0.01)
  y <- seq(q, lim, by=0.01)
  fx <- dt(x, df = nu, ncp)
  fy <- dt(y, df = nu, ncp)
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
    main <- substitute(atop(bold(titulo),
                            f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)),
                       list(t1 = q, x = "x", titulo = titulo))
  }
  curve(dt(x, df = nu, ncp), -lim, lim, ylab = expression(f[X](X)), n = 300,
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
  Pr <- round(pt(qq, df = nu, ncp, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(-lim, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~nu == df,
                             list(df = nu, parametro = parametro)))
}

# Chi-Squared distribution
## Plot
plotpchisqlttplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  maximo <- if (q > ncp + 4 * df) q + 10 * df else ncp + 10 * df
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  if(is.infinite(1.2 * max(fx,fy))){
    auxmain <- c(0, 2.5 + df);
    auxrect <- c(2 + df, 3.5 + df)
  } else {
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: Chi-Squared", domain = "R-leem")
    main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~Fx(t1)== integral(f[X](x)*"dx", 0, t1)),
                       list(t1 = q, x = "x", titulo = titulo))
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
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~nu == dfv,
                             list(dfv = df, parametro = parametro)))
}

# F distribution
## Plot
plotpflttplot <- function(q, df1, df2, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  if (df2 <= 4) {
    maximo <- q + ncp + 6 * (df1 / df2)
  } else {
    maximo <- q + 6 * .desvio_padrao_f_nc(df1, df2, ncp)
  }
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2, ncp)
  fy <- df(y, df1, df2, ncp)
  if (is.infinite(1.2 * max(fx,fy))) {
    auxmain <- c(0, 2.5 + (df1/df2));
    auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
  } else {
    auxrect <- max(fx,fy)
    auxmain <- c(0, 1.2 * max(fx,fy))
  }
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: F", domain = "R-leem")
    main <- substitute(atop(bold(titulo), f[X](x*";"~nu[1]*","~nu[2]) == root(frac((nu[1]*x)^nu[1]*nu[2]^nu[2],(nu[1]*x+nu[2])^{nu[1]+nu[2]}))/ xB(frac(nu[1],2),frac(nu[2],2))*","~~Fx(t1)== integral(f[X](x)*"dx", 0, t1)),
                       list(t1 = q, x = "x", titulo = titulo))
  }
  curve(df(x, df1, df2, ncp), minimo, maximo, n =  300,
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
  Pr <- round(pf(qq,  df1, df2, ncp, lower.tail = TRUE), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                             list(df1v = df1, df2v = df2,
                                  parametro = parametro)))
} # plotcurve (older)

# Gumbel distribution
## Plot
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
    ##qq <- gsub("\\.", ",", qq)
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

# Beta distribution
## Plot
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
#Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)

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

# Exponential distribution
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
#Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
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

# Gamma distribution
## Plot
plotpgammalttplot <- function(q, shape, rate, scale, rounding, main = NULL) {
  if(is.na(rate)){
    rate <- 1/scale
    auxarg <- scale
    minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
    maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = auxarg)
    fy <- dgamma(y, shape, scale = auxarg)
    if (is.null(main)) {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
    }
    curve(dgamma(x, shape, scale = auxarg), minimo, maximo,
          ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main,
          cex = 0.8)
    Pr <- round(pgamma(q, shape, scale = auxarg, lower.tail = TRUE), digits=rounding)
  }

  if(is.na(scale)){
    scale <- 1/rate
    auxarg <- rate
    minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
    maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate = auxarg)
    fy <- dgamma(y, shape, rate = auxarg)
    if (is.null(main)) {
      main <- substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
    }
    curve(dgamma(x, shape, rate = auxarg), minimo, maximo,
          ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main,
          cex = 0.8)
    Pr <- round(pgamma(q, shape, rate = auxarg, lower.tail = TRUE), digits=rounding)
  }

  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
         legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                             list(shapev = shape, ratev = rate, scalev = scale)))
} # plotcurve (older)

# Cauchy distribution
## Plot
plotpcauchylttplot <- function(q, location, scale, rounding, main = NULL){
  minimo <- if (q <=  scale - 10 * scale) q - 10 * scale else scale - 10 * scale
  maximo <- if (q > scale + 10 * scale) q + 10 * scale else scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fy <- dcauchy(y, location, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dcauchy(x, location, scale), minimo, maximo,
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
  Pr <- round(pcauchy(qq,  location, scale, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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

# Logistic distribution
## Plot
plotplogislttplot <- function(q, location, scale, rounding, main = NULL){
  minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
  maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fy <- dlogis(y, location, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dlogis(x, location, scale), minimo, maximo,
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
  Pr <- round(plogis(qq,  location, scale, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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

# Logarithmic Normal distribution
## Plot
plotplnormallttplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dlnorm(x, meanlog = mu, sdlog = sigma), minimo, maximo,
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
  Pr <- round(plnorm(qq,  meanlog = mu, sdlog = sigma, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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

# Studentized Range distribution
## plot
## Soon...

# Weibull distribution
##
# Plot
plotpweibulllttplot <- function(q, shape, scale, rounding, main = NULL) {
  minimo <- if (q <= shape - 4 * shape) q - 4 * shape else 0
  maximo <- if (q > shape + 4 * shape) q + 4 * shape else shape + 4 * shape
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fy <- dweibull(y, shape, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = q, x = "x"))
  }
  curve(dweibull(x, shape, scale), minimo, maximo,
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
  Pr <- round(pweibull(qq, shape, scale, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
         legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                             list(shapev = shape, scalev = scale)))
} # plotcurve (older)


# Discrete Distributions

# Poisson distribution
## Plot
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


# Binomial distribution
## Plot
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

# Negative Binomial distribution
## Plot
plotpnbinomiallttplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dnbinom(x, size = size, prob = prob)
  probx1 <- dnbinom(x1, size = size, prob = prob)
  probx2 <- dnbinom(x2, size = size, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",main=substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                                                               list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2)
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pnbinom(q, size = size, prob = prob, lower.tail = T), rounding)
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

# Hypergeometric distribution
## Plot
plotphyperlttplot <- function(q, m, n, k, rounding, main = NULL){
  rmin <- if (q < k) trunc(q - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > k) ceiling(q + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dhyper(x, m, n, k)
  probx1 <- dhyper(x1, m, n, k)
  probx2 <- dhyper(x2, m, n, k)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                          list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2, panel.first = grid(col = "gray90"))
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(phyper(qq, m, n, k, lower.tail = T), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                             list(mv = m, nv = n, kv = k)))
}

# Geometric distribution
## Plot
plotpgeomlttplot <- function(q, prob, rounding, main = NULL){
  rmin <- if (q < 10*prob) trunc(q - 4 * sqrt(10*prob)) else trunc(10*prob - 4 * sqrt(10*prob))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > 10*prob) ceiling(q + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dgeom(x, prob)
  probx1 <- dgeom(x1, prob)
  probx2 <- dgeom(x2, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Geometric"), p[X](x) == q^{k-1}*p[k]*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                          list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2, panel.first = grid(col = "gray90"))
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pgeom(qq, prob, lower.tail = T), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~prob == probv,
                             list(probv = prob)))
}

# Uniform distribution
## Plot
plotpuniflttplot <- function(q, min, max, rounding, main = NULL){
  rmin <- if (q < min) trunc(q - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > max) ceiling(q + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dunif(x, min, max)
  probx1 <- dunif(x1, min, max)
  probx2 <- dunif(x2, min, max)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",main=substitute(atop(bold("Probability function plot: Uniform"), p[X](x) == frac(1,b-a)*","~~F[X](t) == sum(p[X](x), x<=t, "")),
                                                               list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2)
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(punif(q, min, max, lower.tail = T), rounding)
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
         legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                             list( A = min, B = max)))
}

# Wilcoxon distribution
## Plot
plotpwilcoxlttplot <- function(q, m, n, rounding, main = NULL){
  rmin <- if (q < m+n) trunc(q - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > m+n) ceiling(q + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dwilcox(x, m, n)
  probx1 <- dwilcox(x1, m, n)
  probx2 <- dwilcox(x2, m, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Wilcoxon"), F[X](t) == sum(p[X](x), x<=t, "")),
                          list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2, panel.first = grid(col = "gray90"))
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pwilcox(qq, m, n, lower.tail = T), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                             list(mv = m, nv = n)))
}

# Signed Wilcoxon distribution
## Plot
plotpswilcoxlttplot <- function(q, n, rounding, main = NULL){
  rmin <- if (q < n) trunc(q - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > n) ceiling(q + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q:rmax
  probx <- dsignrank(x, n)
  probx1 <- dsignrank(x1, n)
  probx2 <- dsignrank(x2, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Signed Wilcoxon"), F[X](t) == sum(p[X](x), x<=t, "")),
                          list(t = q, t2 = q + 1)))
  lines(x2, probx2, type = "h", lwd = 2, panel.first = grid(col = "gray90"))
  points(x2, probx2, lwd = 2, pch = 19)
  lines(x1, probx1, type = "h", lwd = 2, col = "red")
  points(x1, probx1, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(psignrank(qq, n, lower.tail = T), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~n == nv,
                             list(nv = n)))
}



## lower.tail == FALSE (name: plot+q+name_distribution+ltf+type_distribution)
# OBS.: lt - lower.tail; ltf - lower.tail == FALSE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function

# Continuous Distributions

# Normal distribution
## Plot
plotpnormalltfplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <= mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    titulo <- gettext("Probability function plot: Normal", domain = "R-leem")
    main = substitute(atop(bold(titulo),
                           f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)),
                      list(t = q, titulo = titulo))
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
  # #qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~mu ==  mean ~ "," ~ sigma == varen,
                             list(mean = mu, varen = sigma,
                                  parametro = parametro)))
}

# Student's t distribution
## Plot
plotptstudentltfplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  nu <- df
  lim <- if (abs(q) > 6) abs(q) + 2 + abs(ncp) * 3  else 6 + abs(ncp) * 3
  lim <- if(nu < 3) lim + nu * 20 else lim
  x <- seq(q, lim, by=0.01)
  y <- seq(-lim, q, by=0.01)
  fx <- dt(x, df = nu, ncp)
  fy <- dt(y, df = nu, ncp)
  if(is.null(main)){
    titulo <- gettext("Probability function plot: Student's t", domain = "R-leem")
    main <- substitute(atop(bold(titulo),
                            f[X](x*";"~nu) == frac(Gamma*group("[",(nu + 1) / 2,"]"), root(nu*pi)*Gamma*group("(",frac(1,2)*","*frac(nu,2),")"))*(1+frac(x^2, nu))^{-(nu+1)/2}*","~~S[X](t1)== 1 - F[X](t1)~ "="*1 - integral(f[X](x)*"dx", -infinity, t1)~"="*P(X>= t1) == integral(f[X](x)*"dx", t1, infinity)),
                       list(t1 = q, x = "x", titulo = titulo))
  }
  curve(dt(x, df = nu, ncp), -lim, lim, ylab = expression(f[X](x)),
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
  Pr <- round(pt(qq, df = nu, ncp, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(-lim, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~nu == df,
                             list(df = nu)))
}

# Chi-Squared distribution
## Plot
plotpchisqltfplot <- function(q, df, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  maximo <- if (q > ncp + 4 * df) q + 10 * df else ncp + 10 * df
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
    titulo <- gettext("Probability function plot: Chi-Squared", domain = "R-leem")
    main <- substitute(atop(bold(titulo), f[X](x*";"~nu) == frac(1, 2^{nu/2}*Gamma(nu/2))*x^{nu/2-1}*e^{- x / 2} *","~~S[X](t1)~"="~1-Fx(t1)~"="~1-integral(f[X](x)*"dx", -infinity, t1)~"="~P(X>5)~"="~integral(f[X](x)*"dx", t1, infinity)),
                       list(t1 = q, x = "x", titulo = titulo))
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
  ##qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~nu == dfv,
                             list(dfv = df, parametro = parametro)))
}

# F distribution
## Plot
plotpfltfplot <- function(q, df1, df2, ncp = 0, rounding, main = NULL) {
  minimo <- 0
  if (df2 <= 4) {
    maximo <- q + ncp + 6 * (df1 / df2)
  } else {
    maximo <- q + 6 * .desvio_padrao_f_nc(df1, df2, ncp)
  }
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2, ncp)
  fy <- df(y, df1, df2, ncp)
  if (is.null(main)) {
    if (is.infinite(1.2 * max(fx,fy))) {
      auxmain <- c(0, 2.5 + (df1/df2))
      auxrect <- c(2 + df1/df2, 3.5 + df1/df2)
    } else {
      auxrect <- max(fx,fy)
      auxmain <- c(0, 1.2 * max(fx,fy))
    }
    titulo <- gettext("Probability function plot: F", domain = "R-leem")
    main = substitute(atop(bold(titulo), f[X](x) == root(frac((d[1]*x)^d[1]*d[2]^d[2],(d[1]*x+d[2])^{d[1]+d[2]}))/xB(frac(d[1],2),frac(d[2],2))*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)),
                      list(t = q, titulo = titulo))
  }
  curve(df(x, df1, df2, ncp), minimo, maximo, n = 300,
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
  Pr <- round(pf(qq,  df1, df2, ncp, lower.tail = FALSE), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~nu[1] == df1v ~ "," ~ nu[2] == df2v,
                             list(df1v = df1, df2v = df2,
                                  parametro = parametro)))
}

# Gumbel distribution
## Plot
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
  # #qq <- gsub("\\.", ",", qq)
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

# Beta distribution
## Plot
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

# Exponential distribution
## Plot
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
#Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
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

# Gamma distribution
## Plot
plotpgammaltfplot <- function(q, shape, rate, scale, rounding, main = NULL) {
  if(is.na(rate)){
    rate <- 1/scale
    auxarg <- scale
    minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
    maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = auxarg)
    fy <- dgamma(y, shape, scale = auxarg)
    if (is.null(main)) {
      main = substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
    }
    curve(dgamma(x, shape, scale = auxarg), minimo, maximo,
          ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main,
          cex = 0.8)
    Pr <- round(pgamma(q, shape, scale = auxarg, lower.tail = FALSE), digits=rounding)

  }

    if(is.na(scale)){
    scale <- 1/rate
    auxarg <- rate
    minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
    maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else auxarg + 4 * sqrt(auxarg)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate = auxarg)
    fy <- dgamma(y, shape, rate = auxarg)
    if (is.null(main)) {
      main = substitute(atop(bold("Probability function plot: Gamma"), f[X](x) == frac(1, Gamma*(k)*theta^k)*x^{k-1}*e^{-frac(x,theta)}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
    }
    curve(dgamma(x, shape, rate = auxarg), minimo, maximo,
          ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray90"),
          main = main,
          cex = 0.8)
    Pr <- round(pgamma(q, shape, rate = auxarg, lower.tail = FALSE), digits=rounding)

  }

  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pgamma(qq,  shape, rate = auxarg, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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
         legend = substitute("Parameters:"~k == shapev ~ "," ~ symbol(beta) == ratev ~ "," ~ symbol(theta) == scalev,
                             list(shapev = shape, ratev = rate, scalev = scale)))
}

# Cauchy distribution
## Plot
plotpcauchyltfplot <- function(q, location, scale, rounding, main = NULL){
  minimo <- if (q <=  scale - 10 * scale) q - 10 * scale else scale - 10 * scale
  maximo <- if (q > scale + 10 * scale) q + 10 * scale else scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fy <- dcauchy(y, location, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Cauchy"), f[X](x) == frac(1,pi*gamma*'['*1+(frac(x-x[0],gamma))^2*']')*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dcauchy(x, location, scale), minimo, maximo,
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
  Pr <- round(pcauchy(qq,  location, scale, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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


# Logistic distribution
## Plot
plotplogisltfplot <- function(q, location, scale, rounding, main = NULL){
  minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
  maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fy <- dlogis(y, location, scale)
  if (is.null(main)) {
    main <- substitute(atop(bold("Probability function plot: Logistic"), f[X](x) == frac(e^{-(x-mu)*"/"*s}, s(1+e^{-(x-u)*"/"*s})^2)*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dlogis(x, location, scale), minimo, maximo,
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
  Pr <- round(plogis(qq,  location, scale, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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


# Logarithmic Normal distribution
## Plot
plotplnormalltfplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <= mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  if (is.null(main)) {
    main = substitute(atop(bold("Probability function plot: Logarithmic Normal"), f[X](x) == frac(1,x*sigma*sqrt(2*pi))*exp(-frac((ln*"(x)"-mu)^2,2*sigma^2))*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dlnorm(x, meanlog = mu, sdlog = sigma), minimo, maximo,
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
  Pr <- round(plnorm(qq,  meanlog = mu, sdlog = sigma, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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

# Studentized Range distribution
## Plot
## Soon...

# Weibull distribution
## Plot
plotpweibullltfplot <- function(q, shape, scale, rounding, main = NULL) {
  minimo <- if (q <= shape - 4 * shape) q - 4 * shape else 0
  maximo <- if (q > shape + 4 * shape) q + 4 * shape else shape + 4 * shape
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fy <- dweibull(y, shape, scale)
  if (is.null(main)) {
    main = substitute(atop(bold("Probability function plot: Weibull"), f[X](x) == frac(k,lambda)*(frac(x,lambda))^{k-1}*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)), list(t = q))
  }
  curve(dweibull(x, shape, scale), minimo, maximo,
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
  Pr <- round(pweibull(qq, shape, scale, lower.tail = FALSE), digits=rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # #qq <- gsub("\\.", ",", qq)
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
         legend = substitute("Parameters:"~lambda == shapev ~ "," ~ k == scalev,
                             list(shapev = shape, scalev = scale)))
}


# Discrete Distributions


# Poisson distribution
## Plot
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

# Binomial distribution
## Plot
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

# Negative Binomial distribution
## Plot
plotpnbinomialltfplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dnbinom(x, size = size, prob = prob)
  probx1 <- dnbinom(x1, size = size, prob = prob)
  probx2 <- dnbinom(x2, size = size, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X", main = substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                                                                  list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pnbinom(qq, size = size, prob = prob, lower.tail = F), digits=rounding)
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

# Hypergeometric distribution
## Plot
plotphyperltfplot <- function(q, m, n, k, rounding, main = NULL){
  rmin <- if (q < k) trunc(q - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > k) ceiling(q + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dhyper(x, m, n, k)
  probx1 <- dhyper(x1, m, n, k)
  probx2 <- dhyper(x2, m, n, k)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                          list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(phyper(qq, m, n, k, lower.tail = F), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                             list(mv = m, nv = n, kv = k)))
}

# Geometric distribution
## Plot
plotpgeomltfplot <- function(q, prob, rounding, main = NULL){
  rmin <- if (q < 10*prob) trunc(q - 4 * sqrt(10*prob)) else trunc(10*prob - 4 * sqrt(10*prob))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > 10*prob) ceiling(q + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dgeom(x, prob)
  probx1 <- dgeom(x1, prob)
  probx2 <- dgeom(x2, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Geometric"), p[X](x) == q^{k-1}*p[k]*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                          list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pgeom(qq, prob, lower.tail = F), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~prob == probv,
                             list(probv = prob)))
}

# Uniform distribution
## Plot
plotpunifltfplot <- function(q, min, max, rounding, main = NULL){
  rmin <- if (q < min) trunc(q - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > max) ceiling(q + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dunif(x, min, max)
  probx1 <- dunif(x1, min, max)
  probx2 <- dunif(x2, min, max)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X", main = substitute(atop(bold("Probability function plot: Uniform"), p[X](x) == frac(1,b-a)*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                                                                  list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(punif(qq, min, max, lower.tail = F), digits=rounding)
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
         legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                             list( A = min, B = max)))
}

# Wilcoxon distribution
## Plot
plotpwilcoxltfplot <- function(q, m, n, rounding, main = NULL){
  rmin <- if (q < m+n) trunc(q - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > m+n) ceiling(q + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dwilcox(x, m, n)
  probx1 <- dwilcox(x1, m, n)
  probx2 <- dwilcox(x2, m, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Wilcoxon"), S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                          list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(pwilcox(qq, m, n, lower.tail = F), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                             list(mv = m, nv = n)))
}

# Signed Wilcoxon distribution
## Plot
plotpswilcoxltfplot <- function(q, n, rounding, main = NULL){
  rmin <- if (q < n) trunc(q - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > n) ceiling(q + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
  auxq <- q+1
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- auxq:rmax
  probx <- dsignrank(x, n)
  probx1 <- dsignrank(x1, n)
  probx2 <- dsignrank(x2, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Signed Wilcoxon"), S[X](t)~"="~1 - F[X](t)~"="*1 - sum(p[X](x), x<=t, "")~"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)),
                          list(t = q, t2 = q + 1)))
  lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x1, probx1, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(psignrank(qq, n, lower.tail = F), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex = 0.8,
         legend = substitute("Parameters:"~n == nv,
                             list(nv = n)))
}


## lower.tail == NULL (name: plot+q+name_distribution+ltn+type_distribution)
# OBS.: lt - lower.tail; ltn - lower.tail == NULL;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function

# Discrete Distributions

# Poisson distribution
## Plot
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

# Binomial distribution
## Plot
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

# Negative Binomial distribution
## Plot
plotpnbinomialltnplot <- function(q, size, prob, rounding, main = NULL){
  rmin <- if (q < size) trunc(q - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > size) ceiling(q + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dnbinom(x, size, prob)
  probx2 <- dnbinom(x2, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Negative Binomial"), p[X](x) == (atop(k+r-1,k))*(1-p)^n*p^k*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dnbinom(qq, size, prob), rounding)
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

# Hypergeometric distribution
## Plot
plotphyperltnplot <- function(q, m, n, k, rounding, main = NULL){
  rmin <- if (q < k) trunc(q - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > k) ceiling(q + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dhyper(x, m, n, k)
  probx2 <- dhyper(x2, m, n, k)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Hypergeometric"), p[X](x) == frac((atop(K,k))*(atop(N-K,n-k)),(atop(N,n)))*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dhyper(qq, m, n, k), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv ~";"~ k == kv,
                             list(mv = m, nv = n, kv = k)))
}

# Geometric distribution
## Plot
plotpgeomltnplot <- function(q, prob, rounding, main = NULL){
  rmin <- if (q < 10*prob) trunc(q - 4 * sqrt(10*prob)) else trunc(10*prob - 4 * sqrt(10*prob))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > 10*prob) ceiling(q + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dgeom(x, prob)
  probx2 <- dgeom(x2, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Geometric"), p[X](x) == q^{k-1}*p[k]*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dgeom(qq, prob), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~prob == probv,
                             list(probv = prob)))
}

# Uniform distribution
## Plot
plotpunifltnplot <- function(q, min, max, rounding, main = NULL){
  rmin <- if (q < min) trunc(q - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > max) ceiling(q + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dunif(x, min, max)
  probx2 <- dunif(x2, min, max)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Uniform"), p[X](x) == frac(1,b-a)*","~~p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dunif(qq, min, max), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white", cex=0.8,
         legend = substitute("Parameters:"~ a == A ~ "," ~ b == B,
                             list( A = min, B = max)))
}

# Wilcoxon distribution
## Plot
plotpwilcoxltnplot <- function(q, m, n, rounding, main = NULL){
  rmin <- if (q < m+n) trunc(q - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > m+n) ceiling(q + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dwilcox(x, m, n)
  probx2 <- dwilcox(x2, m, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Wilcoxon"), p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dwilcox(qq, m, n), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~m == mv~";"~ n == nv,
                             list(mv = m, nv = n)))
}

# Signed Wilcoxon distribution
## Plot
plotpswilcoxltnplot <- function(q, n, rounding, main = NULL){
  rmin <- if (q < n) trunc(q - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
  if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
  rmax <- if (q > n) ceiling(q + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  x1 <- rmin:q
  x2 <- q
  probx <- dsignrank(x, n)
  probx2 <- dsignrank(x2, n)
  xlim <- c(rmin, rmax)
  ylim <- c(min(probx), max(probx)*1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5*(0:rmax))
  axis(2)
  title(ylab = expression(p[X](x)), xlab = "X",
        main = substitute(atop(bold("Probability function plot: Signed Wilcoxon"), p[X](t1) ~"="~ P(X == 20)),
                          list(t1 = q)))
  lines(x, probx, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
  points(x, probx, lwd = 2, pch = 19)
  lines(x2, probx2, type = "h", lwd = 2, col = "red")
  points(x2, probx2, lwd = 2, col = "red", pch = 19)
  # Mean
  #abline(v = lambda, lty = 2)
  qq <- round(q, digits = 2)
  qqaux <- round(q, digits = 2)
  Pr <- round(dsignrank(qq, n), rounding)
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
  legend(rmin, legaux$text$y, bty="n", bg = "white",cex = 0.8,
         legend = substitute("Parameters:"~n == nv,
                             list(nv = n)))
}
