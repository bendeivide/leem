###########################
# Auxiliar functions of Q()
###########################
# Observations:
#    - `%>X<=%`() internal function
################################################################################

################################################################################
## two.sided == TRUE (name: plot+q+name_distribution+ts+type_distribution)
################################################################################
# OBS.: ts - two.sided; type_distribution: cdf - cumulative distribution function
#       pdf - probability density function
#-------------------------------------------------------------------------------

# Normal distribution
#####################

# CDF
plotqnormaltscdf <- function(p, mu, sigma, rounding) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
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
  ##
  # X-axis => Q1
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side = 1, at = qqaux[1], labels = qqaux[1], col.axis = "red", font = 2, pos = aux2, tick = FALSE)
  axis(side = 1, at = qqaux[1], labels = FALSE, col.axis = "red", col = "red", font = 2, tick = TRUE, lwd.ticks = 1)
  # X-axis => Q2
  axis(side = 1, at = qqaux[2], labels = qqaux[2], col.axis = "blue", font = 2, pos = aux2, tick = FALSE)
  axis(side = 1, at = qqaux[2], labels = FALSE, col.axis = "blue", col = "blue", font = 2, tick = TRUE, lwd.ticks = 1)
  # Y-axis => P1
  aux <- par("usr")[1]-(par("usr")[2] - par("usr")[1])/20
  axis(side = 2, at = qq[1], labels = qq[1], col.axis = "red", font = 2, pos = aux, lwd.ticks = 0)
  axis(side = 2, at = qq[1], labels = FALSE, col.axis = "red", col = "red", font = 2, tick = TRUE, lwd.ticks = 1)
  # Y-axis => P2
  axis(side = 2, at = qq[2], labels = qq[2], col.axis = "blue", font = 2, pos = aux, lwd.ticks = 0)
  axis(side = 2, at = qq[2], labels = FALSE, col.axis = "blue", col = "blue", font = 2, tick = TRUE, lwd.ticks = 1)


  segments(qqaux, 0, qqaux, qq, lty = 2, col = c("red", "blue"))
  segments(par("usr")[1], qq, qqaux, qq, lty = 2, col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend("topleft", bty = "n", col = "red", pch = 16,
         legend = substitute(Q[1](p==p1 ~"; " ~ mu == media ~ ","~ sigma == varen) == Qr,
                             list(p = "p", p1 = p[1], Qr = qqaux[1], media = mu, varen = sigma)))
  legend(par("usr")[1], 1.18, bty = "n", col = "blue", pch = 16,
         legend = substitute(Q[2](p==p1 ~"; " ~ mu == media ~ ","~ sigma == varen) == Qr,
                             list(p = "p", p1 = p[2], Qr = qqaux[2], media = mu, varen = sigma)))
} # plotcurve (older)

# PDF
plotqnormaltspdfaux <- function(q, mu, sigma, rounding) {
  minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <-seq(minimo, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fz <- dnorm(z,mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <- gettext("Distribution: Normal", domain = "R-leem")
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
        ylab = expression(f[X](x)),
        panel.first = grid(col="gray90"),
        main = main)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
          col="red" )
  abline(v=mu, lty=2)
  qq <- round(q, digits=rounding)
  Pr <- round(pnorm(q[1], mean = mu,sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd=sigma, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, tick = TRUE, lwd = 0,
       col="red", font = 2, lwd.ticks = 0, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qq, lty=2, col = "red")
  # legend("topleft", bty="n", fill="red",
  #        legend = substitute(atop(P(X<=t1~";" ~ mu == media ~ "," ~ sigma == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ mu == media ~ "," ~ sigma == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  rect(par("usr")[1], 1.03 * max(fx,fy,fz), par("usr")[2], par("usr")[4], col = "gray")
  legaux <- legend("topleft", bty="n", fill="red", bg = "white",
                   legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  legend(minimo, legaux$text$y, bty="n", bg = "white",
         legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                             list(media = mu, varen = sigma)))

}
plotqnormaltspdf <- function(p, mu, sigma, rounding) {
  p <- c(p / 2, 1 - p / 2)
  q <- qnorm(p, mu, sigma)
  plotqnormaltspdfaux(q[1] %<=X<=% q[2], mu, sigma, rounding) # plotcurve2 (older)
}

# BOTH
plotqnormaltsboth <- function(p, mu, sigma, rounding, mfrow) {
  op <- par(mfrow = mfrow)
  plotqnormaltscdf(p, mu, sigma, rounding)
  plotqnormaltspdf(p, mu, sigma, rounding)
  # Preserving the global variable
  par(op)
}

# T-Student distribution
########################


################################################################################
## lower.tail == TRUE (name: plot+q+name_distribution+ltt+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltt - lower.tail == TRUE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------

# Normal distribution
#####################

# CDF
plotqnormalltcdf <- function(p, mu, sigma, rounding) {
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
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend("topleft", bty = "n", col = "red", pch = 16,
         legend = substitute(Q(p==p1 ~"; " ~ mu == media ~ ","~ sigma == varen) == Qr ~ "\n\n",
                             list(p = "p", p1 = qq, Qr = qqaux, media = mu, varen = sigma))
  )
}

# PDF
plotqnormallttpdfaux <- function(q, mu, sigma, rounding) {
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <- gettext("Distribution: Normal", domain = "R-leem")
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
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legend("topleft", bty="n", fill="red",
         legend=substitute(P(X<= ~ q ~ ";" ~ mu ==  mean ~ "," ~ sigma == varen)==Pr, list(q = qq, Pr = Pr, mean = mu, varen = sigma)))
}
plotqnormallttpdf <- function(p, mu, sigma, rounding) {
  q <- qnorm(p, mu, sigma)
  plotqnormallttpdfaux(q, mu, sigma, rounding) # plotcurve2 (older)
}

# BOTH
plotqnormalttboth <- function(p, mu, sigma, rounding, mfrow) {
  op <- par(mfrow = mfrow)
  plotqnormalltcdf(p, mu, sigma, rounding)
  plotqnormallttpdf(p, mu, sigma, rounding)
  # Preserving the global variable
  par(op)
}


################################################################################
## lower.tail == FALSE (name: plot+q+name_distribution+ltf+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltf - lower.tail == FALSE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------

# Normal distribution
#####################

# CDF
plotqnormalltfcdf <- function(p, mu, sigma, rounding) {

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
  qq <- round(1-p, digits = rounding)
  qqaux <- round(qnorm(p,mu,sigma, lower.tail = FALSE), digits = rounding)
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
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend("topleft", bty = "n", col = "red", pch = 16,
         legend = substitute(Q(p==p1 ~"; " ~ mu == media ~ ","~ sigma == varen) == Qr ~ "\n\n",
                             list(p = "p", p1 = qq, Qr = qqaux, media = mu, varen = sigma))
  )
}

# PDF
plotqnormalltfpdfaux <- function(q, mu, sigma, rounding) {
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <- gettext("Distribution: Normal", domain = "R-leem")
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="red")
  # Insert vertical line over the mean
  abline(v=mu, lty=2)
  qq <- round(q, digits=rounding)
  Pr <- round(pnorm(q,  mean = mu, sd=sigma, lower.tail = FALSE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, labels=qq,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qq, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qq, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legend("topleft", bty="n", fill="red",
         legend=substitute(P(X> ~ q ~ ";" ~ mu ==  mean ~ "," ~ sigma == varen)==Pr, list(q = qq, Pr = Pr, mean = mu, varen = sigma)))
}
plotqnormalltfpdf <- function(p, mu, sigma, rounding) {
  q <- qnorm(p, mu, sigma, lower.tail = FALSE)
  plotqnormalltfpdfaux(q, mu, sigma, rounding) # plotcurve2 (older)
}

# BOTH
plotqnormaltfboth <- function(p, mu, sigma, rounding, mfrow) {
  op <- par(mfrow = mfrow)
  plotqnormalltfcdf(p, mu, sigma, rounding)
  plotqnormalltfpdf(p, mu, sigma, rounding)
  # Preserving the global variable
  par(op)
}



