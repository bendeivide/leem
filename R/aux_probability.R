###########################
# Auxiliar functions of P()
###########################
# Observations:
#    - `%<=X<=%`() internal function
################################################################################

################################################################################
## A-region (name: plot+p+name_distribution+ar+gui)
################################################################################
# OBS.: ar - A-region; gui: "plot", "rstudio", "tcltk"
#-------------------------------------------------------------------------------

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
    main <- gettext("Distribution: Normal", domain = "R-leem")
  }
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
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pnorm(q[1], mean = mu,sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd=sigma, lower.tail = F), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qq, lwd = 0,
       col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2)
  axis(side=1, at=as.character(c(minimo, qq[1])), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  axis(side=1, at=as.character(c(qq[2], maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 1, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region1") {
    legaux <- legend("topleft", bty="n", fill="red",
           legend = substitute(P(X<t1)+P(X>t2)==Pr,
                               list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty="n", fill="red",
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty="n", fill="red",
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty="n", fill="red",
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=qq[1],t2=qq[2], Pr = Pr)))
    legend(minimo, legaux$text$y, bty="n", bg = "white",
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = mu, varen = sigma)))
  }
} # plotcurve (older)

# RStudio and tcltk
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


################################################################################
## B-region (name: plot+p+name_distribution+br+gui)
################################################################################
# OBS.: br - B-region; gui: "plot", "rstudio", "tcltk"
#-------------------------------------------------------------------------------

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
    main <- gettext("Distribution: Normal", domain = "R-leem")
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylab = expression(f[X](x)), xlab = "X",
        ylim = c(0, 1.2 * max(fx,fy)),
        panel.first = grid(col="gray90"),
        main = main)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="red")
  abline(v=mu, lty=2)
  qq <- round(q, digits=2)
  qqaux <- qq
  Pr <- round(pnorm(q[2], mean = mu,sd = sigma, lower.tail = T) - pnorm(q[1], mean = mu, sd=sigma, lower.tail = T), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red")
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  if (attr(q, "region") == "region2") {
    legend("topleft", bty="n", fill="red",
           legend = substitute(P(t1<~X<~t2 ~ ";" ~ mu == media ~ "," ~ sigma == varen)==Pr~"\n\n",
                               list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
  }
  if (attr(q, "region") == "region4") {
    legend("topleft", bty="n", fill="red",
           legend = substitute(P(t1<=~X<=~t2 ~ ";" ~ mu == media ~ "," ~ sigma == varen)==Pr~"\n\n",
                               list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
  }
  if (attr(q, "region") == "region7") {
    legend("topleft", bty="n", fill="red",
           legend = substitute(P(t1<=~X<~t2 ~ ";" ~ mu == media ~ "," ~ sigma == varen)==Pr~"\n\n",
                               list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
  }
  if ( attr(q, "region") == "region8") {
    legend("topleft", bty="n", fill="red",
           legend = substitute(P(t1<~X<=~t2 ~ ";" ~ mu == media ~ "," ~ sigma == varen)==Pr~"\n\n",
                               list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
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



################################################################################
## lower.tail == TRUE (name: plot+q+name_distribution+ltt+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltt - lower.tail == TRUE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function
#-------------------------------------------------------------------------------


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
    main <- gettext("Distribution: Normal", domain = "R-leem")
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
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE), digits=rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point
  aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
  axis(side=1, at=qqaux, labels=qqaux,
       col="red", font = 2, col.axis = "red", tick = FALSE, pos = aux2)
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  abline(v = qqaux, lty=2, col = "red")
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legend("topleft", bty="n", fill="red",
         legend=substitute(P(X<= ~ q ~ ";" ~ mu ==  mean ~ "," ~ sigma == varen)==Pr, list(q = qq, Pr = Pr, mean = mu, varen = sigma)))
} # plotcurve (older)

# Plot
plotpnormallftplot <- function(q, mu, sigma, rounding, main = NULL) {
  minimo <- if (q <= mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  if (is.null(main)) {
    main <- gettext("Distribution: Normal", domain = "R-leem")
  }
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
  abline(v=mu, lty=2)
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
  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(qqaux, maximo)), tick = TRUE, lwd = 1,
       col="red", font = 2, lwd.ticks = 0, labels = FALSE)
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")
  legend("topleft", bty="n", fill="red",
         legend=substitute(P(X> ~ q ~ ";" ~ mu ==  mean ~ "," ~ sigma == varen)==Pr, list(q = qq, Pr = Pr, mean = mu, varen = sigma)))
} # plotcurve (older)

