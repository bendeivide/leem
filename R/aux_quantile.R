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

#####################
# Normal distribution
#####################

# CDF
plotqnormaltscdf <- function(p, mu, sigma, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qnorm(p, mu, sigma)
  curve(
    pnorm(x, mean = mu, sd = sigma),
    mu - 4 * sigma,
    mu + 4 * sigma,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Normal"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(mu - 4 * sigma, x[1], by = 0.01)
  y <- seq(x[1], mu + 4 * sigma, by = 0.01)
  fx <- pnorm(x, mu, sigma)
  fy <- pnorm(y, mu, sigma)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qnorm(p, mu, sigma), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  ##
  # X-axis => Q1
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux[1],
    labels = substitute(q == qtle, list(qtle = qqaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # X-axis => Q2
  axis(
    side = 1,
    at = qqaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qqaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P1
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob1, list(prob1 = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P2
  axis(
    side = 2,
    at = qq[2],
    labels = substitute("p*" == prob2, list(prob2 = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux,
           0,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(
      Q(p == p1 ~ "; " ~ mu == media ~ "," ~ sigma == varen) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        media = mu,
        varen = sigma
      )
    ),
    cex = 0.8
  )
  legend(
    par("usr")[1],
    1.18,
    bty = "n",
    col = "blue",
    pch = 16,
    legend = substitute(
      Q[S](p == p1 ~ "; " ~ mu == media ~ "," ~ sigma == varen) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        media = mu,
        varen = sigma
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqnormaltspdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <-
    if (q[1] <= mu - 4 * sigma)
      q[1] - 4 * sigma
  else
    mu - 4 * sigma
  maximo <-
    if (q[2] > mu + 4 * sigma)
      q[2] + 4 * sigma
  else
    mu + 4 * sigma
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fz <- dnorm(z, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Normal"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dnorm(x, mean = mu, sd = sigma),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy, fz)),
    xlab = "X",
    ylab = expression(f[X](x)),
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
          col = "red")
  abline(v = mu, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pnorm(
        q[1],
        mean = mu,
        sd = sigma,
        lower.tail = T
      ) + pnorm(
        q[2],
        mean = mu,
        sd = sigma,
        lower.tail = F
      ),
      digits = rounding
    )
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qq[1],
    label = substitute(q == qtle, list(qtle = qq[1])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qq[2],
    label = substitute(q[S] == qtle2, list(qtle2 = qq[2])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qq[1])),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qq[2], maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qq, lty = 2, col = "red")
  # legend("topleft", bty="n", fill="red",
  #        legend = substitute(atop(P(X<=t1~";" ~ mu == media ~ "," ~ sigma == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ mu == media ~ "," ~ sigma == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  rect(par("usr")[1],
       1.03 * max(fx, fy, fz),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  legaux <- legend(
    "topleft",
    bty = "n",
    fill = "red",
    bg = "white",
    legend = substitute(
      F[X](t1) + S[X](t2) == Pr,
      list(
        t1 = qq[1],
        t2 = qq[2],
        Pr = Pr,
        media = mu,
        varen = sigma,
        X = "X"
      )
    ),
    cex = 0.8
  )
  legend(
    minimo,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ mu == media ~ "," ~ sigma == varen,
      list(media = mu, varen = sigma)
    ),
    cex = 0.8
  )

}
plotqnormaltspdf <- function(p, mu, sigma, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qnorm(p, mu, sigma)
  plotqnormaltspdfaux(q[1] %<=X<=% q[2], mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqnormaltsboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqnormaltscdf(p, mu, sigma, rounding, ...)
  plotqnormaltspdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}






#####################
# T-Student distribution
#####################

# CDF
plotqtstudenttscdf <- function(p, df, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qt(p, df)
  curve(
    pt(x, df),
    -6,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: T-Student"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(df - 4 * df, x[1], by = 0.01)
  y <- seq(x[1], df + 4 * df, by = 0.01)
  fx <- pt(x, df)
  fy <- pt(y, df)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qt(p, df), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  ##
  # X-axis => Q1
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux[1],
    labels = substitute(q == qtle, list(qtle = qqaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # X-axis => Q2
  axis(
    side = 1,
    at = qqaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qqaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P1
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob1, list(prob1 = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P2
  axis(
    side = 2,
    at = qq[2],
    labels = substitute("p*" == prob2, list(prob2 = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux,
           0,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(
      Q(p == p1 ~ "; " ~ df == dfv) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        dfv = df
      )
    ),
    cex = 0.8
  )
  legend(
    par("usr")[1],
    1.18,
    bty = "n",
    col = "blue",
    pch = 16,
    legend = substitute(
      Q[S](p == p1~ "; " ~ df == dfv) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        dfv = df
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqtstudenttspdfaux <- function(q,df, rounding, ...) {
  minimo <- -6
  maximo <- 6
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dt(x, df)
  fz <- dt(z, df)
  fy <- dt(y, df)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: T-Student"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dt(x, df),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy, fz)),
    xlab = "X",
    ylab = expression(f[X](x)),
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
          col = "red")
  abline(v = df, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pt(
        q[1],
        df,
        lower.tail = T
      ) + pt(
        q[2],
        df,
        lower.tail = F
      ),
      digits = rounding
    )
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qq[1],
    label = substitute(q == qtle, list(qtle = qq[1])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qq[2],
    label = substitute(q[S] == qtle2, list(qtle2 = qq[2])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qq[1])),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qq[2], maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qq, lty = 2, col = "red")
  # legend("topleft", bty="n", fill="red",
  #        legend = substitute(atop(P(X<=t1~";" ~ mu == media ~ "," ~ sigma == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ mu == media ~ "," ~ sigma == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  rect(par("usr")[1],
       1.03 * max(fx, fy, fz),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  legaux <- legend(
    "topleft",
    bty = "n",
    fill = "red",
    bg = "white",
    legend = substitute(
      F[X](t1) + S[X](t2) == Pr,
      list(
        t1 = qq[1],
        t2 = qq[2],
        Pr = Pr,
        X = "X"
      )
    ),
    cex = 0.8
  )
  legend(
    minimo,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv,
      list(dfv = df)
    ),
    cex = 0.8
  )

}
plotqtstudenttspdf <- function(p, df, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qt(p, df)
  plotqtstudenttspdfaux(q[1] %<=X<=% q[2], df, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqtstudenttsboth <- function(p, df, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqtstudenttscdf(p, df, rounding, ...)
  plotqtstudenttspdf(p, df, rounding, ...)
  # Preserving the global variable
  par(op)
}







######################
# Poisson distribution
######################

# CDF
plotqpoissontscdf <- function(p, lambda, rounding, ...) {
  paux <- p
  paux <- c(p / 2, 1 - p / 2)
  qaux <- qpois(paux, lambda)
  paux2 <- c(ppois(qaux, lambda))
  rmin <- 0
  rmax <- ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  pointx <- ppois(x, lambda = lambda)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)

  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Normal"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    ...
  )
  points(x, ppois(x - 1, lambda = lambda), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(paux, digits = rounding)
  qqaux <- round(qpois(paux, lambda), digits = rounding)
  # X-axis: Q1
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qaux[1],
    labels = substitute(q == qtle, list(qtle = qaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = as.character(qaux[1]),
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # X-axis: Q2
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE
  )
  # Y-axis: P1
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob, list(prob = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE,
    lwd = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1,
    lwd = 0
  )
  # Y-axis: P2
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[2],
    labels = substitute(1 - "p*" == prob, list(prob = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  rect(par("usr")[1],
       1.03 * max(pointx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")

  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      ppois(w[i], lambda = lambda),
      w[i + 1],
      max(ppois(w[i], lambda = lambda)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(ppois(w[i + 1], lambda = lambda)),
             w[i + 1],
             max(ppois(w[i], lambda = lambda)),
             lty = 2,
             col = "black")
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    pch = 19,
    col = "red",
    legend = substitute(
      Q(p == p1 ~ "; " ~ lambda == lambd) == Qr,
      list(
        Qr = qaux[1],
        p = "p",
        p1 = qq[1],
        lambd = lambda
      )
    ),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    pch = 19,
    col = "blue",
    legend = substitute(
      Q[S]("p*" == p2 ~ "; " ~ lambda == lambd) == Qr,
      list(
        Qr = qaux[2],
        p = "p",
        p2 = 1-qq[2],
        lambd = lambda
      )
    ),
    cex = 0.8
  )
}

# PDF
plotqpoissontspdfaux <- function(q, lambda, rounding, ...) {
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
    if (q[1] >= q[2])
      stop("Lower limit must be less than upper limit",
           call. = FALSE,
           domain = "R-leem")
  }

  rmin <-
    if (q[1] < lambda) {
      trunc(q[1] - 4 * sqrt(lambda))
  } else {
    trunc(lambda - 4 * sqrt(lambda))
  }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q[2] > lambda) {
      ceiling(q[2] + 4 * sqrt(lambda))
    } else {
      ceiling(lambda + 4 * sqrt(lambda))
    }
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      ppois(q = q[1], lambda = lambda) + ppois(
        q = q[2] - 1,
        lambda = lambda,
        lower.tail = FALSE
      ),
      digits = rounding
    )
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) {
    qqmin
  } else {
    rmin:qqmin
  }
  x2 <- qqmax:rmax
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "red")
  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "red")
  # Mean
  #abline(v = lambda, lty = 2)
  # Point of survival function
  #abline(v = q[2] - 1, lty = 2, col = "blue")
  # blue x-axis
  # axis(
  #   side = 1,
  #   at = as.character(q[2] - 1),
  #   col = "blue",
  #   font = 2,
  #   tick = FALSE,
  #   lwd.ticks = 0,
  #   col.axis = "blue",
  #   pos = aux2
  # )
  # axis(
  #   side = 1,
  #   at = as.character(q[2] - 1),
  #   tick = TRUE,
  #   lwd = 0,
  #   col = "blue",
  #   font = 2,
  #   lwd.ticks = 1,
  #   labels = FALSE
  # )

  # red x-axis
  axis(
    side = 1,
    at = qqmin,
    labels = substitute(q == q1, list(q1 = qqmin)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qqmax,
    labels = substitute(q[S] == q2, list(q2 = qqmax)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(qqmax, rmax)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )

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

  # intervals
  abline(v = c(qqmin, qqmax),
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qqmin < 0) {
    axis(
      side = 1,
      at = as.character(qqmin),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)
        ),
        list(t1 = qqmin, t2 = qqmax, x = "x", t3 = qqmax -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) + S[X](t2) == Pr,
                          list(
                            t1 = qqmin, t2 = qqmax - 1, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
      at = as.character(c(rmin, qqmin)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == sum(p[X](x), x <= t1, "") * "," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)
        ),
        list(t1 = qqmin, t2 = qqmax, x = "x", t3 = qqmax -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) + S[X](t2) == Pr,
                          list(
                            t1 = qqmin, t2 = qqmax - 1, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  }
}
plotqpoissontspdf <- function(p, lambda, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qpois(p, lambda)
  plotqpoissontspdfaux(q[1] %<=X<=% q[2], lambda, rounding, ...)
}

# BOTH
plotqpoissontsboth <- function(p, lambda, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqpoissontscdf(p, lambda, rounding, ...)
  plotqpoissontspdf(p, lambda, rounding, ...)
  # Preserving the global variable
  par(op)
}



######################
# Binomial distribution
######################

# CDF
plotqbinomialtscdf <- function(p, size, prob, rounding, ...) {
  paux <- p
  paux <- c(p / 2, 1 - p / 2)
  qaux <- qbinom(paux, size, prob)
  paux2 <- c(pbinom(qaux, size, prob))
  rmin <- 0
  rmax <- ceiling(size)
  x <- rmin:rmax
  pointx <- pbinom(x, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)

  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Binomial"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    ...
  )
  points(x, pbinom(x - 1, size, prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(paux, digits = rounding)
  qqaux <- round(qbinom(paux, size, prob), digits = rounding)
  # X-axis: Q1
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qaux[1],
    labels = substitute(q == qtle, list(qtle = qaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = as.character(qaux[1]),
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # X-axis: Q2
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE
  )
  # Y-axis: P1
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob, list(prob = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE,
    lwd = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1,
    lwd = 0
  )
  # Y-axis: P2
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[2],
    labels = substitute(1 - "p*" == prob, list(prob = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  rect(par("usr")[1],
       1.03 * max(pointx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")

  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      pbinom(w[i], size, prob),
      w[i + 1],
      max(pbinom(w[i], size, prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pbinom(w[i + 1], size, prob)),
             w[i + 1],
             max(pbinom(w[i], size, prob)),
             lty = 2,
             col = "black")
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    pch = 19,
    col = "red",
    legend = substitute(
      Q(p == p1 ~ "; " ~ size == sizev ~ ";" ~ prob == probv) == Qr,
      list(
        Qr = qaux[1],
        p = "p",
        p1 = qq[1],
        sizev = size,
        probv = prob
      )
    ),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    pch = 19,
    col = "blue",
    legend = substitute(
      Q[S]("p*" == p2 ~ "; " ~ size == sizev ~ ";" ~ prob == probv) == Qr,
      list(
        Qr = qaux[2],
        p = "p",
        p2 = 1-qq[2],
        sizev = size,
        probv = prob
      )
    ),
    cex = 0.8
  )
}

# PDF
plotqbinomialtspdfaux <- function(q, size, prob, rounding, ...) {
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
    if (q[1] >= q[2])
      stop("Lower limit must be less than upper limit",
           call. = FALSE,
           domain = "R-leem")
  }

  rmin <-
    if (q[1] < size) {
      trunc(q[1] - 4 * sqrt(size))
    } else {
      trunc(size - 4 * sqrt(size))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q[2] > size) {
      ceiling(q[2] + 4 * sqrt(size))
    } else {
      ceiling(size + 4 * sqrt(size))
    }
  x <- rmin:rmax
  probx <- dbinom(x, size, prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pbinom(q = q[1], size, prob) + pbinom(
        q = q[2] - 1,
        size, prob,
        lower.tail = FALSE
      ),
      digits = rounding
    )
  qqmin <- qq[1]
  qqmax <- qq[2]
  # red vertical lines and points
  x1 <- if (rmin > qqmin) {
    qqmin
  } else {
    rmin:qqmin
  }
  x2 <- qqmax:rmax
  probx1 <- dbinom(x1, size, prob)
  probx2 <- dbinom(x2, size, prob)
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "red")
  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "red")
  # Mean
  #abline(v = lambda, lty = 2)
  # Point of survival function
  #abline(v = q[2] - 1, lty = 2, col = "blue")
  # blue x-axis
  # axis(
  #   side = 1,
  #   at = as.character(q[2] - 1),
  #   col = "blue",
  #   font = 2,
  #   tick = FALSE,
  #   lwd.ticks = 0,
  #   col.axis = "blue",
  #   pos = aux2
  # )
  # axis(
  #   side = 1,
  #   at = as.character(q[2] - 1),
  #   tick = TRUE,
  #   lwd = 0,
  #   col = "blue",
  #   font = 2,
  #   lwd.ticks = 1,
  #   labels = FALSE
  # )

  # red x-axis
  axis(
    side = 1,
    at = qqmin,
    labels = substitute(q == q1, list(q1 = qqmin)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qqmax,
    labels = substitute(q[S] == q2, list(q2 = qqmax)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(qqmax, rmax)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )

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

  # intervals
  abline(v = c(qqmin, qqmax),
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qqmin < 0) {
    axis(
      side = 1,
      at = as.character(qqmin),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)
        ),
        list(t1 = qqmin, t2 = qqmax, x = "x", t3 = qqmax -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) + S[X](t2) == Pr,
                          list(
                            t1 = qqmin, t2 = qqmax - 1, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ size == sizev ~ ";"~ prob == probv,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
      at = as.character(c(rmin, qqmin)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == sum(p[X](x), x <= t1, "") * "," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t2) == sum(p[X](x), x >= t2, infinity)
        ),
        list(t1 = qqmin, t2 = qqmax, x = "x", t3 = qqmax -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) + S[X](t2) == Pr,
                          list(
                            t1 = qqmin, t2 = qqmax - 1, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ size == sizev ~ ";"~ prob == probv,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqbinomialtspdf <- function(p, size, prob, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qbinom(p, size, prob)
  plotqbinomialtspdfaux(q[1] %<=X<=% q[2], size, prob, rounding, ...)
}

# BOTH
plotqbinomialtsboth <- function(p, size, prob, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqbinomialtscdf(p, size, prob, rounding, ...)
  plotqbinomialtspdf(p, size, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}






##########################
# Chi-Squared distribution
##########################

# CDF
plotqchisqtscdf <- function(p, df, ncp, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qchisq(p, df, ncp)
  curve(
    pchisq(x, df = df, ncp = ncp),
    0,
    ncp + 4 * df,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Chi-Squared"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(ncp - 4 * df, x[1], by = 0.01)
  y <- seq(x[1], ncp + 4 * df, by = 0.01)
  fx <- pchisq(x, df, ncp)
  fy <- pchisq(y, df, ncp)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  qq <- round(p, digits = rounding)
  qqaux <- round(qchisq(p, df, ncp), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux[1],
    labels = substitute(q == qtle, list(qtle = qqaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  axis(
    side = 1,
    at = qqaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qqaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob1, list(prob1 = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  axis(
    side = 2,
    at = qq[2],
    labels = substitute("p*" == prob2, list(prob2 = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )

  segments(qqaux,
           0,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(
      Q(p == p1 ~ "; " ~ df == dfv ~ "," ~ ncp == ncpv) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        dfv = df,
        ncpv = ncp
      )
    ),
    cex = 0.8
  )
  legend(
    par("usr")[1],
    1.18,
    bty = "n",
    col = "blue",
    pch = 16,
    legend = substitute(
      Q[S](p == p1 ~ "; " ~ df == dfv ~ "," ~ ncp == ncpv) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        dfv = df,
        ncpv = ncp
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqchisqtspdfaux <- function(q, df, ncp, rounding, ...) {
  minimo <- if (q[1] <=  ncp - 4 * df){
                q - 4 * sigma
            }
            else{0}

  maximo <-
    if (q[2] > ncp + 4 * df)
      q + 5 * df
  else
    ncp + 5 * df
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fz <- dchisq(z, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  main <-
    bquote(atop(
      bold("Probability density function plot: Chi-Squared"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dchisq(x, df = df, ncp = ncp),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy, fz)),
    xlab = "X",
    ylab = expression(f[X](x)),
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
          col = "red")
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pchisq(
        q[1],
        df = df,
        ncp = ncp,
        lower.tail = T
      ) +pchisq(
        q[2],
        df = df,
        ncp = ncp,
        lower.tail = F
      ),
      digits = rounding
    )
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qq[1],
    label = substitute(q == qtle, list(qtle = qq[1])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qq[2],
    label = substitute(q[S] == qtle2, list(qtle2 = qq[2])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qq[1])),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qq[2], maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qq, lty = 2, col = "red")
  # legend("topleft", bty="n", fill="red",
  #        legend = substitute(atop(P(X<=t1~";" ~ mu == media ~ "," ~ sigma == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ mu == media ~ "," ~ sigma == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  rect(par("usr")[1],
       1.03 * max(fx, fy, fz),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  legaux <- legend(
    "topleft",
    bty = "n",
    fill = "red",
    bg = "white",
    legend = substitute(
      F[X](t1) + S[X](t2) == Pr,
      list(
        t1 = qq[1],
        t2 = qq[2],
        Pr = Pr,
        X = "X"
      )
    ),
    cex = 0.8
  )
  legend(
    minimo,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv ~ "," ~ ncp == ncpv,
      list(dfv = df, ncpv = ncp)
    ),
    cex = 0.8
  )

}
plotqchisqtspdf <- function(p, df, ncp, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qchisq(p, df, ncp)
  plotqchisqtspdfaux(q[1] %<=X<=% q[2], df, ncp, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqchisqtsboth <- function(p, df, ncp, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqchisqtscdf(p, df, ncp, rounding, ...)
  plotqchisqtspdf(p, df, ncp, rounding, ...)
  # Preserving the global variable
  par(op)
}





#####################
# F distribution
#####################

# CDF
plotqftscdf <- function(p, df1, df2, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qf(p, df1,df2)
  curve(
    pf(x, df1, df2),
    0,
    10,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: F"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], df1 + 4 * df2, by = 0.01)
  fx <- pf(x, df1, df2)
  fy <- pf(y, df1, df2)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qf(p, df1, df2), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  ##
  # X-axis => Q1
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux[1],
    labels = substitute(q == qtle, list(qtle = qqaux[1])),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # X-axis => Q2
  axis(
    side = 1,
    at = qqaux[2],
    labels = substitute(q[S] == qtle, list(qtle = qqaux[2])),
    col.axis = "blue",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P1
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq[1],
    labels = substitute(p == prob1, list(prob1 = qq[1])),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[1],
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # Y-axis => P2
  axis(
    side = 2,
    at = qq[2],
    labels = substitute("p*" == prob2, list(prob2 = qq[2])),
    col.axis = "blue",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq[2],
    labels = FALSE,
    col.axis = "blue",
    col = "blue",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux,
           0,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = c("red", "blue"))
  points(qqaux, qq, pch = 16, col = c("red", "blue"))

  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(
      Q(p == p1 ~ "; " ~ df1 == df1v ~ "," ~ df2 == df2v) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        df1v = df1,
        df2v = df2
      )
    ),
    cex = 0.8
  )
  legend(
    par("usr")[1],
    1.18,
    bty = "n",
    col = "blue",
    pch = 16,
    legend = substitute(
      Q[S](p == p1 ~ "; " ~df1 == df1v ~ "," ~ df2 == df2v) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        df1v = df1,
        df2v = df2
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqftspdfaux <- function(q, df1, df2, rounding, ...) {
  minimo <- 0
  maximo <- 10

  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fz <- df(z, df1, df2)
  fy <- df(y, df1, df2)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: F"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    df(x, df1, df2),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy, fz)),
    xlab = "X",
    ylab = expression(f[X](x)),
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
          col = "red")
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pf(
        q[1],
        df1, df2,
        lower.tail = T
      ) + pf(
        q[2],
        df1, df2,
        lower.tail = F
      ),
      digits = rounding
    )
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qq[1],
    label = substitute(q == qtle, list(qtle = qq[1])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = qq[2],
    label = substitute(q[S] == qtle2, list(qtle2 = qq[2])),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    col.axis = "red",
    pos = aux2
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qq[1])),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qq[2], maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  abline(v = qq, lty = 2, col = "red")
  # legend("topleft", bty="n", fill="red",
  #        legend = substitute(atop(P(X<=t1~";" ~ mu == media ~ "," ~ sigma == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ mu == media ~ "," ~ sigma == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma, X ="X")))
  rect(par("usr")[1],
       1.03 * max(fx, fy, fz),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  legaux <- legend(
    "topleft",
    bty = "n",
    fill = "red",
    bg = "white",
    legend = substitute(
      F[X](t1) + S[X](t2) == Pr,
      list(
        t1 = qq[1],
        t2 = qq[2],
        Pr = Pr,
        X = "X"
      )
    ),
    cex = 0.8
  )
  legend(
    minimo,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                          list(df1v = df1, df2v = df2)),
    cex = 0.8
  )

}
plotqftspdf <- function(p, df1, df2, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qf(p, df1, df2)
  plotqftspdfaux(q[1] %<=X<=% q[2], df1, df2, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqftsboth <- function(p, df1, df2, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqftscdf(p, df1, df2, rounding, ...)
  plotqftspdf(p, df1, df2, rounding, ...)
  # Preserving the global variable
  par(op)
}






################################################################################
## lower.tail == TRUE (name: plot+q+name_distribution+ltt+type_distribution)
################################################################################
# OBS.: lt - lower.tail; ltt - lower.tail == TRUE;
#       type_distribution: cdf - cumulative distribution function;
#       pdf - probability density function; sf - survival function
#-------------------------------------------------------------------------------

#####################
# Normal distribution
#####################

# CDF
plotqnormalltcdf <- function(p, mu, sigma, rounding, ...) {
  x <- qnorm(p, mu, sigma)
  curve(
    pnorm(x, mean = mu, sd = sigma),
    mu - 4 * sigma,
    mu + 4 * sigma,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Normal"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(mu - 4 * sigma, x[1], by = 0.01)
  y <- seq(x[1], mu + 4 * sigma, by = 0.01)
  fx <- pnorm(x, mu, sigma)
  fy <- pnorm(y, mu, sigma)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qnorm(p, mu, sigma), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          p = "p", p1 = qq, Qr = qqaux
                        )),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ mu == media ~ "," ~ sigma == varen,
      list(media = mu, varen = sigma)
    ),
    cex = 0.8
  )
}

# PDF
plotqnormallttpdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <-
    if (q <=  mu - 4 * sigma)
      q - 4 * sigma
  else
    mu - 4 * sigma
  maximo <-
    if (q > mu + 4 * sigma)
      q + 4 * sigma
  else
    mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Normal"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dnorm(x, mean = mu, sd = sigma),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # Insert vertical line over the mean
  abline(v = mu, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pnorm(
      qq,
      mean = mu,
      sd = sigma,
      lower.tail = TRUE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qqaux)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(F[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ mu == media ~ "," ~ sigma == varen,
      list(media = mu, varen = sigma)
    ),
    cex = 0.8
  )
}
plotqnormallttpdf <- function(p, mu, sigma, rounding, ...) {
  q <- qnorm(p, mu, sigma)
  plotqnormallttpdfaux(q, mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqnormalttboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqnormalltcdf(p, mu, sigma, rounding, ...)
  plotqnormallttpdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}




########################
# T-Student distribution
########################

# CDF
plotqtstudentlttcdf <- function(p, df, rounding, ...) {
  x <- qt(p, df)
  curve(
    pt(x, df = df),
    -6,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: T-student"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(-6, x[1], by = 0.01)
  y <- seq(x[1], 6, by = 0.01)
  fx <- pt(x, df)
  fy <- pt(y, df)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qt(p, df), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          p = "p", p1 = qq, Qr = qqaux
                        )),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv,
      list(dfv =df)
    ),
    cex = 0.8
  )
}

# PDF
plotqtstudentlttpdfaux <- function(q, df, rounding, ...) {
  minimo <- -6
  maximo <- 6
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dt(x, df = df)
  fy <- dt(y, df = df)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: T-Student"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dt(x, df = df),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # Insert vertical line over the mean
  abline(v = df, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pt(
      qq,
      df = df,
      lower.tail = TRUE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qqaux)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(F[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv,
      list(dfv = df)
    ),
    cex = 0.8
  )
}
plotqtstudentlttpdf <- function(p, df, rounding, ...) {
  q <- qt(p, df)
  plotqtstudentlttpdfaux(q, df, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqtstudentlttboth <- function(p, df, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqtstudentlttcdf(p, df, rounding, ...)
  plotqtstudentlttpdf(p, df, rounding, ...)
  # Preserving the global variable
  par(op)
}





######################
# Poisson distribution
######################


#CDF
plotqpoissonlttcdf <- function(p, lambda, rounding) {
  rmin <- 0
  rmax <- ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  pointx <- ppois(x, lambda = lambda)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)

  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Poisson"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, ppois(x - 1, lambda = lambda), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qpois(p, lambda), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE,
    lwd.ticks = 0
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 40
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  rect(par("usr")[1],
       1.03 * max(pointx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")

  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      ppois(w[i], lambda = lambda),
      w[i + 1],
      max(ppois(w[i], lambda = lambda)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(ppois(w[i + 1], lambda = lambda)),
             w[i + 1],
             max(ppois(w[i], lambda = lambda)),
             lty = 2,
             col = "black")
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    pch = 19,
    col = "red",
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          Qr = qqaux, p = "p", p1 = qq
                        )), cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:" ~ lambda == lambd,
                        list(lambd = lambda)), cex = 0.8
  )

}

# PF
plotqpoissonlttpdfaux <- function(q, lambda, rounding, ...) {

  rmin <-
    if (q < lambda) {
      trunc(q - 4 * sqrt(lambda))
    } else {
      trunc(lambda - 4 * sqrt(lambda))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > lambda) {
      ceiling(q + 4 * sqrt(lambda))
    } else {
      ceiling(lambda + 4 * sqrt(lambda))
    }
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      ppois(q = q, lambda = lambda),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)

  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "black")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "red")
  axis(
    side = 1,
    at = qq,
    labels = substitute(q == q1, list(q1 = qq)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )

  axis(
    side = 1,
    at = as.character(qq),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )

  # intervals
  abline(v = qq,
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qq < 0) {
    axis(
      side = 1,
      at = as.character(qq),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t1) == sum(p[X](x), x >= t1, infinity)
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
     at = as.character(c(rmin, qq)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 0,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](q*"*") == sum(p[X](x), x <= q*"*", "")
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](q*"*"~"="~t1) == p[X](t1) ~"="~ Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  }
}
plotqpoissonlttpdf <- function(p, lambda, rounding, ...) {
  q <- qpois(p, lambda)
  plotqpoissonlttpdfaux(q, lambda, rounding, ...)
}

# BOTH
plotqpoissonlttboth <- function(p, lambda, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqpoissonlttcdf(p, lambda, rounding, ...)
  plotqpoissonlttpdf(p, lambda, rounding, ...)
  # Preserving the global variable
  par(op)
}




######################
# Binomial distribution
######################


#CDF
plotqbinomiallttcdf <- function(p, size, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  pointx <- pbinom(x, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)

  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Binomial"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, pbinom(x - 1, size, prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qbinom(p, size, prob), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE,
    lwd.ticks = 0
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 40
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  rect(par("usr")[1],
       1.03 * max(pointx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")

  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      pbinom(w[i], size, prob),
      w[i + 1],
      max(pbinom(w[i], size, prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pbinom(w[i + 1], size, prob)),
             w[i + 1],
             max(pbinom(w[i], size, prob)),
             lty = 2,
             col = "black")
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    pch = 19,
    col = "red",
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          Qr = qqaux, p = "p", p1 = qq
                        )),  cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == probv,
                        list(sizev = size, probv = prob)),  cex = 0.8
  )

}

# PF
plotqbinomiallttpdfaux <- function(q, size, prob, rounding, ...) {

  rmin <-
    if (q < size) {
      trunc(q - 4 * sqrt(size))
    } else {
      trunc(size - 4 * sqrt(size))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > size) {
      ceiling(q + 4 * sqrt(size))
    } else {
      ceiling(size + 4 * sqrt(size))
    }
  x <- rmin:rmax
  probx <- dbinom(x, size, prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pbinom(q = q, size, prob),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dbinom(x1, size, prob)
  probx2 <- dbinom(x2, size, prob)

  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "black")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "red")
  axis(
    side = 1,
    at = qq,
    labels = substitute(q == q1, list(q1 = qq)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )

  axis(
    side = 1,
    at = as.character(qq),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )

  # intervals
  abline(v = qq,
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qq < 0) {
    axis(
      side = 1,
      at = as.character(qq),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t1) == sum(p[X](x), x >= t1, infinity)
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ size == sizev~";" ~ prob == probv,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
      at = as.character(c(rmin, qq)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 0,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](q*"*") == sum(p[X](x), x <= q*"*", "")
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](q*"*"~"="~t1) == p[X](t1) ~"="~ Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ size == sizev~";" ~ prob == probv,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqbinomiallttpdf <- function(p, size, prob, rounding, ...) {
  q <- qbinom(p, size, prob)
  plotqbinomiallttpdfaux(q, size, prob, rounding, ...)
}

# BOTH
plotqbinomiallttboth <- function(p, size, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqbinomiallttcdf(p, size, prob, rounding, ...)
  plotqbinomiallttpdf(p, size, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}


##########################
# Chi-Squared distribution
##########################

# CDF
plotqchisqlttcdf <- function(p, df, ncp, rounding, ...) {
  x <- qchisq(p, df = df, ncp = ncp)
  curve(
    pchisq(x, df = df, ncp = ncp),
    0,
    ncp + 4 * df,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Chi-Squared"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(ncp - 4 * df, x[1], by = 0.01)
  y <- seq(x[1], ncp + 4 * df, by = 0.01)
  fx <- pchisq(x, df, ncp)
  fy <- pchisq(y, df, ncp)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qchisq(p, df, ncp), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          p = "p", p1 = qq, Qr = qqaux
                        )),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv ~ "," ~ ncp == ncpv,
      list(dfv = df, ncpv = ncp)
    ),
    cex = 0.8
  )
}

# PDF
plotqchisqlttpdfaux <- function(q, df, ncp, rounding, ...) {
  minimo <-
    if (q <=  ncp - 4 * df)
      q - 4 * sigma
  else
    0
  maximo <-
    if (q > ncp + 4 * df)
      q + 5 * df
  else
    ncp + 5 * df
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Chi-Squared"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dchisq(x, df = df, ncp = ncp),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pchisq(
      qq,
      df = df,
      ncp = ncp,
      lower.tail = TRUE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qqaux)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(F[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv ~ "," ~ ncp == ncpv,
      list(dfv = df, ncpv = ncp)
    ),
    cex = 0.8
  )
}
plotqchisqlttpdf <- function(p, df, ncp, rounding, ...) {
  q <- qchisq(p, df, ncp)
  plotqchisqlttpdfaux(q, df, ncp, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqchisqlttboth <- function(p, df, ncp, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqchisqlttcdf(p, df, ncp, rounding, ...)
  plotqchisqlttpdf(p, df, ncp, rounding, ...)
  # Preserving the global variable
  par(op)
}




#####################
# F distribution
#####################

# CDF
plotqflttcdf <- function(p, df1, df2, rounding, ...) {
  x <- qf(p, df1, df2)
  curve(
    pf(x, df1, df2),
    0,
   10,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: F"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], 4 * mean(df1+df2), by = 0.01)
  fx <- pf(x, df1, df2)
  fy <- pf(y, df1, df2)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qf(p, df1, df2), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute(p == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q(p == p1) == Qr,
                        list(
                          p = "p", p1 = qq, Qr = qqaux
                        )),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                        list(df1v = df1, df2v = df2)),
    cex = 0.8
  )
}

# PDF
plotqflttpdfaux <- function(q, df1, df2, rounding, ...) {
  minimo <- 0
  maximo <- 10
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fy <- df(y, df1, df2)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: F"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    df(x, df1, df2),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pf(
      qq,
      df1, df2,
      lower.tail = TRUE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(minimo, qqaux)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(F[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                          list(df1v = df1, df2v = df2)),
    cex = 0.8
  )
}
plotqflttpdf <- function(p, df1, df2, rounding, ...) {
  q <- qf(p, df1, df2)
  plotqflttpdfaux(q, df1, df2, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqflttboth <- function(p, df1, df2, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqflttcdf(p, df1, df2, rounding, ...)
  plotqflttpdf(p, df1, df2, rounding, ...)
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

######################
# Normal distribution
#####################

# Survival function (type == cdf)
plotqnormalltfsf <- function(p, mu, sigma, rounding, ...) {
  x <- qnorm(p, mu, sigma, lower.tail = FALSE)
  curve(
    pnorm(
      x,
      mean = mu,
      sd = sigma,
      lower.tail = FALSE
    ),
    mu - 4 * sigma,
    mu + 4 * sigma,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Normal"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(mu - 4 * sigma, x[1], by = 0.01)
  y <- seq(x[1], mu + 4 * sigma, by = 0.01)
  fx <- pnorm(x, mu, sigma, lower.tail = FALSE)
  fy <- pnorm(y, mu, sigma, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qnorm(p, mu, sigma, lower.tail = FALSE), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q[S]("p*" == p1) == Qr,
                        list(p1 = qq, Qr = qqaux)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ mu == media ~ "," ~ sigma == varen,
      list(media = mu, varen = sigma)
    ),
    cex = 0.8
  )
}


# PDF
plotqnormalltfpdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <-
    if (q <=  mu - 4 * sigma)
      q - 4 * sigma
  else
    mu - 4 * sigma
  maximo <-
    if (q > mu + 4 * sigma)
      q + 4 * sigma
  else
    mu + 4 * sigma
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dnorm(x, mean = mu, sd = sigma)
  fy <- dnorm(y, mean = mu, sd = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Normal"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dnorm(x, mean = mu, sd = sigma),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "red")
  # Insert vertical line over the mean
  abline(v = mu, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pnorm(
      qq,
      mean = mu,
      sd = sigma,
      lower.tail = FALSE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qqaux, maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(S[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ mu == media ~ "," ~ sigma == varen,
      list(media = mu, varen = sigma)
    ),
    cex = 0.8
  )
}
plotqnormalltfpdf <- function(p, mu, sigma, rounding, ...) {
  q <- qnorm(p, mu, sigma, lower.tail = FALSE)
  plotqnormalltfpdfaux(q, mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqnormaltfboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqnormalltfsf(p, mu, sigma, rounding, ...)
  plotqnormalltfpdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}





######################
# T-Student distribution
#####################

# Survival function (type == cdf)
plotqtstudentltfsf <- function(p, df, rounding, ...) {
  x <- qt(p, df, lower.tail = FALSE)
  curve(
    pt(
      x,
      df,
      lower.tail = FALSE
    ),
    -6,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: T-Student"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(-6, x[1], by = 0.01)
  y <- seq(x[1], 6, by = 0.01)
  fx <- pt(x, df, lower.tail = FALSE)
  fy <- pt(y, df, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qt(p, df, lower.tail = FALSE), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q[S]("p*" == p1) == Qr,
                        list(p1 = qq, Qr = qqaux)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv,
      list(dfv = df)
    ),
    cex = 0.8
  )
}


# PDF
plotqtstudentltfpdfaux <- function(q, df, rounding, ...) {
  minimo <- -6
  maximo <- 6
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dt(x, df = df)
  fy <- dt(y, df = df)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: T-Student"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dt(x, df = df),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "red")
  # Insert vertical line over the mean
  abline(v = df, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pt(
      qq,
      df = df,
      lower.tail = FALSE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qqaux, maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(S[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv,
      list(dfv = df)
    ),
    cex = 0.8
  )
}
plotqtstudentltfpdf <- function(p, df, rounding, ...) {
  q <- qt(p, df, lower.tail = FALSE)
  plotqtstudentltfpdfaux(q, df, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqtstudentltfboth <- function(p, df, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqtstudentltfsf(p, df, rounding, ...)
  plotqtstudentltfpdf(p, df, rounding, ...)
  # Preserving the global variable
  par(op)
}









######################
# Poisson distribution
######################
# Survival function
plotqpoissonlttsf <- function(p, lambda, rounding) {
  rmin <- 0
  rmax <- ceiling(lambda + 4 * sqrt(lambda))
  x <- rmin:rmax
  pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Poisson"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         ppois(x - 1, lambda = lambda, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qpois(p, lambda, lower.tail = FALSE), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 40
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      ppois(w[i], lambda = lambda, lower.tail = FALSE),
      w[i + 1],
      ppois(w[i], lambda = lambda, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      ppois(w[i + 1], lambda = lambda, lower.tail = FALSE),
      w[i + 1],
      ppois(w[i], lambda = lambda, lower.tail = FALSE),
      lty = 2,
      col = "black"
    )
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    ,
    legend = substitute(Q[S](p == p1) == Qr ~ "\n\n",
                        list(
                          Qr = qqaux, `p` = "p*", p1 = qq
                        )), cex = 0.8,
    pch = 19,
    col = "red"
  )
  legend(
    rmin,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameter:" ~ lambda == lambd,
                        list(lambd = lambda)),  cex = 0.8
  )

}

# PF
plotqpoissonltfpdfaux <- function(q, lambda, rounding, ...) {

  rmin <-
    if (q < lambda) {
      trunc(q - 4 * sqrt(lambda))
    } else {
      trunc(lambda - 4 * sqrt(lambda))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > lambda) {
      ceiling(q + 4 * sqrt(lambda))
    } else {
      ceiling(lambda + 4 * sqrt(lambda))
    }
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      ppois(q = q,
            lambda = lambda,
            lower.tail = FALSE
      ),
      digits = rounding
    )
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "black")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "red")
  axis(
    side = 1,
    at = qq,
    labels = substitute(q == q1, list(q1 = qq)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )

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

  # intervals
  abline(v = qq,
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qq < 0) {
    axis(
      side = 1,
      at = as.character(qq),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t1) == sum(p[X](x), x >= t1, infinity)
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
      at = as.character(c(q, rmax)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 0,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == sum(p[X](x), x <= t1, "")
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == p[X](t1) ~"="~ Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  }
}
plotqpoissonltfpdf <- function(p, lambda, rounding, ...) {
  q <- qpois(p, lambda, lower.tail = FALSE)
  plotqpoissonltfpdfaux(q, lambda, rounding, ...)
}

# BOTH
plotqpoissonltfboth <- function(p, lambda, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqpoissonlttsf(p, lambda, rounding, ...)
  plotqpoissonltfpdf(p, lambda, rounding, ...)
  # Preserving the global variable
  par(op)
}




######################
# Binomial distribution
######################
# Survival function
plotqbinomiallttsf <- function(p, size, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(size + 4 * sqrt(size))
  x <- rmin:rmax
  pointx <- pbinom(x, size, prob, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Binomial"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         pbinom(x - 1, size, prob, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qbinom(p, size, prob, lower.tail = FALSE), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 40
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    tick = FALSE
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  w <- c(par("usr")[1], x)
  for (i in 1:length(w)) {
    segments(
      w[i],
      pbinom(w[i], size, prob, lower.tail = FALSE),
      w[i + 1],
      pbinom(w[i], size, prob, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      pbinom(w[i + 1], size, prob, lower.tail = FALSE),
      w[i + 1],
      pbinom(w[i], size, prob, lower.tail = FALSE),
      lty = 2,
      col = "black"
    )
  }

  # Quantile
  segments(qqaux,
           par("usr")[3],
           qqaux,
           qq,
           lty = 2,
           col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")

  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    ,
    legend = substitute(Q[S](p == p1) == Qr ~ "\n\n",
                        list(
                          Qr = qqaux, `p` = "p*", p1 = qq
                        )),
    pch = 19,
    col = "red"
  )
  legend(
    rmin,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameter:" ~ size == sizev ~";"~ prob == probv,
                        list(sizev = size, probv = prob))
  )

}

# PF
plotqbinomialltfpdfaux <- function(q, size, prob, rounding, ...) {

  rmin <-
    if (q < size) {
      trunc(q - 4 * sqrt(size))
    } else {
      trunc(size - 4 * sqrt(size))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > lambda) {
      ceiling(q + 4 * sqrt(lambda))
    } else {
      ceiling(lambda + 4 * sqrt(lambda))
    }
  x <- rmin:rmax
  probx <- dpois(x, lambda = lambda)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      ppois(q = q,
            lambda = lambda,
            lower.tail = FALSE
      ),
      digits = rounding
    )
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dpois(x1, lambda = lambda)
  probx2 <- dpois(x2, lambda = lambda)
  lines(x1,
        probx1,
        type = "h",
        lwd = 2,
        col = "black")
  points(x1,
         probx1,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x2,
        probx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         probx2,
         lwd = 2,
         pch = 19,
         col = "red")
  axis(
    side = 1,
    at = qq,
    labels = substitute(q == q1, list(q1 = qq)),
    lwd = 0,
    col = "red",
    font = 2,
    tick = FALSE,
    col.axis = "red",
    pos = aux2
  )

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

  # intervals
  abline(v = qq,
         lty = 2,
         col = "red")
  # rectangle
  rect(par("usr")[1],
       1.03 * max(probx),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # title and legends
  if (qq < 0) {
    axis(
      side = 1,
      at = as.character(qq),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 1,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == 0*"," ~  ~ S[X](t3)*"="*1 - F[X](t3)*"="*P(X >= t1) == sum(p[X](x), x >= t1, infinity)
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  } else{
    axis(
      side = 1,
      at = as.character(c(q, rmax)),
      tick = TRUE,
      lwd = 1,
      col = "red",
      font = 2,
      lwd.ticks = 0,
      labels = FALSE
    )
    title(
      ylab = expression(p[X](x)),
      xlab = "X",
      main = substitute(
        atop(
          bold("Probability function plot: Poisson"),
          p[X](x) == frac(symbol(lambda) ^ x %*% e ^ -symbol(lambda), x * "!") *
            "," ~  ~ F[X](t1) == sum(p[X](x), x <= t1, "")
        ),
        list(t1 = qq, x = "x", t3 = qq -1)
      ),
      ...
    )
    # legends
    legaux <- legend(
      "topleft",
      bty = "n",
      fill = "red",
      legend = substitute(F[X](t1) == p[X](t1) ~"="~ Pr,
                          list(
                            t1 = qq, Pr = Pr
                          )), cex = 0.8
    )
    legend(
      rmin,
      legaux$text$y,
      bty = "n",
      bg = "white",
      legend = substitute("Parameters:" ~ lambda == lambd,
                          list(lambd = lambda)), cex = 0.8
    )
  }
}
plotqpoissonltfpdf <- function(p, lambda, rounding, ...) {
  q <- qpois(p, lambda, lower.tail = FALSE)
  plotqpoissonltfpdfaux(q, lambda, rounding, ...)
}

# BOTH
plotqpoissonltfboth <- function(p, lambda, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqpoissonlttsf(p, lambda, rounding, ...)
  plotqpoissonltfpdf(p, lambda, rounding, ...)
  # Preserving the global variable
  par(op)
}





##########################
# Chi-Squared distribution
##########################

# Survival function (type == cdf)
plotqchisqltfsf <- function(p, df, ncp, rounding, ...) {
  x <- qchisq(p, df, ncp, lower.tail = FALSE)
  curve(
    pchisq(
      x,
      df = df,
      ncp = ncp,
      lower.tail = FALSE
    ),
    0,
    ncp + 4 * df,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Chi-Squared"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], ncp + 4 * df, by = 0.01)
  fx <- pchisq(x, df, ncp, lower.tail = FALSE)
  fy <- pchisq(y, df, ncp, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qchisq(p, df, ncp, lower.tail = FALSE), digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q[S]("p*" == p1) == Qr,
                        list(p1 = qq, Qr = qqaux)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv ~ "," ~ ncp == ncpv,
      list(dfv = df, ncpv = ncp)
    ),
    cex = 0.8
  )
}


# PDF
plotqchisqltfpdfaux <- function(q, df, ncp, rounding, ...) {
  minimo <-
    if (q <=  ncp - 4 * df)
      q - 4 * sigma
  else
    0
  maximo <-
    if (q > ncp + 4 * df)
      q + 5 * df
  else
    ncp + 5 * df
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dchisq(x, df = df, ncp = ncp)
  fy <- dchisq(y, df = df, ncp = ncp)

  main <-
    bquote(atop(
      bold("Probability density function plot: Chi-Squared"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dchisq(x, df = df, ncp = ncp),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "red")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pchisq(
      qq,
      df = df,
      ncp = ncp,
      lower.tail = FALSE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qqaux, maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(S[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute(
      "Parameters:" ~ df == dfv ~ "," ~ ncp == ncpv,
      list(dfv = df, ncpv = ncp)
    ),
    cex = 0.8
  )
}
plotqchisqltfpdf <- function(p, df, ncp, rounding, ...) {
  q <- qchisq(p, df, ncp, lower.tail = FALSE)
  plotqchisqltfpdfaux(q, df, ncp, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqchisqltfboth <- function(p, df, ncp, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqchisqltfsf(p, df, ncp, rounding, ...)
  plotqchisqltfpdf(p, df, ncp, rounding, ...)
  # Preserving the global variable
  par(op)
}


######################
# F distribution
#####################

# Survival function (type == cdf)
plotqfltfsf <- function(p, df1, df2, rounding, ...) {
  x <- qf(p, df1, df2, lower.tail = FALSE)
  curve(
    pf(
      x,
      df1, df2,
      lower.tail = FALSE
    ),
    0,
    10,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: F"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], 4 * mean(df1+df2), by = 0.01)
  fx <- pf(x, df1, df2, lower.tail = FALSE)
  fy <- pf(y, df1, df2, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qf(p, df1, df2, lower.tail = FALSE), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col.axis = "red",
    font = 2,
    pos = aux2,
    tick = FALSE
  )
  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )
  # auxiliar variable
  aux <- par("usr")[1] - (par("usr")[2] - par("usr")[1]) / 20
  axis(
    side = 2,
    at = qq,
    labels = substitute("p*" == prob, list(prob = qq)),
    col.axis = "red",
    font = 2,
    pos = aux,
    lwd.ticks = 0
  )
  axis(
    side = 2,
    at = qq,
    labels = FALSE,
    col.axis = "red",
    col = "red",
    font = 2,
    tick = TRUE,
    lwd.ticks = 1
  )


  segments(qqaux, 0, qqaux, qq, lty = 2, col = "red")
  segments(par("usr")[1],
           qq,
           qqaux,
           qq,
           lty = 2,
           col = "red")
  points(qqaux, qq, pch = 16, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    pch = 16,
    legend = substitute(Q[S]("p*" == p1) == Qr,
                        list(p1 = qq, Qr = qqaux)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                        list(df1v = df1, df2v = df2)),
    cex = 0.8
  )
}


# PDF
plotqfltfpdfaux <- function(q, df1, df2, rounding, ...) {
  minimo <- 0
  maximo <- 10
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- df(x, df1, df2)
  fy <- df(y, df1, df2)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: F"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    df(x, df1, df2),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx, fy)),
    ylab = expression(f[X](x)),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = main,
    ...
  )
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "red")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pf(
      qq,
      df1, df2,
      lower.tail = FALSE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qtle, list(qtle = qqaux)),
    col = "red",
    font = 2,
    col.axis = "red",
    tick = FALSE,
    pos = aux2
  )
  # Insert red horizontal and vertical line (X-axis)
  axis(
    side = 1,
    at = as.character(qqaux),
    tick = TRUE,
    lwd = 0,
    col = "red",
    font = 2,
    lwd.ticks = 1,
    labels = FALSE
  )
  axis(
    side = 1,
    at = as.character(c(qqaux, maximo)),
    tick = TRUE,
    lwd = 1,
    col = "red",
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )
  abline(v = qqaux, lty = 2, col = "red")
  rect(par("usr")[1],
       1.03 * max(fx, fy),
       par("usr")[2],
       par("usr")[4],
       col = "gray")
  # Hint: https://www.statlect.com/fundamentals-of-probability/quantile
  legaux <- legend(
    "topleft",
    bty = "n",
    col = "red",
    fill = "red",
    legend = substitute(S[X](q1) == p,
                        list(q1 = qqaux, p = Pr)),
    cex = 0.8
  )
  legend(
    legaux$rect$left,
    legaux$text$y,
    bty = "n",
    bg = "white",
    legend = substitute("Parameters:"~df1 == df1v ~ "," ~ df2 == df2v,
                        list(df1v = df1, df2v = df2)),
    cex = 0.8
  )
}
plotqfltfpdf <- function(p, df1, df2, rounding, ...) {
  q <- qf(p, df1, df2, lower.tail = FALSE)
  plotqfltfpdfaux(q, df1, df2, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqfltfboth <- function(p, df1, df2, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqfltfsf(p, df1, df2, rounding, ...)
  plotqfltfpdf(p, df1, df2, rounding, ...)
  # Preserving the global variable
  par(op)
}




