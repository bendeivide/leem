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
#---------------------------- Continuous Distributions -------------------------
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
    x[2]+5*sqrt(df),
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
  x <- seq(0, x[2]+5*sqrt(x[2]), by = 0.01)
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
  minimo <- if (q[1] <= ncp - 4 * df) ncp - 4 * df else 0
  maximo <- if (q[2] > ncp + 4 * df) q[2] + 4 * df else ncp + 4 * df
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





################
# F distribution
################

# CDF
plotqftscdf <- function(p, df1, df2, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qf(p, df1,df2)
  curve(
    pf(x, df1, df2),
    0,
    x[2] + 5*(df1/df2),
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
  x <- seq(0, x[2]+ 5*(df1/df2), by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- pf(x, df1, df2)
  fy <- pf(y, df1, df2)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
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
    ylim = auxmain,
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
       1.03 * auxrect,
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


#####################
# Gumbel distribution
#####################

# CDF
plotqgumbeltscdf <- function(p, location, scale, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qgumbel(p, location, scale)
  curve(
    pgumbel(x, location,scale),
    location - 10 * scale,
    location + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Culocationlative distribution plot: Gumbel"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(location - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], location + 10 * scale, by = 0.01)
  fx <- pgumbel(x, location, scale)
  fy <- pgumbel(y, location, scale)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qgumbel(p, location, scale), digits = rounding)
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqgumbeltspdfaux <- function(q, location, scale, rounding, ...) {
  minimo <-
    if (q[1] <= location - 10 * scale)
      q[1] - 10 * scale
  else
    location - 10 * scale
  maximo <-
    if (q[2] > location + 10 * scale)
      q[2] + 10 * scale
  else
    location + 10 * scale
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fz <- dgumbel(z, location, scale)
  fy <- dgumbel(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: gumbelal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Gumbel"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dgumbel(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pgumbel(
        q[1],
        location,
        scale,
        lower.tail = T
      ) + pgumbel(
        q[2],
        location,
        scale,
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
  #        legend = substitute(atop(P(X<=t1~";" ~ location == media ~ "," ~ scale == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ location == media ~ "," ~ scale == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = location, varen = scale, X ="X")))
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
        media = location,
        varen = scale,
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
      "Parameters:" ~ mu == locv ~ "," ~ beta == scalev,
      list(mu = location, scalev = scale)
    ),
    cex = 0.8
  )

}
plotqgumbeltspdf <- function(p, location, scale, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qgumbel(p, location, scale)
  plotqgumbeltspdfaux(q[1] %<=X<=% q[2], location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgumbeltsboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgumbeltscdf(p, location, scale, rounding, ...)
  plotqgumbeltspdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}

###################
# Beta distribution
###################

# CDF
plotqbetatscdf <- function(p, alpha, beta, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qbeta(p, alpha, beta)
  curve(
    pbeta(x, alpha, beta),
    0,
    1,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Beta"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], 1, by = 0.01)
  fx <- pbeta(x, alpha, beta)
  fy <- pbeta(y, alpha, beta)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qbeta(p, alpha, beta), digits = rounding)
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
      Q(p == p1 ~ "; " ~ alpha == alphav ~ "," ~ beta == betav) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        alphav = alpha,
        betav = beta
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
      Q[S](p == p1 ~ "; " ~ alpha == alphav ~ "," ~ beta == betav) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        alphav = alpha,
        betav = beta
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqbetatspdfaux <- function(q, alpha, beta, rounding, ...) {
  minimo <- 0
  maximo <- 1
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dbeta(x, alpha, beta)
  fz <- dbeta(z, alpha, beta)
  fy <- dbeta(y, alpha, beta)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Beta"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dbeta(x, alpha, beta),
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
      pbeta(
        q[1],
        alpha,
        beta,
        lower.tail = T
      ) + pbeta(
        q[2],
        alpha,
        beta,
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
      "Parameters:" ~ alpha == alphav ~ "," ~ beta == betav,
      list(alphav = alpha, betav = beta)
    ),
    cex = 0.8
  )

}
plotqbetatspdf <- function(p, alpha, beta, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qbeta(p, alpha, beta)
  plotqbetatspdfaux(q[1] %<=X<=% q[2], alpha, beta, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqbetatsboth <- function(p, alpha, beta, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqbetatscdf(p, alpha, beta, rounding, ...)
  plotqbetatspdf(p, alpha, beta, rounding, ...)
  # Preserving the global variable
  par(op)
}


##########################
# Exponential distribution
##########################

# CDF
plotqexptscrate <- function(p, rate, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qexp(p, rate)
  curve(
    pexp(x, rate),
    0,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Exponential"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(rate - 4 * rate, x[1], by = 0.01)
  y <- seq(x[1], rate + 4 * rate, by = 0.01)
  fx <- pexp(x, rate)
  fy <- pexp(y, rate)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qexp(p, rate), digits = rounding)
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
      Q(p == p1 ~ "; " ~ rate == ratev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        ratev = rate
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
      Q[S](p == p1~ "; " ~ rate == ratev) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        ratev = rate
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqexptsprateaux <- function(q,rate, rounding, ...) {
  minimo <- 0
  maximo <- 6
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dexp(x, rate)
  fz <- dexp(z, rate)
  fy <- dexp(y, rate)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Exponential"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dexp(x, rate),
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
      pexp(
        q[1],
        rate,
        lower.tail = T
      ) + pexp(
        q[2],
        rate,
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
      "Parameters:" ~ rate == ratev,
      list(ratev = rate)
    ),
    cex = 0.8
  )

}
plotqexptsprate <- function(p, rate, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qexp(p, rate)
  plotqexptsprateaux(q[1] %<=X<=% q[2], rate, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqexptsboth <- function(p, rate, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqexptscrate(p, rate, rounding, ...)
  plotqexptsprate(p, rate, rounding, ...)
  # Preserving the global variable
  par(op)
}


#####################
# Gamma distribution
#####################

# CDF
plotqgammatscdf <- function(p, shape, rate, scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- qgamma(p, shape, rate) + 4* sqrt(qgamma(p, shape, rate))
    paux <- p
    p <- c(p / 2, 1 - p / 2)
    x <- qgamma(p, shape,  scale = scale)
    curve(
      pgamma(x, shape, scale = scale),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(
        atop(
          bold("Cumulative distribution plot: Gamma"),
          Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
            inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
            1 - p
        )
      ),
      lwd = 4,
      ...
    )
    x <- seq(minimo, x[1], by = 0.01)
    y <- seq(x[1],maximo, by = 0.01)
    fx <- pgamma(x, shape, scale = scale)
    fy <- pgamma(y, shape, scale = scale)

    qqaux <- round(qgamma(p, shape, scale = scale), digits = rounding)
  }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- 0
    maximo <- qgamma(p, shape, rate) + 4* sqrt(qgamma(p, shape, rate))
    paux <- p
    p <- c(p / 2, 1 - p / 2)
    x <- qgamma(p, shape, rate)
    curve(
      pgamma(x, shape, rate),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(
        atop(
          bold("Cumulative distribution plot: Gamma"),
          Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
            inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
            1 - p
        )
      ),
      lwd = 4,
      ...
    )
    x <- seq(minimo, x[1], by = 0.01)
    y <- seq(x[1], maximo, by = 0.01)
    fx <- pgamma(x, shape, rate)
    fy <- pgamma(y, shape, rate)

    qqaux <- round(qgamma(p, shape, rate), digits = rounding)
  }

  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)

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
      Q(p == p1 ~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        shapev = shape,
        ratev = rate,
        scalev = scale
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
      Q[S](p == p1 ~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev) == Qr,
      list(
        p = "p",
        p1 = p[2],
        Qr = qqaux[2],
        shapev = shape,
        ratev = rate,
        scalev = scale
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqgammatspdfaux <- function(q, shape, rate, scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- q[2] + 4 * sqrt(q[2])
    x <- seq(minimo, q[1], by = 0.01)
    z <- seq(q[2], maximo, by = 0.01)
    y <- seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = scale)
    fz <- dgamma(z,shape, scale = scale)
    fy <- dgamma(y, shape, scale = scale)
    # if (!any(names(argaddit) == "main")) {
    #   main <- gettext("Distribution Function: gamma", domain = "R-leem")
    # } else {
    #   main <- argaddit$main
    # }
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
          integral(f[X](x) * dx, q[S], infinity)
      ))
    curve(
      dgamma(x, shape, scale = scale),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx, fy, fz)),
      xlab = "X",
      ylab = expression(f[X](x)),
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    Pr <-
      round(
        pgamma(
          q[1],
          shape,
          scale = scale,
          lower.tail = T
        ) + pgamma(
          q[2],
          shape,
          scale = scale,
          lower.tail = F
        ),
        digits = rounding
      )
  }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- 0
    maximo <- q[2] + 4 *  sqrt(q[2])
    x <- seq(minimo, q[1], by = 0.01)
    z <- seq(q[2], maximo, by = 0.01)
    y <- seq(minimo, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate)
    fz <- dgamma(z,shape, rate)
    fy <- dgamma(y, shape, rate)
    # if (!any(names(argaddit) == "main")) {
    #   main <- gettext("Distribution Function: gamma", domain = "R-leem")
    # } else {
    #   main <- argaddit$main
    # }
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
          integral(f[X](x) * dx, q[S], infinity)
      ))
    curve(
      dgamma(x, shape, rate),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx, fy, fz)),
      xlab = "X",
      ylab = expression(f[X](x)),
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    Pr <-
      round(
        pgamma(
          q[1],
          shape,
          rate,
          lower.tail = T
        ) + pgamma(
          q[2],
          shape,
          rate,
          lower.tail = F
        ),
        digits = rounding
      )
  }

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
      pgamma(
        q[1],
        shape,
        rate,
        lower.tail = T
      ) + pgamma(
        q[2],
        shape,
        rate,
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
      "Parameters:" ~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev,
      list(shapev = shape, ratev = rate, scalev = scale)
    ),
    cex = 0.8
  )

}
plotqgammatspdf <- function(p, shape, rate, scale, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  if(is.na(rate)){
    q <- qgamma(p, shape, scale = scale)
  }
  if(is.na(scale)){
    q <- qgamma(p, shape, rate)
  }

  plotqgammatspdfaux(q[1] %<=X<=% q[2], shape, rate, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgammatsboth <- function(p, shape, rate, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgammatscdf(p, shape, rate, scale, rounding, ...)
  plotqgammatspdf(p, shape, rate, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}

#####################
# Cauchy distribution
#####################

# CDF
plotqcauchytscdf <- function(p, location, scale, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qcauchy(p, location, scale)
  curve(
    pcauchy(x, location,scale),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Culocationlative distribution plot: Cauchy"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], scale + 10 * scale, by = 0.01)
  fx <- pcauchy(x, location, scale)
  fy <- pcauchy(y, location, scale)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qcauchy(p, location, scale), digits = rounding)
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqcauchytspdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fz <- dcauchy(z, location, scale)
  fy <- dcauchy(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: cauchyal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Cauchy"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dcauchy(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pcauchy(
        q[1],
        location,
        scale,
        lower.tail = T
      ) + pcauchy(
        q[2],
        location,
        scale,
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
  #        legend = substitute(atop(P(X<=t1~";" ~ location == media ~ "," ~ scale == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ location == media ~ "," ~ scale == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = location, varen = scale, X ="X")))
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
        media = location,
        varen = scale,
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
      "Parameters:" ~ mu == locv ~ "," ~ beta == scalev,
      list(mu = location, scalev = scale)
    ),
    cex = 0.8
  )

}
plotqcauchytspdf <- function(p, location, scale, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qcauchy(p, location, scale)
  plotqcauchytspdfaux(q[1] %<=X<=% q[2], location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqcauchytsboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqcauchytscdf(p, location, scale, rounding, ...)
  plotqcauchytspdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


#######################
# Logistic distribution
#######################

# CDF
plotqlogistscdf <- function(p, location, scale, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qlogis(p, location, scale)
  curve(
    plogis(x, location,scale),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Culocationlative distribution plot: Logistic"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], scale + 10 * scale, by = 0.01)
  fx <- plogis(x, location, scale)
  fy <- plogis(y, location, scale)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qlogis(p, location, scale), digits = rounding)
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
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
      Q(p == p1 ~ "; " ~ mu == locv ~ "," ~ beta == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = location,
        scalev = scale
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqlogistspdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fz <- dlogis(z, location, scale)
  fy <- dlogis(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: logisal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot:  Logistic"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dlogis(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      plogis(
        q[1],
        location,
        scale,
        lower.tail = T
      ) + plogis(
        q[2],
        location,
        scale,
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
  #        legend = substitute(atop(P(X<=t1~";" ~ location == media ~ "," ~ scale == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ location == media ~ "," ~ scale == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = location, varen = scale, X ="X")))
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
        media = location,
        varen = scale,
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
      "Parameters:" ~ mu == locv ~ "," ~ beta == scalev,
      list(mu = location, scalev = scale)
    ),
    cex = 0.8
  )

}
plotqlogistspdf <- function(p, location, scale, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qlogis(p, location, scale)
  plotqlogistspdfaux(q[1] %<=X<=% q[2], location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlogistsboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlogistscdf(p, location, scale, rounding, ...)
  plotqlogistspdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


#################################
# Logarithmic Normal distribution
#################################

# CDF
plotqlnormaltscdf <- function(p, mu, sigma, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qlnorm(p, mu, sigma)
  curve(
    plnorm(x, meanlog = mu, sdlog = sigma),
    0,
    x[2] + 4 * x[2],
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Logarithmic Normal"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  y <- seq(0, x[2] + 4 * x[2], by = 0.01)
  x <- seq(0, x[1], by = 0.01)
  fx <- plnorm(x, mu, sigma)
  fy <- plnorm(y, mu, sigma)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qlnorm(p, mu, sigma), digits = rounding)
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
plotqlnormaltspdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <- 0
  maximo <- q[2] + 4 * q[2]
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fz <- dlnorm(z, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: lnormal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Logarithmic Normal"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dlnorm(x, meanlog = mu, sdlog = sigma),
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
      plnorm(
        q[1],
        meanlog = mu,
        sdlog = sigma,
        lower.tail = T
      ) + plnorm(
        q[2],
        meanlog = mu,
        sdlog = sigma,
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
plotqlnormaltspdf <- function(p, mu, sigma, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qlnorm(p, mu, sigma)
  plotqlnormaltspdfaux(q[1] %<=X<=% q[2], mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlnormaltsboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlnormaltscdf(p, mu, sigma, rounding, ...)
  plotqlnormaltspdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}


#####################
# Weibull distribution
#####################

# CDF
plotqweibulltscdf <- function(p, shape, scale, rounding, ...) {
  paux <- p
  p <- c(p / 2, 1 - p / 2)
  x <- qweibull(p, shape, scale)
  curve(
    pweibull(x, shape,scale),
    0,
    x[2]+2*x[2],
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cushapelative distribution plot: Weibull"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    lwd = 4,
    ...
  )
  y <- seq(0, x[2]+2*x[2], by = 0.01)
  x <- seq(0, x[1], by = 0.01)
  fx <- pweibull(x, shape, scale)
  fy <- pweibull(y, shape, scale)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = shape, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qweibull(p, shape, scale), digits = rounding)
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
      Q(p == p1 ~ "; " ~ lambda == locv ~ "," ~ k == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = shape,
        scalev = scale
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
      Q(p == p1 ~ "; " ~ lambda == locv ~ "," ~ k == scalev) == Qr,
      list(
        p = "p",
        p1 = p[1],
        Qr = qqaux[1],
        locv = shape,
        scalev = scale
      )
    ),
    cex = 0.8
  )
} # plotcurve (older)

# PDF
plotqweibulltspdfaux <- function(q, shape, scale, rounding, ...) {
  minimo <- 0
  maximo <- q[2]+2*q[2]
  x <- seq(minimo, q[1], by = 0.01)
  z <- seq(q[2], maximo, by = 0.01)
  y <- seq(minimo, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fz <- dweibull(z, shape, scale)
  fy <- dweibull(y, shape, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: weibullal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Weibull"),
      F[X](q) == integral(f[X](x) * dx, infinity, q) * "," ~  ~ S[X](q[S]) ==
        integral(f[X](x) * dx, q[S], infinity)
    ))
  curve(
    dweibull(x, shape, scale),
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
  abline(v = shape, lty = 2)
  qq <- round(q, digits = rounding)
  Pr <-
    round(
      pweibull(
        q[1],
        shape,
        scale,
        lower.tail = T
      ) + pweibull(
        q[2],
        shape,
        scale,
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
  #        legend = substitute(atop(P(X<=t1~";" ~ shape == media ~ "," ~ scale == varen)~"+"~"\n\n\n\n\n", "+"~P(X>=t2~";" ~ shape == media ~ "," ~ scale == varen)==Pr),
  #                            list(t1=qq[1],t2=qq[2], Pr = Pr, media = shape, varen = scale, X ="X")))
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
        media = shape,
        varen = scale,
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
      "Parameters:" ~ lambda == locv ~ "," ~ k == scalev,
      list(locv = shape, scalev = scale)
    ),
    cex = 0.8
  )

}
plotqweibulltspdf <- function(p, shape, scale, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qweibull(p, shape, scale)
  plotqweibulltspdfaux(q[1] %<=X<=% q[2], shape, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqweibulltsboth <- function(p, shape, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqweibulltscdf(p, shape, scale, rounding, ...)
  plotqweibulltspdf(p, shape, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}









#----------------------------- Discrete Distributions --------------------------
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Poisson"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
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
          p[X](x) == bgroup("(",atop(n,x),")") *
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
          p[X](x) == bgroup("(",atop(n,x),")") *
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






################################
# Negative Binomial distribution
################################

# CDF
plotqnbinomtscdf <- function(p, size, prob, rounding, ...) {
  paux <- p
  paux <- c(p / 2, 1 - p / 2)
  qaux <- qnbinom(paux, size, prob)
  paux2 <- c(pnbinom(qaux, size, prob))
  rmin <- 0
  rmax <- ceiling(size + 10 * size)
  x <- rmin:rmax
  pointx <- pnbinom(x, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Negative Binomial"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    ...
  )
  points(x, pnbinom(x - 1, size, prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = size, lty = 2)
  qq <- round(paux, digits = rounding)
  qqaux <- round(qnbinom(paux, size, prob), digits = rounding)
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
      pnbinom(w[i], size, prob),
      w[i + 1],
      max(pnbinom(w[i], size, prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pnbinom(w[i + 1], size, prob)),
             w[i + 1],
             max(pnbinom(w[i], size, prob)),
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
      Q(p == p1 ~ "; " ~ size == sizev ~ "; " ~ prob == probv) == Qr,
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
      Q[S]("p*" == p2 ~ "; " ~ size == sizev ~ "; " ~ prob == probv) == Qr,
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
plotqnbinomtspdfaux <- function(q, size, prob, rounding, ...) {
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
      trunc(q[1] - 10 * sqrt(size))
    } else {
      trunc(size - 10 * sqrt(size))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q[2] > size) {
      ceiling(q[2] + 10 * sqrt(size))
    } else {
      ceiling(size + 10 * sqrt(size))
    }
  x <- rmin:rmax
  probx <- dnbinom(x, size, prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pnbinom(q = q[1], size, prob) + pnbinom(
        q = q[2] - 1,
        size,
        prob,
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
  probx1 <- dnbinom(x1, size, prob)
  probx2 <- dnbinom(x2, size, prob)
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
  #abline(v = size, lty = 2)
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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~";"~ prob == probv,
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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~";"~ prob == probv,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqnbinomtspdf <- function(p, size, prob, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qnbinom(p, size, prob)
  plotqnbinomtspdfaux(q[1] %<=X<=% q[2], size, prob, rounding, ...)
}

# BOTH
plotqnbinomtsboth <- function(p, size, prob, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqnbinomtscdf(p, size, prob, rounding, ...)
  plotqnbinomtspdf(p, size, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}



########################
# Geometric distribution
########################

# CDF
plotqgeomtscdf <- function(p, prob, rounding, ...) {
  paux <- p
  paux <- c(p / 2, 1 - p / 2)
  qaux <- qgeom(paux, prob)
  paux2 <- c(pgeom(qaux, prob))
  rmin <- 0
  rmax <- ceiling(qaux[2] + 10*paux[2])
  x <- rmin:rmax
  pointx <- pgeom(x, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Geometric"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    ...
  )
  points(x, pgeom(x - 1, prob = prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = prob, lty = 2)
  qq <- round(paux, digits = rounding)
  qqaux <- round(qgeom(paux, prob), digits = rounding)
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
      pgeom(w[i], prob = prob),
      w[i + 1],
      max(pgeom(w[i], prob = prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pgeom(w[i + 1], prob = prob)),
             w[i + 1],
             max(pgeom(w[i], prob = prob)),
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
      Q(p == p1 ~ "; " ~ prob == lambd) == Qr,
      list(
        Qr = qaux[1],
        p = "p",
        p1 = qq[1],
        lambd = prob
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
      Q[S]("p*" == p2 ~ "; " ~ prob == lambd) == Qr,
      list(
        Qr = qaux[2],
        p = "p",
        p2 = 1-qq[2],
        lambd = prob
      )
    ),
    cex = 0.8
  )
}

# PDF
plotqgeomtspdfaux <- function(q, prob, rounding, ...) {
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
    if (q[1] < prob) {
      trunc(q[1] - 4 * sqrt(prob))
    } else {
      trunc(prob - 4 * sqrt(prob))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- ceiling(q[2] + 4*q[2])
  x <- rmin:rmax
  probx <- dgeom(x, prob = prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pgeom(q = q[1], prob = prob) + pgeom(
        q = q[2] - 1,
        prob = prob,
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
  probx1 <- dgeom(x1, prob = prob)
  probx2 <- dgeom(x2, prob = prob)
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
  #abline(v = prob, lty = 2)
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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
    )
  }
}
plotqgeomtspdf <- function(p, prob, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qgeom(p, prob)
  plotqgeomtspdfaux(q[1] %<=X<=% q[2], prob, rounding, ...)
}

# BOTH
plotqgeomtsboth <- function(p, prob, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgeomtscdf(p, prob, rounding, ...)
  plotqgeomtspdf(p, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}




#############################
# Hypergeometric distribution
#############################

        # CDF
        plotqhypertscdf <- function(p, m, n, k, rounding, ...) {
          paux <- p
          paux <- c(p / 2, 1 - p / 2)
          qaux <- qhyper(paux, m, n, k)
          paux2 <- c(phyper(qaux, m, n, k))
          rmin <- 0
          rmax <- ceiling(qaux[2] + sqrt(k))
          x <- rmin:rmax
          pointx <- phyper(x, m, n, k)
          xlim <- c(rmin, rmax)
          ylim <- c(0, 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = x)
          axis(2)
          panel.first = grid(col = "gray90")
          title(
            ylab = expression(F[X](x)),
            xlab = "X",
            #panel.first = grid(col = "gray90"),
            main = bquote(
              atop(
                bold("Cumulative distribution plot: Hypergeometric"),
                Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
                  inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
                  1 - p
              )
            ),
            ...
          )
          points(x, phyper(x - 1, m, n, k), lwd = 2, pch = 1)
          points(x, pointx, lwd = 2, pch = 19)
          #abline(v = lambda, lty = 2)
          qq <- round(paux, digits = rounding)
          qqaux <- round(qhyper(paux, m, n, k), digits = rounding)
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
              phyper(w[i], m, n, k),
              w[i + 1],
              max(phyper(w[i], m, n, k)),
              lty = 1,
              col = "black"
            )
            segments(w[i + 1],
                     min(phyper(w[i + 1], m, n, k)),
                     w[i + 1],
                     max(phyper(w[i], m, n, k)),
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
              Q(p == p1 ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv) == Qr,
              list(
                Qr = qaux[1],
                p = "p",
                p1 = qq[1],
                mv = m,
                nv = n,
                kv = k
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
              Q[S]("p*" == p2 ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv) == Qr,
              list(
                Qr = qaux[2],
                p = "p",
                p2 = 1-qq[2],
                mv = m,
                nv = n,
                kv = k
              )
            ),
            cex = 0.8
          )
        }

        # PDF
        plotqhypertspdfaux <- function(q, m, n, k, rounding, ...) {
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
            if (q[1] < k) {
              trunc(q[1] - sqrt(k))
            } else {
              trunc(k - sqrt(k))
            }
          if (rmin < 0) {
            rmin <- 0
          } else {
            rmin <- round(rmin)
          }
          rmax <- ceiling(q[2] + sqrt(k))

          x <- rmin:rmax
          probx <- dhyper(x, m, n, k)

          xlim <- c(rmin, rmax)
          ylim <- c(0, max(probx) * 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = 5 * (0:rmax))
          axis(2)
          panel.first = grid(col = "gray90")
          points(
            x,
            probx,
            lwd = 2,
            pch = 19
            #panel.first = grid(col = "gray90")
          )
          lines(x, probx, type = "h", lwd = 2)
          qq <- round(q, digits = rounding)
          aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
          Pr <-
            round(
              phyper(q = q[1], m, n, k) + phyper(
                q = q[2] - 1,
                m, n, k,
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
          probx1 <- dhyper(x1, m, n, k)
          probx2 <- dhyper(x2, m, n, k)
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
                  bold("Probability function plot: Hypergeometric"),
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
              legend = substitute("Parameters:" ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                                  list(mv = m,
                                       nv = n,
                                       kv = k)), cex = 0.8
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
                  bold("Probability function plot: Hypergeometric"),
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
              legend = substitute("Parameters:" ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                                  list(mv = m,
                                       nv = n,
                                       kv = k)), cex = 0.8
            )
          }
        }
        plotqhypertspdf <- function(p, m, n, k, rounding, ...) {
          p <- c(p / 2, 1 - p / 2)
          q <- qhyper(p, m, n, k)
          plotqhypertspdfaux(q[1] %<=X<=% q[2], m, n, k, rounding, ...)
        }

        # BOTH
        plotqhypertsboth <- function(p, m, n, k, rounding, mfrow, ...) {
          op <- par(mfrow = mfrow)
          plotqhypertscdf(p, m, n, k, rounding, ...)
          plotqhypertspdf(p, m, n, k, rounding, ...)
          # Preserving the global variable
          par(op)
        }





######################
# Uniform distribution
######################

        # CDF
        plotquniftscdf <- function(p, min, max, rounding, ...) {
          paux <- p
          paux <- c(p / 2, 1 - p / 2)
          qaux <- qunif(paux, min, max)
          paux2 <- c(punif(qaux, min, max))
          rmin <-
            if (qaux[1] < min) {
              trunc(qaux[1] - 4 * sqrt(min))
            } else {
              trunc(min - 4 * sqrt(min))
            }
          if (rmin < 0) {
            rmin <- 0
          } else {
            rmin <- round(rmin)
          }
          rmax <- ceiling(max + 2 * sqrt(max))
          x <- rmin:rmax
          pointx <- punif(x, min, max)
          xlim <- c(rmin, rmax)
          ylim <- c(0, 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = x)
          axis(2)
          panel.first = grid(col = "gray90")
          title(
            ylab = expression(F[X](x)),
            xlab = "X",
            #panel.first = grid(col = "gray90"),
            main = bquote(
              atop(
                bold("Cumulative distribution plot: Uniform"),
                Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
                  inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
                  1 - p
              )
            ),
            ...
          )
          points(x, punif(x - 1, min, max), lwd = 2, pch = 1)
          points(x, pointx, lwd = 2, pch = 19)
          #abline(v = lambda, lty = 2)
          qq <- round(paux, digits = rounding)
          qqaux <- round(qunif(paux, min, max), digits = rounding)
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
            labels = substitute(p == max, list(max = qq[1])),
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
            labels = substitute(1 - "p*" == max, list(max = qq[2])),
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
              punif(w[i], min, max),
              w[i + 1],
              max(punif(w[i], min, max)),
              lty = 1,
              col = "black"
            )
            segments(w[i + 1],
                     min(punif(w[i + 1], min, max)),
                     w[i + 1],
                     max(punif(w[i], min, max)),
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

          # Hint: https://www.statlect.com/fundamentals-of-maxability/quantile
          legaux <- legend(
            "topleft",
            bty = "n",
            pch = 19,
            col = "red",
            legend = substitute(
              Q(p == p1 ~ "; " ~ min == minv ~ ";" ~ max == maxv) == Qr,
              list(
                Qr = qaux[1],
                p = "p",
                p1 = qq[1],
                minv = min,
                maxv = max
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
              Q[S]("p*" == p2 ~ "; " ~ min == minv ~ ";" ~ max == maxv) == Qr,
              list(
                Qr = qaux[2],
                p = "p",
                p2 = 1-qq[2],
                minv = min,
                maxv = max
              )
            ),
            cex = 0.8
          )
        }

        # PDF
        plotquniftspdfaux <- function(q, min, max, rounding, ...) {
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
            if (q[1] < min) {
              trunc(q[1] - 4 * sqrt(min))
            } else {
              trunc(min - 4 * sqrt(min))
            }
          if (rmin < 0) {
            rmin <- 0
          } else {
            rmin <- round(rmin)
          }
          rmax <-
            if (q[2] > min) {
              ceiling(q[2] + 4 * sqrt(min))
            } else {
              ceiling(min + 4 * sqrt(min))
            }
          x <- rmin:rmax
          maxx <- dunif(x, min, max)

          xlim <- c(rmin, rmax)
          ylim <- c(0, max(maxx) * 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = 5 * (0:rmax))
          axis(2)
          panel.first = grid(col = "gray90")
          points(
            x,
            maxx,
            lwd = 2,
            pch = 19
            #panel.first = grid(col = "gray90")
          )
          lines(x, maxx, type = "h", lwd = 2)
          qq <- round(q, digits = rounding)
          aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
          Pr <-
            round(
              punif(q = q[1], min, max) + punif(
                q = q[2] - 1,
                min, max,
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
          maxx1 <- dunif(x1, min, max)
          maxx2 <- dunif(x2, min, max)
          lines(x1,
                maxx1,
                type = "h",
                lwd = 2,
                col = "red")
          points(x1,
                 maxx1,
                 lwd = 2,
                 pch = 19,
                 col = "red")
          lines(x2,
                maxx2,
                type = "h",
                lwd = 2,
                col = "red")
          points(x2,
                 maxx2,
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
               1.03 * max(maxx),
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
                  bold("Probability function plot: Uniform"),
                  p[X](x) == frac(n*"!", x*"!"*(n-x)*"!") *
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
              legend = substitute("Parameters:" ~ min == minv ~ ";"~ max == maxv,
                                  list(minv = min, maxv = max)), cex = 0.8
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
                  bold("Probability function plot: Uniform"),
                  p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
              legend = substitute("Parameters:" ~ min == minv ~ ";"~ max == maxv,
                                  list(minv = min, maxv = max)), cex = 0.8
            )
          }
        }
        plotquniftspdf <- function(p, min, max, rounding, ...) {
          p <- c(p / 2, 1 - p / 2)
          q <- qunif(p, min, max)
          plotquniftspdfaux(q[1] %<=X<=% q[2], min, max, rounding, ...)
        }

        # BOTH
        plotquniftsboth <- function(p, min, max, rounding, mfrow, ...) {
          op <- par(mfrow = mfrow)
          plotquniftscdf(p, min, max, rounding, ...)
          plotquniftspdf(p, min, max, rounding, ...)
          # Preserving the global variable
          par(op)
        }






######################
# Wilcox distribution
######################

        # CDF
        plotqwilcoxtscdf <- function(p, m, n, rounding, ...) {
          paux <- p
          paux <- c(p / 2, 1 - p / 2)
          qaux <- qwilcox(paux, m, n)
          paux2 <- c(pwilcox(qaux, m, n))
          rmin <- 0
          rmax <- qaux[2]+2*qaux[2]
          x <- rmin:rmax
          pointx <- pwilcox(x, m, n)
          xlim <- c(rmin, rmax)
          ylim <- c(0, 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = x)
          axis(2)
          panel.first = grid(col = "gray90")
          title(
            ylab = expression(F[X](x)),
            xlab = "X",
            #panel.first = grid(col = "gray90"),
            main = bquote(
              atop(
                bold("Cumulative distribution plot: Wilcox"),
                Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
                  inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
                  1 - p
              )
            ),
            ...
          )
          points(x, pwilcox(x - 1, m, n), lwd = 2, pch = 1)
          points(x, pointx, lwd = 2, pch = 19)
          #abline(v = lambda, lty = 2)
          qq <- round(paux, digits = rounding)
          qqaux <- round(qwilcox(paux, m, n), digits = rounding)
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
            labels = substitute(p == n, list(n = qq[1])),
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
            labels = substitute(1 - "p*" == n, list(n = qq[2])),
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
              pwilcox(w[i], m, n),
              w[i + 1],
              max(pwilcox(w[i], m, n)),
              lty = 1,
              col = "black"
            )
            segments(w[i + 1],
                     min(pwilcox(w[i + 1], m, n)),
                     w[i + 1],
                     max(pwilcox(w[i], m, n)),
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

          # Hint: https://www.statlect.com/fundamentals-of-nability/quantile
          legaux <- legend(
            "topleft",
            bty = "n",
            pch = 19,
            col = "red",
            legend = substitute(
              Q(p == p1 ~ "; " ~ m == mv ~ ";" ~ n == nv) == Qr,
              list(
                Qr = qaux[1],
                p = "p",
                p1 = qq[1],
                mv = m,
                nv = n
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
              Q[S]("p*" == p2 ~ "; " ~ m == mv ~ ";" ~ n == nv) == Qr,
              list(
                Qr = qaux[2],
                p = "p",
                p2 = 1-qq[2],
                mv = m,
                nv = n
              )
            ),
            cex = 0.8
          )
        }

        # PDF
        plotqwilcoxtspdfaux <- function(q, m, n, rounding, ...) {
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
            if (q[1] < m) {
              trunc(q[1] - 4 * sqrt(m))
            } else {
              trunc(m - 4 * sqrt(m))
            }
          if (rmin < 0) {
            rmin <- 0
          } else {
            rmin <- round(rmin)
          }
          rmax <- ceiling(q[2] + 2 * q[2])
          x <- rmin:rmax
          nx <- dwilcox(x, m, n)

          xlim <- c(rmin, rmax)
          ylim <- c(0, max(nx) * 1.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1, at = 5 * (0:rmax))
          axis(2)
          panel.first = grid(col = "gray90")
          points(
            x,
            nx,
            lwd = 2,
            pch = 19
            #panel.first = grid(col = "gray90")
          )
          lines(x, nx, type = "h", lwd = 2)
          qq <- round(q, digits = rounding)
          aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
          Pr <-
            round(
              pwilcox(q = q[1], m, n) + pwilcox(
                q = q[2] - 1,
                m, n,
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
          nx1 <- dwilcox(x1, m, n)
          nx2 <- dwilcox(x2, m, n)
          lines(x1,
                nx1,
                type = "h",
                lwd = 2,
                col = "red")
          points(x1,
                 nx1,
                 lwd = 2,
                 pch = 19,
                 col = "red")
          lines(x2,
                nx2,
                type = "h",
                lwd = 2,
                col = "red")
          points(x2,
                 nx2,
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
               1.03 * max(nx),
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
                  bold("Probability function plot: Wilcox"),
                  p[X](x) == frac(n*"!", x*"!"*(n-x)*"!") *
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
              legend = substitute("Parameters:" ~ m == mv ~ ";"~ n == nv,
                                  list(mv = m, nv = n)), cex = 0.8
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
                  bold("Probability function plot: Wilcox"),
                  p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
              legend = substitute("Parameters:" ~ m == mv ~ ";"~ n == nv,
                                  list(mv = m, nv = n)), cex = 0.8
            )
          }
        }
        plotqwilcoxtspdf <- function(p, m, n, rounding, ...) {
          p <- c(p / 2, 1 - p / 2)
          q <- qwilcox(p, m, n)
          plotqwilcoxtspdfaux(q[1] %<=X<=% q[2], m, n, rounding, ...)
        }

        # BOTH
        plotqwilcoxtsboth <- function(p, m, n, rounding, mfrow, ...) {
          op <- par(mfrow = mfrow)
          plotqwilcoxtscdf(p, m, n, rounding, ...)
          plotqwilcoxtspdf(p, m, n, rounding, ...)
          # Preserving the global variable
          par(op)
        }






######################
# Signrank distribution
######################

# CDF
plotqsignranktscdf <- function(p, n, rounding, ...) {
  paux <- p
  paux <- c(p / 2, 1 - p / 2)
  qaux <- qsignrank(paux, n)
  paux2 <- c(psignrank(qaux, n))
  rmin <- 0
  rmax <- ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  pointx <- psignrank(x, n = n)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(
      atop(
        bold("Cumulative distribution plot: Signed-Rank Wilcox"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}") ~ "," ~ Q[S]("p*") ==
          inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") * "," ~ "p*" ==
          1 - p
      )
    ),
    ...
  )
  points(x, psignrank(x - 1, n = n), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = n, lty = 2)
  qq <- round(paux, digits = rounding)
  qqaux <- round(qsignrank(paux, n), digits = rounding)
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
      psignrank(w[i], n = n),
      w[i + 1],
      max(psignrank(w[i], n = n)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(psignrank(w[i + 1], n = n)),
             w[i + 1],
             max(psignrank(w[i], n = n)),
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
      Q(p == p1 ~ "; " ~ n == lambd) == Qr,
      list(
        Qr = qaux[1],
        p = "p",
        p1 = qq[1],
        lambd = n
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
      Q[S]("p*" == p2 ~ "; " ~ n == lambd) == Qr,
      list(
        Qr = qaux[2],
        p = "p",
        p2 = 1-qq[2],
        lambd = n
      )
    ),
    cex = 0.8
  )
}

# PDF
plotqsignranktspdfaux <- function(q, n, rounding, ...) {
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
    if (q[1] < n) {
      trunc(q[1] - 4 * sqrt(n))
    } else {
      trunc(n - 4 * sqrt(n))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q[2] > n) {
      ceiling(q[2] + 4 * sqrt(n))
    } else {
      ceiling(n + 4 * sqrt(n))
    }
  x <- rmin:rmax
  probx <- dsignrank(x, n = n)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      psignrank(q = q[1], n = n) + psignrank(
        q = q[2] - 1,
        n = n,
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
  probx1 <- dsignrank(x1, n = n)
  probx2 <- dsignrank(x2, n = n)
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
  #abline(v = n, lty = 2)
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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
    )
  }
}
plotqsignranktspdf <- function(p, n, rounding, ...) {
  p <- c(p / 2, 1 - p / 2)
  q <- qsignrank(p, n)
  plotqsignranktspdfaux(q[1] %<=X<=% q[2], n, rounding, ...)
}

# BOTH
plotqsignranktsboth <- function(p, n, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqsignranktscdf(p, n, rounding, ...)
  plotqsignranktspdf(p, n, rounding, ...)
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
#---------------------------- Continuous Distributions -------------------------
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





##########################
# Chi-Squared distribution
##########################

# CDF
plotqchisqlttcdf <- function(p, df, ncp, rounding, ...) {
  x <- qchisq(p, df = df, ncp = ncp)
  curve(
    pchisq(x, df = df, ncp = ncp),
    0,
    x+5*sqrt(df),
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




################
# F distribution
################

# CDF
plotqflttcdf <- function(p, df1, df2, rounding, ...) {
  x <- qf(p, df1, df2)
  curve(
    pf(x, df1, df2),
    0,
    x+10,
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
  x <- seq(0, x + 10, by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- pf(x, df1, df2)
  fy <- pf(y, df1, df2)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
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
    ylim = auxmain,
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
       1.03 * auxrect,
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




#####################
# Gumbel distribution
#####################

# CDF
plotqgumbellttcdf <- function(p, location, scale, rounding, ...) {
  x <- qgumbel(p, location, scale)
  curve(
    pgumbel(x, location, scale),
    scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Gumbel"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(scale - 10 * scale, scale + 10 * scale, by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- pgumbel(x, location, scale)
  fy <- pgumbel(y, location, scale)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qgumbel(p, location, scale), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}

# PDF
plotqgumbellttpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- if (q <=  scale - 10 * scale) q - 10 * scale else scale - 10 * scale
  maximo <- if (q > scale + 10 * scale) q + 10 * scale else scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fy <- dgumbel(y, location, scale)
  main <-
    bquote(atop(
      bold("Probability density function plot: Gumbel"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dgumbel(x, location, scale),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx,fy)),
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
    round(pgumbel(
      qq,
      location, scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqgumbellttpdf <- function(p, location, scale, rounding, ...) {
  q <- qgumbel(p, location, scale)
  plotqgumbellttpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgumbellttboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgumbellttcdf(p, location, scale, rounding, ...)
  plotqgumbellttpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


1

###################
# Beta distribution
###################

# CDF
plotqbetalttcdf <- function(p, alpha, beta, rounding, ...) {
  x <- qbeta(p, alpha, beta)
  curve(
    pbeta(x, alpha, beta),
    0,
    1,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cualphalative distribution plot: Beta"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], 1, by = 0.01)
  fx <- pbeta(x, alpha, beta)
  fy <- pbeta(y, alpha, beta)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = alpha, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qbeta(p, alpha, beta), digits = rounding)
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
      "Parameters:" ~ alpha == alphav ~ "," ~ beta == betav,
      list(alphav = alpha, betav = beta)
    ),
    cex = 0.8
  )
}

# PDF
plotqbetalttpdfaux <- function(q, alpha, beta, rounding, ...) {
  minimo <- 0
  maximo <- 1
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dbeta(x, alpha, beta)
  fy <- dbeta(y, alpha, beta)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: beta", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Beta"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dbeta(x, alpha, beta),
    0,
    1,
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
  abline(v = alpha, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pbeta(
      qq,
      alpha,
      beta,
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
      "Parameters:" ~ alpha == alphav ~ "," ~ beta == betav,
      list(alphav = alpha, betav = beta)
    ),
    cex = 0.8
  )
}
plotqbetalttpdf <- function(p, alpha, beta, rounding, ...) {
  q <- qbeta(p, alpha, beta)
  plotqbetalttpdfaux(q, alpha, beta, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqbetalttboth <- function(p, alpha, beta, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqbetalttcdf(p, alpha, beta, rounding, ...)
  plotqbetalttpdf(p, alpha, beta, rounding, ...)
  # Preserving the global variable
  par(op)
}



##########################
# Exponential distribution
##########################

# CDF
plotqexplttcrate <- function(p, rate, rounding, ...) {
  x <- qt(p, rate)
  curve(
    pexp(x, rate = rate),
    0,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Exponential"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(-6, x[1], by = 0.01)
  y <- seq(x[1], 6, by = 0.01)
  fx <- pexp(x, rate)
  fy <- pexp(y, rate)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qexp(p, rate), digits = rounding)
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
      "Parameters:" ~ rate == ratev,
      list(ratev =rate)
    ),
    cex = 0.8
  )
}

# PDF
plotqexplttprateaux <- function(q, rate, rounding, ...) {
  minimo <- 0
  maximo <- 6
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dexp(x, rate = rate)
  fy <- dexp(y, rate = rate)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Exponential"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dexp(x, rate = rate),
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
    round(pexp(
      qq,
      rate = rate,
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
      "Parameters:" ~ rate == ratev,
      list(ratev = rate)
    ),
    cex = 0.8
  )
}
plotqexplttprate <- function(p, rate, rounding, ...) {
  q <- qexp(p, rate)
  plotqexplttprateaux(q, rate, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqexplttboth <- function(p, rate, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqexplttcrate(p, rate, rounding, ...)
  plotqexplttprate(p, rate, rounding, ...)
  # Preserving the global variable
  par(op)
}





#####################
# Gamma distribution
#####################

# CDF
plotqgammalttcdf <- function(p, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- qgamma(p, shape, rate) + 4* sqrt(qgamma(p, shape, rate))
    x <- qgamma(p, shape, scale = scale)
    curve(
      pgamma(x, shape, scale = scale),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(atop(
        bold("Cumulative distribution plot: Gamma"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
      )),
      lwd = 4,
      ...
    )
    x <- seq(minimo, maximo, by = 0.01)
    y <- seq(x[1], x[2], by = 0.01)
    fx <- pgamma(x, shape, scale = scale)
    fy <- pgamma(y, shape, scale = scale)
    qqaux <- round(qgamma(p, shape, scale = scale), digits = rounding)
  }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- 0
    maximo <- qgamma(p, shape, rate) + 4* sqrt(qgamma(p, shape, rate))
    x <- qgamma(p, shape, rate)
    curve(
      pgamma(x, shape, rate),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(atop(
        bold("Cumulative distribution plot: Gamma"),
        Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
      )),
      lwd = 4,
      ...
    )
    x <- seq(minimo, maximo, by = 0.01)
    y <- seq(x[1], x[2], by = 0.01)
    fx <- pgamma(x, shape, rate)
    fy <- pgamma(y, shape, rate)
    qqaux <- round(qgamma(p, shape, rate), digits = rounding)
  }


  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)

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
    legend = substitute("Parameters:"~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev,
                        list(shapev = shape, ratev = rate, scalev = scale)),
    cex = 0.8
  )
}

# PDF
plotqgammalttpdfaux <- function(q, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- q + 4 * sqrt(q)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = scale)
    fy <- dgamma(y, shape, scale = scale)
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        F[X](q) == integral(f[X](x) * dx, infinity, q)
      ))
    curve(
      dgamma(x, shape, scale = scale),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx,fy)),
      ylab = expression(f[X](x)),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    qq <- round(q, digits = rounding)
    Pr <-
      round(pgamma(
        qq,
        shape, scale = scale,
        lower.tail = TRUE
      ), digits = rounding)

  }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- 0
    maximo <- q + 4 * sqrt(q)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate)
    fy <- dgamma(y, shape, rate)
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        F[X](q) == integral(f[X](x) * dx, infinity, q)
      ))
    curve(
      dgamma(x, shape, rate),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx,fy)),
      ylab = expression(f[X](x)),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    qq <- round(q, digits = rounding)
    Pr <-
      round(pgamma(
        qq,
        shape, rate,
        lower.tail = TRUE
      ), digits = rounding)

  }

  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "red")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)

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
    legend = substitute("Parameters:"~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev,
                        list(shapev = shape, ratev = rate, scalev = scale)),
    cex = 0.8
  )
}
plotqgammalttpdf <- function(p, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    q <- qgamma(p, shape, scale = scale)
  }
  if(is.na(scale)){
    q <- qgamma(p, shape, rate)
  }

  plotqgammalttpdfaux(q, shape, rate, scale = scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgammalttboth <- function(p, shape, rate, scale = scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgammalttcdf(p, shape, rate, scale = scale, rounding, ...)
  plotqgammalttpdf(p, shape, rate, scale = scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


#####################
# Cauchy distribution
#####################

# CDF
plotqcauchylttcdf <- function(p, location, scale, rounding, ...) {
  x <- qcauchy(p, location, scale)
  curve(
    pcauchy(x, location, scale),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Cauchy"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, scale + 10 * scale, by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- pcauchy(x, location, scale)
  fy <- pcauchy(y, location, scale)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qcauchy(p, location, scale), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}

# PDF
plotqcauchylttpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fy <- dcauchy(y, location, scale)
  main <-
    bquote(atop(
      bold("Probability density function plot: Cauchy"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dcauchy(x, location, scale),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx,fy)),
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
    round(pcauchy(
      qq,
      location, scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqcauchylttpdf <- function(p, location, scale, rounding, ...) {
  q <- qcauchy(p, location, scale)
  plotqcauchylttpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqcauchylttboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqcauchylttcdf(p, location, scale, rounding, ...)
  plotqcauchylttpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


1

#######################
# Logistic distribution
#######################

# CDF
plotqlogislttcdf <- function(p, location, scale, rounding, ...) {
  x <- qlogis(p, location, scale)
  curve(
    plogis(x, location, scale),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot:  Logistic"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, scale + 10 * scale, by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- plogis(x, location, scale)
  fy <- plogis(y, location, scale)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qlogis(p, location, scale), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}

# PDF
plotqlogislttpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fy <- dlogis(y, location, scale)
  main <-
    bquote(atop(
      bold("Probability density function plot:  Logistic"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dlogis(x, location, scale),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx,fy)),
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
    round(plogis(
      qq,
      location, scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqlogislttpdf <- function(p, location, scale, rounding, ...) {
  q <- qlogis(p, location, scale)
  plotqlogislttpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlogislttboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlogislttcdf(p, location, scale, rounding, ...)
  plotqlogislttpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}



#################################
# Logarithmic Normal distribution
#################################

# CDF
plotqlnormalltcdf <- function(p, mu, sigma, rounding, ...) {
  x <- qlnorm(p, mu, sigma)
  curve(
    plnorm(x, meanlog = mu, sdlog = sigma),
    0,
    x + 4 * x,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Logarithmic Normal"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  y <- seq(0, x + 4 * x, by = 0.01)
  x <- seq(0, x[1], by = 0.01)
  fx <- plnorm(x, mu, sigma)
  fy <- plnorm(y, mu, sigma)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qlnorm(p, mu, sigma), digits = rounding)
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
plotqlnormallttpdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <- 0
  maximo <- q + 4 * q
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: lnormal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Logarithmic Normal"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dlnorm(x, meanlog = mu, sdlog = sigma),
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
  # Insert vertical line over the meanlog
  abline(v = mu, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(plnorm(
      qq,
      meanlog = mu,
      sdlog = sigma,
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
plotqlnormallttpdf <- function(p, mu, sigma, rounding, ...) {
  q <- qlnorm(p, mu, sigma)
  plotqlnormallttpdfaux(q, mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlnormalttboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlnormalltcdf(p, mu, sigma, rounding, ...)
  plotqlnormallttpdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}




#####################
# Weibull distribution
#####################

# CDF
plotqweibulllttcdf <- function(p, shape, scale, rounding, ...) {
  x <- qweibull(p, shape, scale)
  curve(
    pweibull(x, shape, scale),
    0,
    x+2*x,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Weibull"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x+2*x, by = 0.01)
  y <- seq(x[1], x[1]+2*x[1], by = 0.01)
  fx <- pweibull(x, shape, scale)
  fy <- pweibull(y, shape, scale)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qweibull(p, shape, scale), digits = rounding)
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
    legend = substitute("Parameters:"~lambda == locv ~ "," ~ k == scav,
                        list(locv = shape, scav = scale)),
    cex = 0.8
  )
}

# PDF
plotqweibulllttpdfaux <- function(q, shape, scale, rounding, ...) {
  minimo <- 0
  maximo <- q + 2*q
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fy <- dweibull(y, shape, scale)
  main <-
    bquote(atop(
      bold("Probability density function plot: Weibull"),
      F[X](q) == integral(f[X](x) * dx, infinity, q)
    ))
  curve(
    dweibull(x, shape, scale),
    minimo,
    maximo,
    ylim = c(0, 1.2 * max(fx,fy)),
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
    round(pweibull(
      qq,
      shape, scale,
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
    legend = substitute("Parameters:"~lambda == locv ~ "," ~ k == scav,
                        list(locv = shape, scav = scale)),
    cex = 0.8
  )
}
plotqweibulllttpdf <- function(p, shape, scale, rounding, ...) {
  q <- qweibull(p, shape, scale)
  plotqweibulllttpdfaux(q, shape, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqweibulllttboth <- function(p, shape, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqweibulllttcdf(p, shape, scale, rounding, ...)
  plotqweibulllttpdf(p, shape, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}


1

#----------------------------- Discrete Distributions --------------------------
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
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
          p[X](x) == bgroup("(",atop(n,x),")")*theta^x*(1 - )*
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
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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


################################
# Negative Binomial distribution
################################


#CDF
plotqnbinomlttcdf <- function(p, size, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(size + 10 * size)
  x <- rmin:rmax
  pointx <- pnbinom(x, size, prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Negative Binomial"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, pnbinom(x - 1, size, prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = size, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qnbinom(p, size, prob), digits = rounding)
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
      pnbinom(w[i], size, prob),
      w[i + 1],
      max(pnbinom(w[i], size, prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pnbinom(w[i + 1], size, prob)),
             w[i + 1],
             max(pnbinom(w[i], size, prob)),
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
    legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
                        list(sizev = size, probv = prob)), cex = 0.8
  )

}

# PF
plotqnbinomlttpdfaux <- function(q, size, prob, rounding, ...) {

  rmin <-
    if (q < size) {
      trunc(q - 10 * size)
    } else {
      trunc(size - 10 * size)
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > size) {
      ceiling(q + 10 * size)
    } else {
      ceiling(size + 10 * size)
    }
  x <- rmin:rmax
  probx <- dnbinom(x, size, prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pnbinom(q = q, size, prob),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dnbinom(x1, size, prob)
  probx2 <- dnbinom(x2, size, prob)

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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqnbinomlttpdf <- function(p, size, prob, rounding, ...) {
  q <- qnbinom(p, size, prob)
  plotqnbinomlttpdfaux(q, size, prob, rounding, ...)
}

# BOTH
plotqnbinomlttboth <- function(p, size, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqnbinomlttcdf(p, size, prob, rounding, ...)
  plotqnbinomlttpdf(p, size, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}



########################
# Geometric distribution
########################


#CDF
plotqgeomlttcdf <- function(p, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(qgeom(p, prob) + 4*qgeom(p, prob))
  x <- rmin:rmax
  pointx <- pgeom(x, prob = prob)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Geometric"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, pgeom(x - 1, prob = prob), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = prob, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qgeom(p, prob), digits = rounding)
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
      pgeom(w[i], prob = prob),
      w[i + 1],
      max(pgeom(w[i], prob = prob)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pgeom(w[i + 1], prob = prob)),
             w[i + 1],
             max(pgeom(w[i], prob = prob)),
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
    legend = substitute("Parameters:" ~ prob == lambd,
                        list(lambd = prob)), cex = 0.8
  )

}

# PF
plotqgeomlttpdfaux <- function(q, prob, rounding, ...) {

  rmin <-
    if (q < prob) {
      trunc(q - 4 * sqrt(prob))
    } else {
      trunc(prob - 4 * sqrt(prob))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-  rmax <- ceiling(q + 4*q)

  x <- rmin:rmax
  probx <- dgeom(x, prob = prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pgeom(q = q, prob = prob),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dgeom(x1, prob = prob)
  probx2 <- dgeom(x2, prob = prob)

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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
    )
  }
}
plotqgeomlttpdf <- function(p, prob, rounding, ...) {
  q <- qgeom(p, prob)
  plotqgeomlttpdfaux(q, prob, rounding, ...)
}

# BOTH
plotqgeomlttboth <- function(p, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqgeomlttcdf(p, prob, rounding, ...)
  plotqgeomlttpdf(p, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}




#############################
# Hypergeometric distribution
#############################


#CDF
plotqhyperlttcdf <- function(p, m, n, k, rounding) {
  rmin <- 0
  rmax <- ceiling(qhyper(p, m, n, k) + sqrt(k))
  x <- rmin:rmax
  pointx <- phyper(x, m, n, k)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Hypergeometric"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, phyper(x - 1, m, n, k), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = m, n, k, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qhyper(p, m, n, k), digits = rounding)
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
      phyper(w[i], m, n, k),
      w[i + 1],
      max(phyper(w[i], m, n, k)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(phyper(w[i + 1], m, n, k)),
             w[i + 1],
             max(phyper(w[i], m, n, k)),
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
    legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                        list(mv = m, nv = n, kv = k)), cex = 0.8
  )

}

# PF
plotqhyperlttpdfaux <- function(q, m, n, k, rounding, ...) {

  rmin <-
    if (q < k) {
      trunc(q - sqrt(k))
    } else {
      trunc(k - sqrt(k))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- ceiling(q + sqrt(k))
  x <- rmin:rmax
  probx <- dhyper(x, m, n, k)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      phyper(q = q, m, n, k),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dhyper(x1, m, n, k)
  probx2 <- dhyper(x2, m, n, k)

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
          bold("Probability function plot: Hypergeometric"),
          p[X](x) == frac(symbol(m, n, k) ^ x %*% e ^ -symbol(m, n, k), x * "!") *
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
      legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                          list(mv = m, nv = n, kv = k)), cex = 0.8
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
          bold("Probability function plot: Hypergeometric"),
          p[X](x) == frac(symbol(m, n, k) ^ x %*% e ^ -symbol(m, n, k), x * "!") *
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
      legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                          list(mv = m, nv = n, kv = k)), cex = 0.8
    )
  }
}
plotqhyperlttpdf <- function(p, m, n, k, rounding, ...) {
  q <- qhyper(p, m, n, k)
  plotqhyperlttpdfaux(q, m, n, k, rounding, ...)
}

# BOTH
plotqhyperlttboth <- function(p, m, n, k, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqhyperlttcdf(p, m, n, k, rounding, ...)
  plotqhyperlttpdf(p, m, n, k, rounding, ...)
  # Preserving the global variable
  par(op)
}




######################
# Uniform distribution
######################


#CDF
plotquniflttcdf <- function(p, min, max, rounding) {
  rmin <- 0
  rmax <- ceiling(max + 2 * sqrt(max))
  x <- rmin:rmax
  pointx <- punif(x, min, max)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Uniform"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, punif(x - 1, min, max), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qunif(p, min, max), digits = rounding)
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
    labels = substitute(p == max, list(max = qq)),
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
      punif(w[i], min, max),
      w[i + 1],
      max(punif(w[i], min, max)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(punif(w[i + 1], min, max)),
             w[i + 1],
             max(punif(w[i], min, max)),
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

  # Hint: https://www.statlect.com/fundamentals-of-maxability/quantile
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
    legend = substitute("Parameters:" ~ min == minv ~ ";" ~ max == maxv,
                        list(minv = min, maxv = max)),  cex = 0.8
  )

}

# PF
plotquniflttpdfaux <- function(q, min, max, rounding, ...) {

  rmin <-
    if (q < min) {
      trunc(q - 4 * sqrt(min))
    } else {
      trunc(min - 4 * sqrt(min))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- max + 2 * sqrt(max)
  x <- rmin:rmax
  maxx <- dunif(x, min, max)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(maxx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    maxx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, maxx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      punif(q = q, min, max),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  maxx1 <- dunif(x1, min, max)
  maxx2 <- dunif(x2, min, max)

  lines(x2,
        maxx2,
        type = "h",
        lwd = 2,
        col = "black")
  points(x2,
         maxx2,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x1,
        maxx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         maxx1,
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
       1.03 * max(maxx),
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
          bold("Probability function plot: Uniform"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ min == minv~";" ~ max == maxv,
                          list(minv = min, maxv = max)), cex = 0.8
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
          bold("Probability function plot: Uniform"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ min == minv~";" ~ max == maxv,
                          list(minv = min, maxv = max)), cex = 0.8
    )
  }
}
plotquniflttpdf <- function(p, min, max, rounding, ...) {
  q <- qunif(p, min, max)
  plotquniflttpdfaux(q, min, max, rounding, ...)
}

# BOTH
plotquniflttboth <- function(p, min, max, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotquniflttcdf(p, min, max, rounding, ...)
  plotquniflttpdf(p, min, max, rounding, ...)
  # Preserving the global variable
  par(op)
}


######################
# Wilcox distribution
######################


#CDF
plotqwilcoxlttcdf <- function(p, m, n, rounding) {
  rmin <- 0
  rmax <- ceiling(qwilcox(p,m,n) + 2 * qwilcox(p,m,n))
  x <- rmin:rmax
  pointx <- pwilcox(x, m, n)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Wilcox"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, pwilcox(x - 1, m, n), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qwilcox(p, m, n), digits = rounding)
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
    labels = substitute(p == n, list(n = qq)),
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
      pwilcox(w[i], m, n),
      w[i + 1],
      max(pwilcox(w[i], m, n)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(pwilcox(w[i + 1], m, n)),
             w[i + 1],
             max(pwilcox(w[i], m, n)),
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

  # Hint: https://www.statlect.com/fundamentals-of-nability/quantile
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
    legend = substitute("Parameters:" ~ m == mv ~ ";" ~ n == nv,
                        list(mv = m, nv = n)),  cex = 0.8
  )

}

# PF
plotqwilcoxlttpdfaux <- function(q, m, n, rounding, ...) {

  rmin <-
    if (q < m) {
      trunc(q - 4 * sqrt(m))
    } else {
      trunc(m - 4 * sqrt(m))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- q + 2 * q
  x <- rmin:rmax
  nx <- dwilcox(x, m, n)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(nx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    nx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, nx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pwilcox(q = q, m, n),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  nx1 <- dwilcox(x1, m, n)
  nx2 <- dwilcox(x2, m, n)

  lines(x2,
        nx2,
        type = "h",
        lwd = 2,
        col = "black")
  points(x2,
         nx2,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x1,
        nx1,
        type = "h",
        lwd = 2,
        col = "red")
  points(x1,
         nx1,
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
       1.03 * max(nx),
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
          bold("Probability function plot: Wilcox"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ m == mv~";" ~ n == nv,
                          list(mv = m, nv = n)), cex = 0.8
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
          bold("Probability function plot: Wilcox"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ m == mv~";" ~ n == nv,
                          list(mv = m, nv = n)), cex = 0.8
    )
  }
}
plotqwilcoxlttpdf <- function(p, m, n, rounding, ...) {
  q <- qwilcox(p, m, n)
  plotqwilcoxlttpdfaux(q, m, n, rounding, ...)
}

# BOTH
plotqwilcoxlttboth <- function(p, m, n, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqwilcoxlttcdf(p, m, n, rounding, ...)
  plotqwilcoxlttpdf(p, m, n, rounding, ...)
  # Preserving the global variable
  par(op)
}
######################
# Signrank distribution
######################


#CDF
plotqsignranklttcdf <- function(p, n, rounding) {
  rmin <- 0
  rmax <- ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  pointx <- psignrank(x, n = n)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Cumulative distribution plot: Signed-Rank WIlcox"),
      Q(p) == inf ~ bgroup("{", x %in% R ~ ":" ~ p <= F(x), "}")
    )),
    cex.main = 1
  )
  points(x, psignrank(x - 1, n = n), lwd = 2, pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = n, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <- round(qsignrank(p, n), digits = rounding)
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
      psignrank(w[i], n = n),
      w[i + 1],
      max(psignrank(w[i], n = n)),
      lty = 1,
      col = "black"
    )
    segments(w[i + 1],
             min(psignrank(w[i + 1], n = n)),
             w[i + 1],
             max(psignrank(w[i], n = n)),
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
    legend = substitute("Parameters:" ~ n == lambd,
                        list(lambd = n)), cex = 0.8
  )

}

# PF
plotqsignranklttpdfaux <- function(q, n, rounding, ...) {

  rmin <-
    if (q < n) {
      trunc(q - 4 * sqrt(n))
    } else {
      trunc(n - 4 * sqrt(n))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > n) {
      ceiling(q + 4 * sqrt(n))
    } else {
      ceiling(n + 4 * sqrt(n))
    }
  x <- rmin:rmax
  probx <- dsignrank(x, n = n)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      psignrank(q = q, n = n),
      digits = rounding
    )

  # red vertical lines and points
  x1 <- if (rmin > qq) {
    qqmin
  } else {
    rmin:qq
  }
  x2 <- qq:rmax
  probx1 <- dsignrank(x1, n = n)
  probx2 <- dsignrank(x2, n = n)

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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
    )
  }
}
plotqsignranklttpdf <- function(p, n, rounding, ...) {
  q <- qsignrank(p, n)
  plotqsignranklttpdfaux(q, n, rounding, ...)
}

# BOTH
plotqsignranklttboth <- function(p, n, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqsignranklttcdf(p, n, rounding, ...)
  plotqsignranklttpdf(p, n, rounding, ...)
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
#---------------------------- Continuous Distributions -------------------------
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
    x+5*sqrt(df),
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
    x+10,
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
  x <- seq(0, x+10, by = 0.01)
  y <- seq(x[1], x[2], by = 0.01)
  fx <- pf(x, df1, df2, lower.tail = F)
  fy <- pf(y, df1, df2, lower.tail = F)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
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
    ylim = auxmain,
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
       1.03 * auxrect,
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





######################
# Gumbel distribution
#####################

# Survival function (type == cdf)
plotqgumbelltfsf <- function(p, location, scale, rounding, ...) {
  x <- qgumbel(p, location, scale, lower.tail = FALSE)
  curve(
    pgumbel(
      x,
      location,
      scale,
      lower.tail = FALSE
    ),
    location - 10 * scale,
    location + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Gumbel"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(location - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], location + 10 * scale, by = 0.01)
  fx <- pgumbel(x, location, scale, lower.tail = FALSE)
  fy <- pgumbel(y, location, scale, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qgumbel(p, location, scale, lower.tail = FALSE), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}


# PDF
plotqgumbelltfpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <-
    if (q <=  location - 10 * scale)
      q - 10 * scale
  else
    location - 10 * scale
  maximo <-
    if (q > location + 10 * scale)
      q + 10 * scale
  else
    location + 4 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dgumbel(x, location, scale)
  fy <- dgumbel(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: gumbelal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Gumbel"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dgumbel(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pgumbel(
      qq,
      location,
      scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqgumbelltfpdf <- function(p, location, scale, rounding, ...) {
  q <- qgumbel(p, location, scale, lower.tail = FALSE)
  plotqgumbelltfpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgumbelltfboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgumbelltfsf(p, location, scale, rounding, ...)
  plotqgumbelltfpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}





###################
# Beta distribution
###################

# Survival function (type == cdf)
plotqbetaltfsf <- function(p, alpha, beta, rounding, ...) {
  x <- qbeta(p, alpha, beta, lower.tail = FALSE)
  curve(
    pbeta(
      x,
      alpha,
      beta,
      lower.tail = FALSE
    ),
    0,
    1,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Beta"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], 1, by = 0.01)
  fx <- pbeta(x, alpha, beta, lower.tail = FALSE)
  fy <- pbeta(y, alpha, beta, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = alpha, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qbeta(p, alpha, beta, lower.tail = FALSE), digits = rounding)
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
      "Parameters:" ~ alpha == alphav ~ "," ~ beta == betav,
      list(alphav = alpha, betav = beta)
    ),
    cex = 0.8
  )
}


# PDF
plotqbetaltfpdfaux <- function(q, alpha, beta, rounding, ...) {
  minimo <- 0
  maximo <- 1
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dbeta(x, alpha, beta)
  fy <- dbeta(y, alpha, beta)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: beta", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Beta"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dbeta(x, alpha, beta),
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
  abline(v = alpha, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pbeta(
      qq,
      alpha,
      beta,
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
      "Parameters:" ~ alpha == alphav ~ "," ~ beta == betav,
      list(alphav = alpha, betav = beta)
    ),
    cex = 0.8
  )
}
plotqbetaltfpdf <- function(p, alpha, beta, rounding, ...) {
  q <- qbeta(p, alpha, beta, lower.tail = FALSE)
  plotqbetaltfpdfaux(q, alpha, beta, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqbetaltfboth <- function(p, alpha, beta, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqbetaltfsf(p, alpha, beta, rounding, ...)
  plotqbetaltfpdf(p, alpha, beta, rounding, ...)
  # Preserving the global variable
  par(op)
}

##########################
# Exponential distribution
##########################

# Survival function (type == cdf)
plotqexpltfsf <- function(p, rate, rounding, ...) {
  x <- qexp(p, rate, lower.tail = FALSE)
  curve(
    pexp(
      x,
      rate,
      lower.tail = FALSE
    ),
    0,
    6,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Exponential"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(-6, x[1], by = 0.01)
  y <- seq(x[1], 6, by = 0.01)
  fx <- pexp(x, rate, lower.tail = FALSE)
  fy <- pexp(y, rate, lower.tail = FALSE)
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
    round(qexp(p, rate, lower.tail = FALSE), digits = rounding)
  # Pr <- gsub("\\.", ",", Pr)
  # qq <- gsub("\\.", ",", qq)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qexple, list(qexple = qqaux)),
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
      "Parameters:" ~ rate == ratev,
      list(ratev = rate)
    ),
    cex = 0.8
  )
}


# PDF
plotqexpltfprateaux <- function(q, rate, rounding, ...) {
  minimo <- 0
  maximo <- 6
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dexp(x, rate = rate)
  fy <- dexp(y, rate = rate)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: Normal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Exponential"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dexp(x, rate = rate),
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
    round(pexp(
      qq,
      rate = rate,
      lower.tail = FALSE
    ), digits = rounding)
  #Pr <- gsub("\\.", ",", Pr)
  #qq <- gsub("\\.", ",", qq)
  # Insert red q point and vertical line (X-axis)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  axis(
    side = 1,
    at = qqaux,
    labels = substitute(q[S] == qexple, list(qexple = qqaux)),
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
      "Parameters:" ~ rate == ratev,
      list(ratev = rate)
    ),
    cex = 0.8
  )
}
plotqexpltfprate <- function(p, rate, rounding, ...) {
  q <- qexp(p, rate, lower.tail = FALSE)
  plotqexpltfprateaux(q, rate, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqexpltfboth <- function(p, rate, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqexpltfsf(p, rate, rounding, ...)
  plotqexpltfprate(p, rate, rounding, ...)
  # Preserving the global variable
  par(op)
}









######################
# Gamma distribution
#####################

# Survival function (type == cdf)
plotqgammaltfsf <- function(p, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- qgamma(p, shape, scale = scale) + 4* sqrt(qgamma(p, shape, scale = scale))
    x <- qgamma(p, shape, scale = scale, lower.tail = FALSE)
    curve(
      pgamma(
        x,
        shape, scale = scale,
        lower.tail = FALSE
      ),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(atop(
        bold("Survival function plot: Gamma"),
        Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
          "," ~ "p*" == 1 - p
      )),
      lwd = 4,
      ...
    )
    x <- seq(minimo, x[1], by = 0.01)
    y <- seq(x[1],maximo, by = 0.01)
    fx <- pgamma(x, shape, scale = scale, lower.tail = FALSE)
    fy <- pgamma(y, shape, scale = scale, lower.tail = FALSE)
    qqaux <-
      round(qgamma(p, shape, scale = scale, lower.tail = FALSE), digits = rounding)
  }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    minimo <- 0
    maximo <- qgamma(p, shape, rate) + 4* sqrt(qgamma(p, shape, rate))
    x <- qgamma(p, shape, rate, lower.tail = FALSE)
    curve(
      pgamma(
        x,
        shape, rate,
        lower.tail = FALSE
      ),
      minimo,
      maximo,
      ylab = expression(F[X](x)),
      ylim = c(0, 1.2),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = bquote(atop(
        bold("Survival function plot: Gamma"),
        Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
          "," ~ "p*" == 1 - p
      )),
      lwd = 4,
      ...
    )
    x <- seq(minimo, x[1], by = 0.01)
    y <- seq(x[1],maximo, by = 0.01)
    fx <- pgamma(x, shape, scale = scale, lower.tail = FALSE)
    fy <- pgamma(y, shape, scale = scale, lower.tail = FALSE)
    qqaux <-
      round(qgamma(p, shape, scale = scale, lower.tail = FALSE), digits = rounding)
  }

  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = mu, lty = 2)
  qq <- round(p, digits = rounding)

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
    legend = substitute("Parameters:"~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev,
                        list(shapev = shape, ratev = rate, scalev = scale)),
    cex = 0.8
  )
}


# PDF
plotqgammaltfpdfaux <- function(q, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- q + 4 * sqrt(q)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, scale = scale)
    fy <- dgamma(y, shape, scale = scale)
    # if (!any(names(argaddit) == "main")) {
    #   main <- gettext("Distribution Function: gamma", domain = "R-leem")
    # } else {
    #   main <- argaddit$main
    # }
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
      ))
    curve(
      dgamma(x, shape, scale = scale),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx, fy)),
      ylab = expression(f[X](x)),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    Pr <-
      round(pgamma(
        q,
        shape, scale = scale,
        lower.tail = FALSE
      ), digits = rounding)
    }
  if(is.na(scale)){
    auxarg <- rate
    scale <- 1/rate
    auxarg <- scale
    rate <- 1/scale
    minimo <- 0
    maximo <- q + 4 * sqrt(q)
    x <- seq(minimo, q, by = 0.01)
    y <- seq(q, maximo, by = 0.01)
    fx <- dgamma(x, shape, rate)
    fy <- dgamma(y, shape, rate)
    # if (!any(names(argaddit) == "main")) {
    #   main <- gettext("Distribution Function: gamma", domain = "R-leem")
    # } else {
    #   main <- argaddit$main
    # }
    main <-
      bquote(atop(
        bold("Probability density function plot: Gamma"),
        S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
      ))
    curve(
      dgamma(x, shape, rate),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(fx, fy)),
      ylab = expression(f[X](x)),
      xlab = "X",
      panel.first = grid(col = "gray90"),
      main = main,
      ...
    )
    Pr <-
      round(pgamma(
        q,
        shape, rate,
        lower.tail = FALSE
      ), digits = rounding)
  }

  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "red")
  # Insert vertical line over the mean
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
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
    legend = substitute("Parameters:"~ "; " ~ shape == shapev ~ "," ~ rate == ratev ~";"~ scale == scalev,
                        list(shapev = shape, ratev = rate, scalev = scale)),
    cex = 0.8
  )
}
plotqgammaltfpdf <- function(p, shape, rate, scale = scale, rounding, ...) {
  if(is.na(rate)){
    q <- qgamma(p, shape, scale = scale, lower.tail = FALSE)
  }
  if(is.na(scale)){
    q <- qgamma(p, shape, rate, lower.tail = FALSE)
  }
  plotqgammaltfpdfaux(q, shape, rate, scale = scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqgammaltfboth <- function(p, shape, rate, scale = scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqgammaltfsf(p, shape, rate, scale = scale, rounding, ...)
  plotqgammaltfpdf(p, shape, rate, scale = scale, rounding, ...)
  # Preserving the global variable
  par(op)
}







######################
# Cauchy distribution
#####################

# Survival function (type == cdf)
plotqcauchyltfsf <- function(p, location, scale, rounding, ...) {
  x <- qcauchy(p, location, scale, lower.tail = FALSE)
  curve(
    pcauchy(
      x,
      location,
      scale,
      lower.tail = FALSE
    ),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Cauchy"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], scale + 10 * scale, by = 0.01)
  fx <- pcauchy(x, location, scale, lower.tail = FALSE)
  fy <- pcauchy(y, location, scale, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qcauchy(p, location, scale, lower.tail = FALSE), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}


# PDF
plotqcauchyltfpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dcauchy(x, location, scale)
  fy <- dcauchy(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: cauchyal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Cauchy"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dcauchy(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pcauchy(
      qq,
      location,
      scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqcauchyltfpdf <- function(p, location, scale, rounding, ...) {
  q <- qcauchy(p, location, scale, lower.tail = FALSE)
  plotqcauchyltfpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqcauchyltfboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqcauchyltfsf(p, location, scale, rounding, ...)
  plotqcauchyltfpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}





#######################
# Logistic distribution
#######################

# Survival function (type == cdf)
plotqlogisltfsf <- function(p, location, scale, rounding, ...) {
  x <- qlogis(p, location, scale, lower.tail = FALSE)
  curve(
    plogis(
      x,
      location,
      scale,
      lower.tail = FALSE
    ),
    -scale - 10 * scale,
    scale + 10 * scale,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Logistic"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(-scale - 10 * scale, x[1], by = 0.01)
  y <- seq(x[1], scale + 10 * scale, by = 0.01)
  fx <- plogis(x, location, scale, lower.tail = FALSE)
  fy <- plogis(y, location, scale, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = location, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qlogis(p, location, scale, lower.tail = FALSE), digits = rounding)
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}


# PDF
plotqlogisltfpdfaux <- function(q, location, scale, rounding, ...) {
  minimo <- -scale - 10 * scale
  maximo <- scale + 10 * scale
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlogis(x, location, scale)
  fy <- dlogis(y, location, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: logisal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Logistic"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dlogis(x, location, scale),
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
  abline(v = location, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(plogis(
      qq,
      location,
      scale,
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
    legend = substitute("Parameters:"~mu == locv ~ "," ~ beta == scav,
                        list(locv = location, scav = scale)),
    cex = 0.8
  )
}
plotqlogisltfpdf <- function(p, location, scale, rounding, ...) {
  q <- qlogis(p, location, scale, lower.tail = FALSE)
  plotqlogisltfpdfaux(q, location, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlogisltfboth <- function(p, location, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlogisltfsf(p, location, scale, rounding, ...)
  plotqlogisltfpdf(p, location, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}





#################################
# Logarithmic Normal distribution
#################################

# Survival function (type == cdf)
plotqlnormalltfsf <- function(p, mu, sigma, rounding, ...) {
  x <- qlnorm(p, mu, sigma, lower.tail = FALSE)
  curve(
    plnorm(
      x,
      meanlog = mu,
      sdlog = sigma,
      lower.tail = FALSE
    ),
    0,
    x + 4 * x,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Logarithmic Normal"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  y <- seq(0, x + 4 * x, by = 0.01)
  x <- seq(0, x[1], by = 0.01)
  fx <- plnorm(x, mu, sigma, lower.tail = FALSE)
  fy <- plnorm(y, mu, sigma, lower.tail = FALSE)
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
    round(qlnorm(p, mu, sigma, lower.tail = FALSE), digits = rounding)
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
plotqlnormalltfpdfaux <- function(q, mu, sigma, rounding, ...) {
  minimo <- 0
  maximo <- q + 4 * q
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dlnorm(x, meanlog = mu, sdlog = sigma)
  fy <- dlnorm(y, meanlog = mu, sdlog = sigma)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: lnormal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Logarithmic Normal"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dlnorm(x, meanlog = mu, sdlog = sigma),
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
  # Insert vertical line over the sdloglog
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(plnorm(
      qq,
      meanlog = mu,
      sdlog = sigma,
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
plotqlnormalltfpdf <- function(p, mu, sigma, rounding, ...) {
  q <- qlnorm(p, mu, sigma, lower.tail = FALSE)
  plotqlnormalltfpdfaux(q, mu, sigma, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqlnormaltfboth <- function(p, mu, sigma, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqlnormalltfsf(p, mu, sigma, rounding, ...)
  plotqlnormalltfpdf(p, mu, sigma, rounding, ...)
  # Preserving the global variable
  par(op)
}





######################
# Weibull distribution
#####################

# Survival function (type == cdf)
plotqweibullltfsf <- function(p, shape, scale, rounding, ...) {
  x <- qweibull(p, shape, scale, lower.tail = FALSE)
  curve(
    pweibull(
      x,
      shape,
      scale,
      lower.tail = FALSE
    ),
    0,
    x + 2*x,
    ylab = expression(F[X](x)),
    ylim = c(0, 1.2),
    xlab = "X",
    panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Weibull"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    lwd = 4,
    ...
  )
  x <- seq(0, x[1], by = 0.01)
  y <- seq(x[1], x[1]+2*x[1], by = 0.01)
  fx <- pweibull(x, shape, scale, lower.tail = FALSE)
  fy <- pweibull(y, shape, scale, lower.tail = FALSE)
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = "gray90")
  # polygon(c(x, rev(x)),
  #         c(fx, rep(0, length(fx))),
  #         col = "red"
  # )
  #abline(v = shape, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qweibull(p, shape, scale, lower.tail = FALSE), digits = rounding)
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
    legend = substitute("Parameters:"~lambda == locv ~ "," ~ k == scav,
                        list(locv = shape, scav = scale)),
    cex = 0.8
  )
}


# PDF
plotqweibullltfpdfaux <- function(q, shape, scale, rounding, ...) {
  minimo <- 0
  maximo <- q + 2*q
  x <- seq(minimo, q, by = 0.01)
  y <- seq(q, maximo, by = 0.01)
  fx <- dweibull(x, shape, scale)
  fy <- dweibull(y, shape, scale)
  # if (!any(names(argaddit) == "main")) {
  #   main <- gettext("Distribution Function: weibullal", domain = "R-leem")
  # } else {
  #   main <- argaddit$main
  # }
  main <-
    bquote(atop(
      bold("Probability density function plot: Weibull"),
      S[X]("q*") == integral(f[X](x) * dx, "q*", infinity)
    ))
  curve(
    dweibull(x, shape, scale),
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
  abline(v = shape, lty = 2)
  qq <- round(q, digits = rounding)
  qqaux <- round(q, digits = rounding)
  Pr <-
    round(pweibull(
      qq,
      shape,
      scale,
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
    legend = substitute("Parameters:"~lambda == locv ~ "," ~ k == scav,
                        list(locv = shape, scav = scale)),
    cex = 0.8
  )
}
plotqweibullltfpdf <- function(p, shape, scale, rounding, ...) {
  q <- qweibull(p, shape, scale, lower.tail = FALSE)
  plotqweibullltfpdfaux(q, shape, scale, rounding, ...) # plotcurve2 (older)
}

# BOTH
plotqweibullltfboth <- function(p, shape, scale, rounding, mfrow, ...) {
  op <- par(mfrow = mfrow)
  plotqweibullltfsf(p, shape, scale, rounding, ...)
  plotqweibullltfpdf(p, shape, scale, rounding, ...)
  # Preserving the global variable
  par(op)
}





#----------------------------- Discrete Distributions --------------------------
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
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
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
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
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pbinom(q = q,
             size, prob,
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
  probx1 <- dbinom(x1, size, prob)
  probx2 <- dbinom(x2, size, prob)
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
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ size == sizev ~";"~prob == probv~".",
                          list(sizev = size, probv = prob)), cex = 0.8
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
          bold("Probability function plot: Binomial"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ size == sizev ~";"~prob == probv~".",
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqbinomialltfpdf <- function(p, size, prob, rounding, ...) {
  q <- qbinom(p, size, prob, lower.tail = FALSE)
  plotqbinomialltfpdfaux(q, size, prob, rounding, ...)
}

# BOTH
plotqbinomialltfboth <- function(p, size, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqbinomiallttsf(p, size, prob, rounding, ...)
  plotqbinomialltfpdf(p, size, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}





################################
# Negative Binomial distribution
################################
# Survival function
plotqnbinomlttsf <- function(p, size, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(size + 10 * size)
  x <- rmin:rmax
  pointx <- pnbinom(x, size, prob, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Negative Binomial"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         pnbinom(x - 1, size, prob, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = size, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qnbinom(p, size, lower.tail = FALSE), digits = rounding)
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
      pnbinom(w[i], size, prob, lower.tail = FALSE),
      w[i + 1],
      pnbinom(w[i], size, prob, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      pnbinom(w[i + 1], size, prob, lower.tail = FALSE),
      w[i + 1],
      pnbinom(w[i], size, prob, lower.tail = FALSE),
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
    legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
                        list(sizev = size, probv = prob)), cex = 0.8
  )

}

# PF
plotqnbinomltfpdfaux <- function(q, size, prob, rounding, ...) {

  rmin <-
    if (q < size) {
      trunc(q - 10* size)
    } else {
      trunc(size - 10 * size)
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > size) {
      ceiling(q + 10 * size)
    } else {
      ceiling(size + 10 * size)
    }
  x <- rmin:rmax
  probx <- dnbinom(x, size, prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pnbinom(q = q,
              size, prob,
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
  probx1 <- dnbinom(x1, size, prob)
  probx2 <- dnbinom(x2, size, prob)
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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
                          list(sizev = size, probv = prob)), cex = 0.8
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
          bold("Probability function plot: Negative Binomial"),
          p[X](x) == frac(symbol(size) ^ x %*% e ^ -symbol(size), x * "!") *
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
      legend = substitute("Parameters:" ~ size == sizev ~ ";" ~ prob == prob,
                          list(sizev = size, probv = prob)), cex = 0.8
    )
  }
}
plotqnbinomltfpdf <- function(p, size, prob, rounding, ...) {
  q <- qnbinom(p, size, prob, lower.tail = FALSE)
  plotqnbinomltfpdfaux(q, size, prob, rounding, ...)
}

# BOTH
plotqnbinomltfboth <- function(p, size, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqnbinomlttsf(p, size,prob, rounding, ...)
  plotqnbinomltfpdf(p, size,prob, rounding, ...)
  # Preserving the global variable
  par(op)
}




########################
# Geometric distribution
########################
# Survival function
plotqgeomlttsf <- function(p, prob, rounding) {
  rmin <- 0
  rmax <- ceiling(qgeom(p, prob) + 10*qgeom(p, prob))

  x <- rmin:rmax
  pointx <- pgeom(x, prob = prob, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Geometric"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         pgeom(x - 1, prob = prob, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = prob, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qgeom(p, prob, lower.tail = FALSE), digits = rounding)
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
      pgeom(w[i], prob = prob, lower.tail = FALSE),
      w[i + 1],
      pgeom(w[i], prob = prob, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      pgeom(w[i + 1], prob = prob, lower.tail = FALSE),
      w[i + 1],
      pgeom(w[i], prob = prob, lower.tail = FALSE),
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
    legend = substitute("Parameter:" ~ prob == lambd,
                        list(lambd = prob)),  cex = 0.8
  )

}

# PF
plotqgeomltfpdfaux <- function(q, prob, rounding, ...) {

  rmin <-
    if (q < prob) {
      trunc(q - 4 * sqrt(prob))
    } else {
      trunc(prob - 4 * sqrt(prob))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- q + 4*q
  x <- rmin:rmax
  probx <- dgeom(x, prob = prob)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pgeom(q = q,
            prob = prob,
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
  probx1 <- dgeom(x1, prob = prob)
  probx2 <- dgeom(x2, prob = prob)
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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
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
          bold("Probability function plot: Geometric"),
          p[X](x) == frac(symbol(prob) ^ x %*% e ^ -symbol(prob), x * "!") *
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
      legend = substitute("Parameters:" ~ prob == lambd,
                          list(lambd = prob)), cex = 0.8
    )
  }
}
plotqgeomltfpdf <- function(p, prob, rounding, ...) {
  q <- qgeom(p, prob, lower.tail = FALSE)
  plotqgeomltfpdfaux(q, prob, rounding, ...)
}

# BOTH
plotqgeomltfboth <- function(p, prob, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqgeomlttsf(p, prob, rounding, ...)
  plotqgeomltfpdf(p, prob, rounding, ...)
  # Preserving the global variable
  par(op)
}


#############################
# Hypergeometric distribution
#############################
# Survival function
plotqhyperlttsf <- function(p, m, n, k, rounding) {
  rmin <- 0
  rmax <- ceiling(qhyper(p, m, n, k) + sqrt(k))
  x <- rmin:rmax
  pointx <- phyper(x, m, n, k, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Hypergeometric"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         phyper(x - 1, m, n, k, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = m, n, k, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qhyper(p, m, n, k, lower.tail = FALSE), digits = rounding)
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
      phyper(w[i], m, n, k, lower.tail = FALSE),
      w[i + 1],
      phyper(w[i], m, n, k, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      phyper(w[i + 1], m, n, k, lower.tail = FALSE),
      w[i + 1],
      phyper(w[i], m, n, k, lower.tail = FALSE),
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
    legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                        list(mv = m, nv = n, kv = k)), cex = 0.8
  )

}

# PF
plotqhyperltfpdfaux <- function(q, m, n, k, rounding, ...) {

  rmin <-
    if (q < k) {
      trunc(q - sqrt(k))
    } else {
      trunc(k - sqrt(k))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- ceiling(q + sqrt(k))

  x <- rmin:rmax
  probx <- dhyper(x, m, n, k)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      phyper(q = q,
             m, n, k,
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
  probx1 <- dhyper(x1, m, n, k)
  probx2 <- dhyper(x2, m, n, k)
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
          bold("Probability function plot: Hypergeometric"),
          p[X](x) == frac(symbol(m, n, k) ^ x %*% e ^ -symbol(m, n, k), x * "!") *
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
      legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                          list(mv = m, nv = n, kv = k)), cex = 0.8
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
          bold("Probability function plot: Hypergeometric"),
          p[X](x) == frac(symbol(m, n, k) ^ x %*% e ^ -symbol(m, n, k), x * "!") *
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
      legend = substitute("Parameters:" ~ "; " ~ m == mv ~ "; " ~ n == nv ~ "; " ~ k == kv,
                          list(mv = m, nv = n, kv = k)), cex = 0.8
    )
  }
}
plotqhyperltfpdf <- function(p, m, n, k, rounding, ...) {
  q <- qhyper(p, m, n, k, lower.tail = FALSE)
  plotqhyperltfpdfaux(q, m, n, k, rounding, ...)
}

# BOTH
plotqhyperltfboth <- function(p, m, n, k, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqhyperlttsf(p, m, n, k, rounding, ...)
  plotqhyperltfpdf(p, m, n, k, rounding, ...)
  # Preserving the global variable
  par(op)
}




#######################
# Uniform distribution
######################
# Survival function
plotquniflttsf <- function(p, min, max, rounding) {
  rmin <- 0
  rmax <- ceiling(max + 2 * sqrt(max))
  x <- rmin:rmax
  pointx <- punif(x, min, max, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Uniform"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         punif(x - 1, min, max, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qunif(p, min, max, lower.tail = FALSE), digits = rounding)
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
    labels = substitute("p*" == max, list(max = qq)),
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
      punif(w[i], min, max, lower.tail = FALSE),
      w[i + 1],
      punif(w[i], min, max, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      punif(w[i + 1], min, max, lower.tail = FALSE),
      w[i + 1],
      punif(w[i], min, max, lower.tail = FALSE),
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

  # Hint: https://www.statlect.com/fundamentals-of-maxability/quantile
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
    legend = substitute("Parameter:" ~ min == minv ~";"~ max == maxv,
                        list(minv = min, maxv = max))
  )

}

# PF
plotqunifltfpdfaux <- function(q, min, max, rounding, ...) {

  rmin <-
    if (q < min) {
      trunc(q - 4 * sqrt(min))
    } else {
      trunc(min - 4 * sqrt(min))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > min) {
      ceiling(q + 4 * sqrt(max))
    } else {
      ceiling(max + 4 * sqrt(max))
    }
  x <- rmin:rmax
  maxx <- dunif(x, min, max)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(maxx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    maxx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, maxx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      punif(q = q,
            min, max,
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
  maxx1 <- dunif(x1, min, max)
  maxx2 <- dunif(x2, min, max)
  lines(x1,
        maxx1,
        type = "h",
        lwd = 2,
        col = "black")
  points(x1,
         maxx1,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x2,
        maxx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         maxx2,
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
       1.03 * max(maxx),
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
          bold("Probability function plot: Uniform"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ min == minv ~";"~max == maxv~".",
                          list(minv = min, maxv = max)), cex = 0.8
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
          bold("Probability function plot: Uniform"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ min == minv ~";"~max == maxv~".",
                          list(minv = min, maxv = max)), cex = 0.8
    )
  }
}
plotqunifltfpdf <- function(p, min, max, rounding, ...) {
  q <- qunif(p, min, max, lower.tail = FALSE)
  plotqunifltfpdfaux(q, min, max, rounding, ...)
}

# BOTH
plotqunifltfboth <- function(p, min, max, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotquniflttsf(p, min, max, rounding, ...)
  plotqunifltfpdf(p, min, max, rounding, ...)
  # Preserving the global variable
  par(op)
}





######################
# Wilcox distribution
######################
# Survival function
plotqwilcoxlttsf <- function(p, m, n, rounding) {
  rmin <- 0
  rmax <- ceiling(qwilcox(p,m,n)+2*qwilcox(p,m,n))
  x <- rmin:rmax
  pointx <- pwilcox(x, m, n, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Wilcox"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         pwilcox(x - 1, m, n, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = lambda, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qwilcox(p, m, n, lower.tail = FALSE), digits = rounding)
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
    labels = substitute("p*" == n, list(n = qq)),
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
      pwilcox(w[i], m, n, lower.tail = FALSE),
      w[i + 1],
      pwilcox(w[i], m, n, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      pwilcox(w[i + 1], m, n, lower.tail = FALSE),
      w[i + 1],
      pwilcox(w[i], m, n, lower.tail = FALSE),
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

  # Hint: https://www.statlect.com/fundamentals-of-nability/quantile
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
    legend = substitute("Parameter:" ~ m == mv ~";"~ n == nv,
                        list(mv = m, nv = n))
  )

}

# PF
plotqwilcoxltfpdfaux <- function(q, m, n, rounding, ...) {

  rmin <-
    if (q < m) {
      trunc(q - 4 * sqrt(m))
    } else {
      trunc(m - 4 * sqrt(m))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <- q + 2*q
  x <- rmin:rmax
  nx <- dwilcox(x, m, n)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(nx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    nx,
    lwd = 2,
    pch = 19
    #panel.first = grid(col = "gray90")
  )
  lines(x, nx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      pwilcox(q = q,
              m, n,
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
  nx1 <- dwilcox(x1, m, n)
  nx2 <- dwilcox(x2, m, n)
  lines(x1,
        nx1,
        type = "h",
        lwd = 2,
        col = "black")
  points(x1,
         nx1,
         lwd = 2,
         pch = 19,
         col = "black")
  lines(x2,
        nx2,
        type = "h",
        lwd = 2,
        col = "red")
  points(x2,
         nx2,
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
       1.03 * max(nx),
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
          bold("Probability function plot: Wilcox"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ m == mv ~";"~n == nv~".",
                          list(mv = m, nv = n)), cex = 0.8
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
          bold("Probability function plot: Wilcox"),
          p[X](x) == frac(n*"!", x*"!"*(n-x)*"!")*
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
      legend = substitute("Parameters:" ~ m == mv ~";"~n == nv~".",
                          list(mv = m, nv = n)), cex = 0.8
    )
  }
}
plotqwilcoxltfpdf <- function(p, m, n, rounding, ...) {
  q <- qwilcox(p, m, n, lower.tail = FALSE)
  plotqwilcoxltfpdfaux(q, m, n, rounding, ...)
}

# BOTH
plotqwilcoxltfboth <- function(p, m, n, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqwilcoxlttsf(p, m, n, rounding, ...)
  plotqwilcoxltfpdf(p, m, n, rounding, ...)
  # Preserving the global variable
  par(op)
}







######################
# Signrank distribution
######################
# Survival function
plotqsignranklttsf <- function(p, n, rounding) {
  rmin <- 0
  rmax <- ceiling(n + 4 * sqrt(n))
  x <- rmin:rmax
  pointx <- psignrank(x, n = n, lower.tail = FALSE)
  xlim <- c(rmin, rmax)
  ylim <- c(0, 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = x)
  axis(2)
  panel.first = grid(col = "gray90")
  title(
    ylab = expression(1 - F[X](x)),
    xlab = "X",
    #panel.first = grid(col = "gray90"),
    main = bquote(atop(
      bold("Survival function plot: Signed-Rank Wilcox"),
      Q[S]("p*") == inf ~ bgroup("{", x %in% R ~ ":" ~ "p*" >= 1 - F(x), "}") *
        "," ~ "p*" == 1 - p
    )),
    cex.main = 1
  )
  rect(par("usr")[1], 1.03 * 1, par("usr")[2], par("usr")[4], col = "gray")

  points(x,
         psignrank(x - 1, n = n, lower.tail = FALSE),
         lwd = 2,
         pch = 1)
  points(x, pointx, lwd = 2, pch = 19)
  #abline(v = n, lty = 2)
  qq <- round(p, digits = rounding)
  qqaux <-
    round(qsignrank(p, n, lower.tail = FALSE), digits = rounding)
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
      psignrank(w[i], n = n, lower.tail = FALSE),
      w[i + 1],
      psignrank(w[i], n = n, lower.tail = FALSE),
      lty = 1,
      col = "black"
    )
    segments(
      w[i + 1],
      psignrank(w[i + 1], n = n, lower.tail = FALSE),
      w[i + 1],
      psignrank(w[i], n = n, lower.tail = FALSE),
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
    legend = substitute("Parameter:" ~ n == lambd,
                        list(lambd = n)),  cex = 0.8
  )

}

# PF
plotqsignrankltfpdfaux <- function(q, n, rounding, ...) {

  rmin <-
    if (q < n) {
      trunc(q - 4 * sqrt(n))
    } else {
      trunc(n - 4 * sqrt(n))
    }
  if (rmin < 0) {
    rmin <- 0
  } else {
    rmin <- round(rmin)
  }
  rmax <-
    if (q > n) {
      ceiling(q + 4 * sqrt(n))
    } else {
      ceiling(n + 4 * sqrt(n))
    }
  x <- rmin:rmax
  probx <- dsignrank(x, n = n)

  xlim <- c(rmin, rmax)
  ylim <- c(0, max(probx) * 1.2)
  plot.new()
  plot.window(xlim, ylim)
  axis(1, at = 5 * (0:rmax))
  axis(2)
  panel.first = grid(col = "gray90")
  points(
    x,
    probx,
    lwd = 2,
    pch = 19,
    #panel.first = grid(col = "gray90")
  )
  lines(x, probx, type = "h", lwd = 2)
  qq <- round(q, digits = rounding)
  aux2 <- par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20
  Pr <-
    round(
      psignrank(q = q,
                n = n,
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
  probx1 <- dsignrank(x1, n = n)
  probx2 <- dsignrank(x2, n = n)
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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
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
          bold("Probability function plot: Signed-Rank Wilcox"),
          p[X](x) == frac(symbol(n) ^ x %*% e ^ -symbol(n), x * "!") *
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
      legend = substitute("Parameters:" ~ n == lambd,
                          list(lambd = n)), cex = 0.8
    )
  }
}
plotqsignrankltfpdf <- function(p, n, rounding, ...) {
  q <- qsignrank(p, n, lower.tail = FALSE)
  plotqsignrankltfpdfaux(q, n, rounding, ...)
}

# BOTH
plotqsignrankltfboth <- function(p, n, rounding, mfrow, cex.main, ...) {
  op <- par(mfrow = mfrow)
  plotqsignranklttsf(p, n, rounding, ...)
  plotqsignrankltfpdf(p, n, rounding, ...)
  # Preserving the global variable
  par(op)
}



