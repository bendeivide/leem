#' Distribution Function Properties
#'
#' Graphic presentation of properties for distribution function
#'
#' @param variable Variabe type. Defaults \code{variable = "discrete"}. The options are: \code{discrete} or "\code{1}" and \code{continuous} or "\code{2}".
#' @param prop Properties for distribution function. See Details.
#' @details
#' - \code{prop = 1}: \eqn{\lim_{x\rightarrow\infty}F_X(x)=1} and \eqn{\lim_{x\rightarrow -\infty}F_X(x)=0};
#'
#' - \code{prop = 2}: \eqn{F_X(x)\leq F_X(y), ~ x\leq y~\forall x,y \in \mathbb{R}};
#'
#' - \code{prop = 3}: \eqn{\lim_{x_n\downarrow x}F_X(x_n)\downarrow F_X(x)}.
#'
#' @examples
#' library(leem)
#' # Example 1
#' showcdf()
#' @export
showcdf <- function(variable = "discrete", prop = NULL) {
  if (variable == 1) variable <- "discrete"
  if (variable == 2) variable <- "continuous"
  if (variable == "discrete") {
    rmin <- -2
    rmax <- 8
    x <- rmin:rmax
    lambda <- 2
    pointx <- ppois(x, lambda = lambda)
    xlim <- c(rmin, rmax)
    ylim <- c(0, 1.2)
    plot.new()
    plot.window(xlim, ylim)
    titdist <- gettext("Distribution Function", domain = "R-leem")
    titvar <- gettext("Discrete variable", domain = "R-leem")
    titprp <- gettext("Distribution Function Properties", domain = "R-leem")
    #oldpar <- par(mgp = c(2, 0, 0))  # salva configuracoes anteriores
    if (is.null(prop)) {
      title(substitute(atop(bold(titdist), titvar), list(titdist = titdist, titvar = titvar)),
            xlab = bquote(X), ylab = bquote(F[X](x)), col.lab = "blue")
    } else {
      title(substitute(atop(bold(titprp), titvar), list(titprp = titprp, titvar = titvar)),
            xlab = bquote(X), ylab = bquote(F[X](x)), col.lab = "blue")
    }
    #par(oldpar)  # restaura configuracoes padrao
    axis(1, at = x[3], labels = bquote(min*group("(", x, ")")), tick = TRUE, lwd = 0, lwd.ticks = 1,
         col.axis = "blue", col = "blue", pos = 0.015)
    for(i in 4:7) {
      axis(1, at = x[i], labels = substitute(x[i], list(i = i-2)), tick = TRUE, lwd = 0, lwd.ticks = 1,
           col.axis = "blue", col = "blue", pos = 0.015)
    }
    axis(1, at = x[9], labels = bquote(max*group("(", x, ")")), tick = TRUE, col.axis = "blue", col = "blue", pos = 0.015)
    segments(-0.320,0,x[7]+0.5, 0, col = "blue")
    #text(x[6], -0.1, "...", col = "blue")
    # axis(1, at = x[10], labels = "...", col.axis = "blue", col = "blue",
    #      tick = FALSE, lwd = 0, lwd.ticks = 1, pos = 0.04)
    axis(1, at = x[8], labels = "...", col.axis = "blue", col = "blue",
         tick = FALSE, lwd = 0, lwd.ticks = 1, pos = 0.12)
    arrows(x[8]+0.5, 0, x[10], 0, col = "blue", length = 0.1)
    #axis(2)
    x <- x[c(3:7, 9)]



    w <- c(par("usr")[1], x[-6])
    for (i in 2:length(w)) {
      segments(
        w[i],
        ppois(w[i], lambda = lambda),
        w[i + 1],
        max(ppois(w[i], lambda = lambda)),
        lty = 1,
        col = "black"
      )
      # segments(w[i + 1],
      #          min(ppois(w[i + 1], lambda = lambda)),
      #          w[i + 1],
      #          max(ppois(w[i], lambda = lambda)),
      #          lty = 2,
      #          col = "black")
    }
    segments(
      c(-1, 6, 4),
      c(0, 1, ppois(4, 2)),
      c(0, 7, 5),
      c(0, 1, ppois(4, 2)),
      lty = 1,
      col = "black"
    )

    text(5.3, ppois(4, 2), "...", srt = 30)
    axis(2, at = c(0, 0.5, 1), col.axis = "blue", col = "blue", las = 2)

    if (is.null(prop)) {
      for (i in 1:length(w)) {
        segments(w[i + 1],
                 min(ppois(w[i + 1], lambda = lambda)),
                 w[i + 1],
                 max(ppois(w[i], lambda = lambda)),
                 lty = 2,
                 col = "gray")
      }
      # Pontos cheios
      pointx <- ppois(x, lambda = 2)
      points(x, pointx, lwd = 2, pch = 19, bg = "white", col = "white")
      points(x, pointx, lwd = 2, pch = 19)
      # Pontos vazados
      points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 19, bg = "white", col = "white")
      points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 1)

    } else {
      if (prop == 1) {
        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda)),
                   lty = 2,
                   col = "gray")
        }
        # property I
        text(7, 1.08, bquote(lim(F[X](x), x %->% infinity) == 1), col = "red")
        text(-1, 0.2, bquote(lim(F[X](x), x %->%~-infinity) == 0), col = "red")
        segments(
          c(-1, 6),
          c(0, 1),
          c(0, 7),
          c(0, 1),
          lty = 1,
          col = "red"
        )
        # Pontos cheios
        pointx <- ppois(x, lambda = 2)
        points(x, pointx, lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, pointx, lwd = 2, pch = 19)
        # Pontos vazados
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 1)
      }
      if (prop == 2) {
        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda)),
                   lty = 2,
                   col = "gray")
        }
        # Segments
        segments(c(1.5, 2.5), c(0,0), c(1.5, 2.5), c(ppois(1.5, 2), ppois(2.5, 2)), col = "red", lty = 2)
        segments(c(1.5, 2.5), c(0,0), c(1.5, 2.5), c(ppois(1.5, 2), ppois(2.5, 2)), col = "red", lty = 2)
        # Points
        points(1.5, ppois(1.5, 2), lwd = 2, pch = 19, col = "red")
        text(1.5, 0.46, bquote(x), col = "red")
        points(2.5, ppois(2.5, 2), lwd = 2, pch = 19, col = "red")
        text(2.5, 0.74, bquote(y), col = "red")
        text(0.5, 0.8, bquote(atop(F[X](x) <= F[X](y), x <= y)), col = "red")
        segments(rep(par("usr")[1], 2), c(ppois(1.5, 2), ppois(2.5, 2)), c(1.5, 2.5), c(ppois(1.5, 2), ppois(2.5, 2)), col = "red", lty = 2)
        axis(2, at = c(ppois(1.5, 2)), labels = bquote(F[X](x)), col.axis = "red", las = 2, col = "red")
        axis(2, at = c(ppois(2.5, 2)), labels = bquote(F[X](y)), col.axis = "red", las = 2, col = "red")
        # Pontos cheios
        pointx <- ppois(x, lambda = 2)
        points(x, pointx, lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, pointx, lwd = 2, pch = 19)
        # Pontos vazados
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 1)
      }
      if (prop == 3) {
        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda)),
                   lty = 2,
                   col = "gray")
        }

        points(2, ppois(2, 2), lwd = 2, pch = 19, col = "red")
        text(2, 0.78, bquote(x[3]), col = "red")
        text(3, 0.78, bquote(x[n]), col = "red")

        # Pontos cheios
        pointx <- ppois(x, lambda = 2)
        points(x, pointx, lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, pointx, lwd = 2, pch = 19)
        # Pontos vazados
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x[-6], ppois(x[-6] - 1, lambda = lambda), lwd = 2, pch = 1)
        # Coninuidade
        arrows(2.8, ppois(2, 2), 2.1, ppois(2, 2), col = "red", length = 0.1, lwd = 2)
        text(2.5, 0.53, bquote(lim(F[X](x[n]), x[n]*symbol("\257")*x[3])*symbol("\257")*F[X](x[3])), col = "red")

      }

    }
  }
  if (variable == "continuous") {
    p <- 0.8; mu <- 0; sigma <- 1
    x <- qnorm(p, mu, sigma)
    curve(
      pnorm(x, mean = mu, sd = sigma),
      mu - 4 * sigma,
      mu + 4 * sigma,
      ylim = c(0, 1.2),
      lwd = 4,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )
    x <- seq(mu - 4 * sigma, x[1], by = 0.01)
    y <- seq(x[1], mu + 4 * sigma, by = 0.01)
    fx <- pnorm(x, mu, sigma)
    fy <- pnorm(y, mu, sigma)
    polygon(c(y, rev(y)),
            c(fy, rep(0, length(fy))),
            col = "gray90")

    titdist <- gettext("Distribution Function", domain = "R-leem")
    titvar <- gettext("Continuous variable", domain = "R-leem")
    titprp <- gettext("Distribution Function Properties", domain = "R-leem")

       # Axis
    axis(2, at = c(0, 1), col.axis = "blue", col = "blue", las = 2)

    if (is.null(prop)) {
      title(substitute(atop(bold(titprp), titvar),
                       list(titprp = titprp, titvar = titvar)),
            ylab = bquote(F[X](x)), col.lab = "blue")
      op <- par(mgp=c(1,3,0))
      title(xlab = bquote(X), col.lab = "blue")
      par(op)
    } else {

      title(substitute(atop(bold(titdis), titvar),
                       list(titdis = titdist, titvar = titvar)),
            ylab = bquote(F[X](x)), col.lab = "blue")
      op <- par(mgp=c(1,3,0))
      title(xlab = bquote(X), col.lab = "blue")
      par(op)

      if (prop == 1) {
        # property I
        text(2, 1.1, bquote(lim(F[X](x), x %->% infinity) == 1), col = "red", cex = 1.2)
        arrows(3, 1.03, 4, 1.03, col = "red", length = 0.1, lwd = 2)
        text(-3, 0.2, bquote(lim(F[X](x), x %->%~-infinity) == 0), col = "red", cex = 1.2)
        arrows(-3, 0.05, -4, 0.05, col = "red", length = 0.1, lwd = 2)
      }
      if (prop == 2) {
        # Segments
        segments(c(0, 1), c(0,0), c(0, 1), c(pnorm(0), pnorm(1)), col = "red", lty = 2)
        # Points
        points(0, pnorm(0), lwd = 2, pch = 19, col = "red")
        text(-0.08, 0.55, bquote(x), col = "red")
        points(1, pnorm(1), lwd = 2, pch = 19, col = "red")
        text(0.9, 0.9, bquote(y), col = "red")
        ##
        text(-1, 1, bquote(atop(F[X](x) <= F[X](y), x <= y)), col = "red", cex = 1.2)
        segments(rep(par("usr")[1], 2), c(pnorm(0), pnorm(1)), c(0, 1), c(pnorm(0), pnorm(1)), col = "red", lty = 2)
        axis(2, at = c(pnorm(0)), labels = bquote(F[X](x)), col.axis = "red", las = 2, col = "red")
        axis(2, at = c(pnorm(1)), labels = bquote(F[X](y)), col.axis = "red", las = 2, col = "red")
      }
      if (prop == 3) {
        points(0, pnorm(0), lwd = 2, pch = 19, col = "red")
        text(-0.08, 0.55, bquote(x), col = "red")
        points(1, pnorm(1), lwd = 2, pch = 19, col = "red")
        text(0.9, 0.9, bquote(x[n]), col = "red")
        arrows(1, pnorm(0.9), 0.2, pnorm(0.1), col = "red", length = 0.1, lwd = 3)
        text(-1, 0.8, bquote(lim(F[X](x[n]), x[n]%->%x)%->%F[X](x)), col = "red",
             cex = 1.2)
      }

    }


  }
}










