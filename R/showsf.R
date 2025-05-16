#' Survival Function Properties
#'
#' Graphic presentation of properties for survival function
#'
#' @param variable Variabe type. Defaults \code{variable = "discrete"}. The options are: \code{discrete} or "\code{1}" and \code{continuous} or "\code{2}".
#' @param prop Properties for survival function. See Details.
#' @details
#' - \code{prop = 1}: \eqn{\lim_{x\rightarrow\infty}S_X(x)=0} and \eqn{\lim_{x\rightarrow -\infty}S_X(x)=1};
#'
#' - \code{prop = 2}: \eqn{S_X(x)\geq S_X(y), ~ x\leq y~\forall x,y \in \mathbb{R}};
#'
#' - \code{prop = 3}: \eqn{\lim_{x_n\downarrow x}S_X(x_n)\downarrow S_X(x)}.
#'
#' @examples
#' library(leem)
#' # Example 1: Plot of survival function
#' showsf()
#' @export
showsf <- function(variable = "discrete", prop = NULL) {
  if (variable == 1) variable <- "discrete"
  if (variable == 2) variable <- "continuous"
  if (variable == "discrete") {
    rmin <- -2
    rmax <- 8
    x <- rmin:rmax
    lambda <- 2
    pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
    xlim <- c(rmin, rmax)
    ylim <- c(0, 1.2)
    plot.new()
    plot.window(xlim, ylim)
    titdist <- gettext("Survival Function", domain = "R-leem")
    titvar <- gettext("Discrete variable", domain = "R-leem")
    titprp <- gettext("Survival Function Properties", domain = "R-leem")
    oldpar <- par(mgp = c(1.5, 0, 0))  # salva configuracoes anteriores
    if (is.null(prop)) {
      title(substitute(atop(bold(titdist), titvar), list(titdist = titdist, titvar = titvar)),
            xlab = bquote(X), ylab = bquote(S[X](x)), col.lab = "blue")
    } else {
      title(substitute(atop(bold(titprp), titvar), list(titprp = titprp, titvar = titvar)),
            xlab = bquote(X), ylab = bquote(S[X](x)), col.lab = "blue")
    }
    par(oldpar)  # restaura configuracoes padrao

    axis(1, at = x[3], labels = bquote(min*group("(", x, ")")), tick = TRUE, lwd = 0, lwd.ticks = 1,
         col.axis = "blue", col = "blue", pos = -0.015)
    for(i in 4:7) {
      axis(1, at = x[i], labels = substitute(x[i], list(i = i-2)), tick = TRUE, lwd = 0, lwd.ticks = 1,
           col.axis = "blue", col = "blue", pos = -0.015)
    }
    axis(1, at = x[9], labels = bquote(max*group("(", x, ")")), tick = TRUE, col.axis = "blue", col = "blue", pos = -0.015)
    #axis(1, at = x[8], labels = bquote({}^{"..."}), tick = FALSE, col.axis = "blue")
    segments(-0.320,-0.015,x[7]+0.5, -0.015, col = "blue")
    #text(x[6], -0.1, "...", col = "blue")
    # axis(1, at = 6.5, labels = "...", col.axis = "blue", col = "blue",
    #      tick = FALSE, lwd = 0, lwd.ticks = 1, pos = 0)
    axis(1, at = x[8], labels = "...", col.axis = "blue", col = "blue",
         tick = FALSE, lwd = 0, lwd.ticks = 1, pos = 0.08)
    arrows(x[8]+0.5, -0.015, x[10], -0.015, col = "blue", length = 0.1)
    #axis(2)
    x <- x[c(3:7)]



    w <- c(par("usr")[1], x)
    for (i in 2:length(w)) {
      segments(
        w[i],
        ppois(w[i], lambda = lambda, lower.tail = FALSE),
        w[i + 1],
        max(ppois(w[i], lambda = lambda, lower.tail = FALSE)),
        lty = 1,
        col = "black"
      )
    }
    segments(
      c(-1, 6, 4),
      c(1, 0, ppois(4, 2, lower.tail = FALSE)),
      c(0, 7, 5),
      c(1, 0, ppois(4, 2, lower.tail = FALSE)),
      lty = 1,
      col = "black"
    )

    text(-0.5, 0.94, "...", srt = -30)
    text(5.5, 0.03, "...", srt = -30)
    axis(2, at = c(0, 0.5, 1), col.axis = "blue", col = "blue", las = 2, pos = -1.5)

    if (is.null(prop)) {
      for (i in 1:length(w)) {
        segments(w[i + 1],
                 min(ppois(w[i + 1], lambda = lambda, lower.tail = FALSE)),
                 w[i + 1],
                 max(ppois(w[i], lambda = lambda, lower.tail = FALSE)),
                 lty = 2,
                 col = "gray")
      }
      # Pontos cheios
      x <- c(x, 6)
      points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 1, bg = "white", col = "white")
      points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 19)
      # Pontos vazados
      x <- x[-6]
      pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
      points(x, c(1, pointx[-5]), lwd = 2, pch = 19, bg = "white", col = "white")
      points(x, c(1, pointx[-5]), lwd = 2, pch = 1)
    } else {
      if (prop == 1) {
        # property I
        text(6.5, 0.2, bquote(lim(S[X](x), x %->% infinity) == 0), col = "red")
        text(-0.5, 1.08, bquote(lim(S[X](x), x %->%~-infinity) == 1), col = "red")
        segments(
          c(-1, 6),
          c(1, 0),
          c(0, 7),
          c(1, 0),
          lty = 1,
          col = "red"
        )
        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda, lower.tail = FALSE)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda, lower.tail = FALSE)),
                   lty = 2,
                   col = "gray")
        }
        # Pontos cheios
        x <- c(x, 6)
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 1, bg = "white", col = "white")
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 19)
        # Pontos vazados
        x <- x[-6]
        pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
        points(x, c(1, pointx[-5]), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, c(1, pointx[-5]), lwd = 2, pch = 1)
      }
      if (prop == 2) {
        # Segments
        segments(c(1.5, 2.5), c(0,0), c(1.5, 2.5), c(ppois(1.5, 2, lower.tail = FALSE), ppois(2.5, 2, lower.tail = FALSE)), col = "red", lty = 2)
        segments(c(1.5, 2.5), c(0,0), c(1.5, 2.5), c(ppois(1.5, 2, lower.tail = FALSE), ppois(2.5, 2, lower.tail = FALSE)), col = "red", lty = 2)
        # Points
        points(1.5, ppois(1.5, 2, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(1.5, 0.65, bquote(x), col = "red")
        points(2.5, ppois(2.5, 2, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(2.5, 0.38, bquote(y), col = "red")
        text(3, 0.7, bquote(atop(S[X](x) >= S[X](y), x <= y)), col = "red")
        segments(rep(-1.5, 2), c(ppois(1.5, 2, lower.tail = FALSE), ppois(2.5, 2, lower.tail = FALSE)), c(1.5, 2.5), c(ppois(1.5, 2, lower.tail = FALSE), ppois(2.5, 2, lower.tail = FALSE)), col = "red", lty = 2)
        oldpar <- par(mgp = c(0, -1, 0))  # salva configuracoes anteriores
        axis(2, at = c(ppois(1.5, 2, lower.tail = FALSE)), labels = bquote(S[X](x)), col.axis = "red", las = 2, col = "red", tick = FALSE)
        axis(2, at = c(ppois(2.5, 2, lower.tail = FALSE)), labels = bquote(S[X](y)), col.axis = "red", las = 2, col = "red", tick = FALSE)
        par(oldpar)  # restaura configuracoes padrao
        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda, lower.tail = FALSE)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda, lower.tail = FALSE)),
                   lty = 2,
                   col = "gray")
        }
        # Pontos cheios
        x <- c(x, 6)
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 1, bg = "white", col = "white")
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 19)
        # Pontos vazados
        x <- x[-6]
        pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
        points(x, c(1, pointx[-5]), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, c(1, pointx[-5]), lwd = 2, pch = 1)
      }
      if (prop == 3) {
        points(2, ppois(2, 2, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(2, 0.37, bquote(x[3]), col = "red")
        text(3, 0.37, bquote(x[n]), col = "red")
        arrows(2.8, ppois(2, 2, ppois(2, 2, lower.tail = FALSE)), 2.3, ppois(2, 2, ppois(2, 2, lower.tail = FALSE)), col = "red", length = 0.1, lwd = 2)
        text(4, 0.53, bquote(lim(S[X](x[n]), x[n]*symbol("\257")*x[3])*symbol("\257")*S[X](x[3])), col = "red")

        for (i in 1:length(w)) {
          segments(w[i + 1],
                   min(ppois(w[i + 1], lambda = lambda, lower.tail = FALSE)),
                   w[i + 1],
                   max(ppois(w[i], lambda = lambda, lower.tail = FALSE)),
                   lty = 2,
                   col = "gray")
        }
        # Pontos cheios
        x <- c(x, 6)
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 1, bg = "white", col = "white")
        points(x, ppois(x, lambda = lambda, lower.tail = FALSE), lwd = 2, pch = 19)
        # Pontos vazados
        x <- x[-6]
        pointx <- ppois(x, lambda = lambda, lower.tail = FALSE)
        points(x, c(1, pointx[-5]), lwd = 2, pch = 19, bg = "white", col = "white")
        points(x, c(1, pointx[-5]), lwd = 2, pch = 1)
      }
    }
  }
  if (variable == "continuous") {
    p <- 0.8; mu <- 0; sigma <- 1
    x <- qnorm(p, mu, sigma, lower.tail = FALSE)
    curve(
      pnorm(x, mean = mu, sd = sigma, lower.tail = FALSE),
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
    fx <- pnorm(x, mu, sigma, lower.tail = FALSE)
    fy <- pnorm(y, mu, sigma, lower.tail = FALSE)
    polygon(c(y, rev(y)),
            c(fy, rep(0, length(fy))),
            col = "gray90")

    titdist <- gettext("Distribution Function", domain = "R-leem")
    titvar <- gettext("Continuous variable", domain = "R-leem")
    titprp <- gettext("Distribution Function Properties", domain = "R-leem")

    # Axis
    axis(2, at = c(0, 1), col.axis = "blue", col = "blue", las = 2)

    if (is.null(prop)) {
      #op <- par(mgp=c(1,0,0))
      title(substitute(atop(bold(titprp), titvar),
                       list(titprp = titprp, titvar = titvar)),
            ylab = bquote(S[X](x)), col.lab = "blue")
      title(xlab = bquote(X), col.lab = "blue")
      #par(op)
    } else {
      #op <- par(mgp=c(1,0,0))
      title(substitute(atop(bold(titdis), titvar),
                       list(titdis = titdist, titvar = titvar)),
            ylab = bquote(S[X](x)), col.lab = "blue")
      title(xlab = bquote(X), col.lab = "blue")
      #par(op)

      if (prop == 1) {
        # property I
        text(-3, 1.12, bquote(lim(S[X](x), x %->%~ -infinity) == 1), col = "red", cex = 1.3)
        arrows(-3, 1.05, -4, 1.05, col = "red", length = 0.1, lwd = 2)
        text(3, 0.1, bquote(lim(S[X](x), x %->%~infinity) == 0), col = "red", cex = 1.3)
        arrows(3, 0.03, 4, 0.03, col = "red", length = 0.1, lwd = 2)

      }
      if (prop == 2) {
        # Segments
        segments(c(0, 1), c(0,0), c(0, 1), c(pnorm(0, lower.tail = FALSE), pnorm(1, lower.tail = FALSE)), col = "red", lty = 2)
        # Points
        points(0, pnorm(0, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(0.2, 0.55, bquote(x), col = "red")
        points(1, pnorm(1, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(1.5, 0.21, bquote(y), col = "red")
        ##
        text(1.2, 0.7, bquote(atop(S[X](x) >= S[X](y), x <= y)), col = "red", cex = 1.3)
        segments(rep(par("usr")[1], 2), c(pnorm(0, lower.tail = FALSE), pnorm(1, lower.tail = FALSE)), c(0, 1), c(pnorm(0, lower.tail = FALSE), pnorm(1, lower.tail = FALSE)), col = "red", lty = 2)
        axis(2, at = c(pnorm(0, lower.tail = FALSE)), labels = bquote(S[X](x)), col.axis = "red", las = 2, col = "red")
        axis(2, at = c(pnorm(1, lower.tail = FALSE)), labels = bquote(S[X](y)), col.axis = "red", las = 2, col = "red")
      }
      if (prop == 3) {
        points(0, pnorm(0, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(0.1, 0.55, bquote(x), col = "red")
        points(1, pnorm(1, lower.tail = FALSE), lwd = 2, pch = 19, col = "red")
        text(1.2, 0.22, bquote(x[n]), col = "red")
        arrows(1, pnorm(0.9, lower.tail = FALSE), 0.2, pnorm(0.1, lower.tail = FALSE), col = "red", length = 0.1, lwd = 3)
        text(1.6, 0.6, bquote(lim(S[X](x[n]), x[n]%->%x)%->%~S[X](x)), col = "red",
             cex = 1.3)
      }

    }


  }
}
