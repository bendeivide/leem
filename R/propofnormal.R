#' Properties of the normal distribution
#'
#' Graphically it is possible to observe some properties of the normal distribution
#'
#' @param col color type.
#' @param type numerical. Type of properties. Options: \code{1}, \code{2}, \code{3}, \code{4} and \code{5}. Default \code{code = 1}.

#' @export
propofnormal <- function(col = "lightblue2", type = 1){
  if (type == 1) {
    plot.new()
    plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))
    x <- seq(-4, 4, by = 0.01)
    dx <- dnorm(x)
    polygon(c(x, rev(x)),
            c(dx, rep(0, length(dx))),
            col=col)

    curve(dnorm, -4, 4, xlab = "", ylab = "", add = T, lwd = 2, xaxt='n')
    axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
    axis(1, at = 0, labels = bquote(mu))
    segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
    mtext("X", side  = 1, adj = 0.98)
  }
  if (type == 2) {
    plot.new()
    plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))

    x <- seq(-4, 4, by = 0.01)
    dx <- dnorm(x)
    polygon(c(x, rev(x)),
            c(dx, rep(0, length(dx))),
            col=col)

    curve(dnorm, -4, 4, xlab = "", ylab = "", add = T, lwd = 2, xaxt='n')
    axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
    axis(1, at = 0, labels = bquote(mu))
    segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
    text(c(-0.5, 0.5), c(0.15, 0.15), labels = c(0.5, 0.5), cex = 2)
    mtext("X", side  = 1, adj = 0.98)
  }
  if (type == 3) {
    plot.new()
    plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))

    x <- seq(-4, 4, by = 0.01)
    dx <- dnorm(x)
    polygon(c(x, rev(x)),
            c(dx, rep(0, length(dx))),
            col=col)

    curve(dnorm, -4, 4, xlab = "", ylab = "", add = T, lwd = 2, xaxt='n')
    axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
    axis(1, at = 0, labels = bquote(mu))
    axis(1, at = -3, labels = bquote(mu-3*sigma))
    axis(1, at = -2, labels = bquote(mu-2*sigma))
    axis(1, at = -1, labels = bquote(mu-1*sigma))
    axis(1, at = 1, labels = bquote(mu+1*sigma))
    axis(1, at = 2, labels = bquote(mu+2*sigma))
    axis(1, at = 3, labels = bquote(mu+3*sigma))
    segments(-3:3, par("usr")[3], -3:3, dnorm(-3:3), lty = 2)
    # Flechas
    arrows(0, 0.05, 3, 0.05, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.05, -3, 0.05, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.04, 0.5, 0.06, col = "white")
    text(0, 0.05, labels = round(pnorm(3) - pnorm(-3), 4), lwd = 2, col = "blue")
    segments(c(-3,3), dnorm(c(-3, 3)), c(-3,3), 0.05, lty = 2, col = "blue")
    ##
    arrows(0, 0.12, 2, 0.12, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.12, -2, 0.12, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.11, 0.5, 0.13, col = "white")
    text(0, 0.12, labels = round(pnorm(2) - pnorm(-2), 4), lwd = 2, col = "blue")
    segments(c(-2,2), dnorm(c(-2, 2)), c(-2,2), 0.12, lty = 2, col = "blue")
    ##
    arrows(0, 0.19, 1, 0.19, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.19, -1, 0.19, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.18, 0.5, 0.2, col = "white")
    text(0, 0.19, labels = round(pnorm(1) - pnorm(-1), 4), lwd = 2, col = "blue")
    mtext("X", side  = 1, adj = 0.98)
  }
  if (type == 4) {
    plot.new()
    plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))

    x <- seq(-4, 4, by = 0.01)
    dx <- dnorm(x)
    polygon(c(x, rev(x)),
            c(dx, rep(0, length(dx))),
            col=col)

    curve(dnorm, -4, 4, xlab = "", ylab = "", add = T, lwd = 2, xaxt='n')
    axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
    axis(1, at = 0, labels = bquote(mu))
    axis(1, at = -3, labels = bquote(mu-3*sigma))
    axis(1, at = -2, labels = bquote(mu-2*sigma))
    axis(1, at = -1, labels = bquote(mu-1*sigma))
    axis(1, at = 1, labels = bquote(mu+1*sigma))
    axis(1, at = 2, labels = bquote(mu+2*sigma))
    axis(1, at = 3, labels = bquote(mu+3*sigma))
    segments(-3:3, par("usr")[3], -3:3, dnorm(-3:3), lty = 2)
    # Flechas
    arrows(0, 0.05, 3, 0.05, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.05, -3, 0.05, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.04, 0.5, 0.06, col = "white")
    text(0, 0.05, labels = round(pnorm(3) - pnorm(-3), 4), lwd = 2, col = "blue")
    segments(c(-3,3), dnorm(c(-3, 3)), c(-3,3), 0.05, lty = 2, col = "blue")
    ##
    arrows(0, 0.12, 2, 0.12, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.12, -2, 0.12, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.11, 0.5, 0.13, col = "white")
    text(0, 0.12, labels = round(pnorm(2) - pnorm(-2), 4), lwd = 2, col = "blue")
    segments(c(-2,2), dnorm(c(-2, 2)), c(-2,2), 0.12, lty = 2, col = "blue")
    ##
    arrows(0, 0.19, 1, 0.19, length = 0.1, code = 2, lwd = 2, col = "blue")
    arrows(0, 0.19, -1, 0.19, length = 0.1, code = 2, lwd = 2, col = "blue")
    rect(-0.5, 0.18, 0.5, 0.2, col = "white")
    text(0, 0.19, labels = round(pnorm(1) - pnorm(-1), 4), lwd = 2, col = "blue")
    mtext("X", side  = 1, adj = 0.98)
    axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0)
    ##
    axis(1, at = -3:3, pos = -0.065)
    mtext("Z", side  = 1, line = 2.3, adj = 0.98)
  }
  if (type == 5) {
    plot.new()
    plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))

    x <- seq(-4, 4, by = 0.01)
    dx <- dnorm(x)
    polygon(c(x, rev(x)),
            c(dx, rep(0, length(dx))),
            col=col)

    curve(dnorm, -4, 4, xlab = "", ylab = "", add = T, lwd = 2, xaxt='n')
    axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
    axis(1, at = 0, labels = bquote(mu))
    segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
    mtext("X", side  = 1, adj = 0.98)

    y <- seq(1, 2, by = 0.01)
    dy <- dnorm(y)
    polygon(c(y, rev(y)),
            c(dy, rep(0, length(dy))),
            col="lightblue4")
    axis(1, at = c(1, 2), labels = c("a", "b"))
    arrows(1.5, 0.05, 2, 0.15, length = 0.1, code = 2, lwd = 2, col = "blue")
    text(2.1, 0.17, labels = "A", col = "blue", cex = 2)
    title(main = bquote(A == integral(frac(1, sqrt(2*pi*sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b)), col.main = "blue")
  }
}


