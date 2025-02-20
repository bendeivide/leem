#' Undertanding the probability of the normal distribution
#'
#' Using a graphical visualization, it is possible to understand the probabilities
#' involved in a normal distribution.
#'
#' @param a lower limit. The default is \code{1}.
#' @param b upper limit. The default is \code{2}, and `b` must be greater than `a`.
#' @param col plot color. The default is \code{col = "lightblue"}.
#' @param mean parameter. The default is \code{0}.
#' @param sd parameter. The default is \code{1}.
#' @param type type of visualization of the probability region plot. Default is \code{1}, others: \code{2, 3, 4, 5, 6}. See Details.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{4}).
#' @param zang Angle of the values on the Z-axis. Default is \code{zang = 0}.
#' @param xang Angle of the values on the X-axis. Default is \code{xang = 0}.
#'
#' @details
#' - `type = 1,2`: `a` and `b` must be greater than `mean`;
#' - `type = 3,4`: `a` and `b` must be less than `mean`;
#' - `type = 5,6`: `a` and `b` can be any real value.
#'
#' @examples
#' \dontrun{
#' probnormal(type = 2)
#' probnormal(-1, 0, type = 3)
#' probnormal(-1, 0, type = 4)
#' probnormal(-1, 0, type = 5)
#' probnormal(-1, 2, type = 5)
#' probnormal(1, 2, type = 5)
#' probnormal(1, 2, type = 6)
#' }
#'
#'
#' @export
probnormal <- function(a = 1, b = 2,
                       col = "lightblue", mean = 0, sd = 1,
                       type = 1, rounding = 4, zang = 0, xang = 0) {
  # Change decimals
  # op <- options(OutDec = ",")
  # Restart decimals
  # options(op)
  #######################
  # Defensive programming
  if (a > b) stop("the 'b' argument must be greater than the 'a' argument", call. = FALSE, domain = "R-leem")
  plot.new()
  plot.window(xlim = c(-4, 4), ylim = c(0, 0.4))

  x <- seq(-4, 4, by = 0.01)
  dx <- dnorm(x)
  polygon(c(x, rev(x)),
          c(dx, rep(0, length(dx))),
          col=col)

  curve(dnorm(x), -4, 4, xlab = "", ylab = "", add = T, lwd = 3, xaxt='n')
  axis(1, at = c(-4,4), labels = c(NA, NA), lwd = 2, lwd.ticks = FALSE)
  axis(1, at = 0, labels = FALSE)
  text(0, par("usr")[3] - 0.01, labels = mean, srt = xang, pos = 1, adj = 0, xpd = TRUE)
  segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
  mtext("X", side  = 1, adj = 1)
  if (a == -Inf & b == Inf) {
    y <- seq(-4, 4, by = 0.01)
    dy <- dnorm(y)
    polygon(c(y, rev(y)),
            c(dy, rep(0, length(dy))),
            col="lightblue4")
    # Labels
    axis(1, at = -4, labels = bquote(-infinity))
    axis(1, at = 4, labels = bquote(infinity))
    arrows(1.5, 0.05, 2, 0.15, length = 0.1, code = 2, lwd = 2, col = "blue")
    text(2.1, 0.17, labels = "1", col = "blue", cex = 2)
    ##
    axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0)
    ##
    axis(1, at = -4, labels = bquote(-infinity), pos = -0.065)
    axis(1, at = 4, labels = bquote(infinity), pos = -0.065)
    axis(1, at = 0, pos = -0.065)
    segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
    mtext("Z", side  = 1, line = 2.3, adj = 1)
    title(main = substitute(integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, -infinity, infinity) == 1, list(mu = mean, sigma = sd)), col.main = "blue")
  } else {
    if (type == 1 | type == 2) {
      if (a < mean) stop("the 'a' argument must be greater than the 'mean' argument", call. = FALSE, domain = "R-leem")
      if (!is.infinite(b)) {
        aux <- (b - mean) / sd
        if (aux >= 4) stop("The value of 'b' is too large for the distribution. Use 'b=Inf' in this case.", call. = FALSE, domain = "R-leem")
      }
    }
    if (type == 3 | type == 4) {
      if (b > mean) stop("the 'b' argument must be less than the 'mean' argument", call. = FALSE, domain = "R-leem")
      if (!is.infinite(a)) {
        aux <- (a - mean) / sd
        if (aux <= -4) stop("The value of 'a' is too less for the distribution. Use 'a=-Inf' in this case.", call. = FALSE, domain = "R-leem")
      }
    }
    if (type == 1) {
      if (b == Inf) {
        z1 <- round((a - mean) / sd, rounding)
        z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = bquote(infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        ##
        if ((z2 - z1) <= 0.5 ) {
          xg <- (z2 + z1) / 2
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        if ((z2 - z1) > 0.5  & (z2 - z1) < 2.2) {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        if ((z2 - z1) >= 2.2) {
          xg <- (z2 + z1) / 3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        ##
        pz5 <- round(0.5000000, rounding)
        pz1 <- round(pnorm(z1) - 0.5, rounding)
        pzt <- round(0.5 - pz1, rounding)
        ##
        arrows(2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows(2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(0, z2), par("usr")[3], c(0, z2), 0.35, lty = 2, col = "red")
        text(z2 / 2, 0.365, labels = pz5, col = "red", cex = 1.4)
        #
        if (z1 != 0) {
          arrows(z1 / 2, 0.15, 0, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z1 / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z1), par("usr")[3], c(0, z1), 0.15, lty = 2, col = "red")
          text(z1 / 2, 0.17, labels = pz1, col = "red", cex = 1.4)
        }
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
        text(z1, -0.075, labels = z1, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        text(z2, -0.075, labels = bquote(infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        if (z1 != 0) {
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        }
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 1)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, infinity) == prob), list(mu = mean, sigma = sd, a = a, prob = pzt)),
              col.main = "blue")
      } else {
        if (a == mean) {
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          z1 <- 0
          y <- seq(0, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- round(pnorm(z2) - 0.5, rounding)
          ##
          arrows(z2 / 2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z2 / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z2), par("usr")[3], c(0, z2), 0.35, lty = 2, col = "red")
          text(z2 / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pz2, col = "blue", cex = 1.4)
          ##
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z2), labels = FALSE, pos = -0.065)
          text(c(z2), -0.075, labels = c(z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 1)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pz2)),
                col.main = "blue")
        } else {
          z1 <- round((a - mean) / sd, rounding)
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- round(pnorm(z2) - 0.5, rounding)
          pz1 <- round(pnorm(z1) - 0.5, rounding)
          pzt <- round(pz2 - pz1, rounding)
          ##
          arrows(z1, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(0, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z2), par("usr")[3], c(0, z2), 0.35, lty = 2, col = "red")
          text(z2 / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          #
          arrows(z1 / 2, 0.15, 0, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z1 / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z2), par("usr")[3], c(0, z2), 0.35, lty = 2, col = "red")
          text(z1 / 2, 0.18, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 1)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
    if (type == 2) {
      if (b == Inf) {
        z1 <- round((a - mean) / sd, rounding)
        z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = bquote(infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        ##
        if ((z2 - z1) <= 0.5 ) {
          xg <- (z2 + z1) / 2
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        if ((z2 - z1) > 0.5  & (z2 - z1) < 2.2) {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        if ((z2 - z1) >= 2.2) {
          xg <- (z2 + z1) / 3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        ##
        pz1 <- round(pnorm(z1, lower.tail = FALSE), rounding)
        ##
        arrows((z1 + z2) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows((z1 + z2) / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(z1, z2), par("usr")[3], c(z1, z2), 0.35, lty = 2, col = "red")
        text((z1 + z2) / 2, 0.38, labels = pz1, col = "red", cex = 1.4)
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(-2.75, 0.23, labels = pz1, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z1), labels = FALSE, pos = -0.065)
        text(z1, -0.075, labels = z1, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = c(z2), labels = FALSE, pos = -0.065)
        text(z2, -0.075, labels = bquote(infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = 0, labels = FALSE, pos = -0.065)
        text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 2)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, infinity) == prob), list(mu = mean, sigma = sd, a = a, prob = pz1)),
              col.main = "blue")
      } else {
        if (a == mean) {
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          z1 <- 0
          y <- seq(0, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(0.5000000, rounding)
          pz2 <- round(pnorm(z2, lower.tail = FALSE), rounding)
          ##
          arrows(z2, 0.35, 4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, 4), par("usr")[3], c(0, 4), 0.35, lty = 2, col = "red")
          text(2, 0.38, labels = pz1, col = "red", cex = 1.4)
          ##
          arrows((z2+4) / 2, 0.25, 4, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2+4) / 2, 0.25, z2, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, 4), par("usr")[3], c(z2, 4), 0.25, lty = 2, col = "red")
          text((z2 + 4) / 2, 0.28, labels = pz2, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z2), labels = FALSE, pos = -0.065)
          text(c(z2), -0.075, labels = c(z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 2)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pz1 - pz2)),
                col.main = "blue")

        } else {
          z1 <- round((a - mean) / sd, rounding)
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(pnorm(z1, lower.tail = FALSE), rounding)
          pz2 <- round(pnorm(z2, lower.tail = FALSE), rounding)
          pzt <- round(pz1 - pz2, rounding)
          ##
          arrows((z2 + 4) / 2, 0.35, 4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2 + 4) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, 4), par("usr")[3], c(z2, 4), 0.35, lty = 2, col = "red")
          text((z2 + 4) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          #
          arrows((z1 + 4) / 2, 0.15, 4, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1 + 4) / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, 4), par("usr")[3], c(z1, 4), 0.15, lty = 2, col = "red")
          text((z1 + 4) / 2, 0.17, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 2)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
    if (type == 3) {
      if (a == -Inf) {
        z1 <- -4
        z2 <- round((b - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = bquote(-infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        # Labels
        if ((z2 - z1) <= 0.5 ) {
          xg <- (z2 + z1) / 2
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        if ((z2 - z1) > 0.5  & (z2 - z1) < 2.2) {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        if ((z2 - z1) >= 2.2) {
          xg <- (z2 + z1) / 3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }

        ##
        pz1 <- round(0.5 - pnorm(z1), rounding)
        pz2 <- round(0.5 - pnorm(z2), rounding)
        pzt <- round(pz1 - pz2, rounding)
        ##
        arrows(z1 / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows(z1 / 2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(z1, 0), par("usr")[3], c(z1, 0), 0.35, lty = 2, col = "red")
        text(-2, 0.38, labels = pz1, col = "red", cex = 1.4)
        #
        if (z2 != 0) {
          arrows(z2 / 2, 0.15, 0, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z2 / 2, 0.15, z2, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z2), par("usr")[3], c(0, z2), 0.15, lty = 2, col = "red")
          text(z2 / 2, 0.18, labels = pz2, col = "red", cex = 1.4)
        }
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
        text(z2, -0.075, labels = z2, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        text(z1, -0.075, labels = bquote(-infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        if (z2 != 0) {
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        }
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 3)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, -infinity, b) == prob), list(mu = mean, sigma = sd, b = b, prob = pzt)),
              col.main = "blue")
      } else {
        if (b == mean) {
          z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
          z2 <- 0
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          xg <- (z2 + z1) / 2
          if ((z2 - z1) >= 1.5 ){
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- 0
          pz1 <- round(0.5 - pnorm(z1), rounding)
          pzt <- round(pz1 - pz2, rounding)
          ##
          arrows((z1 + z2) / 2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1 + z2) / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z1), par("usr")[3], c(0, z1), 0.35, lty = 2, col = "red")
          text(-2, 0.38, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z1), labels = FALSE, pos = -0.065)
          text(c(z1), -0.075, labels = c(z1), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 3)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = mean, prob = pzt)),
                col.main = "blue")

        } else {
          z1 <- round((a - mean) / sd, rounding)
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels

          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 3
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- round(0.5 - pnorm(z2), rounding)
          pz1 <- round(0.5 - pnorm(z1), rounding)
          pzt <- round(pz1 - pz2, rounding)
          ##
          arrows(z1 / 2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z1 /2 , 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z1), par("usr")[3], c(0, z1), 0.35, lty = 2, col = "red")
          text(z1 / 2, 0.38, labels = pz1, col = "red", cex = 1.4)
          #
          arrows(z2 / 2, 0.15, 0, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z2 / 2, 0.15, z2, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, z2), par("usr")[3], c(0, z2), 0.15, lty = 2, col = "red")
          text(z2 / 2, 0.17, labels = pz2, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 3)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
    if (type == 4) {
      if (a == -Inf) {
        z2 <- round((b - mean) / sd, rounding)
        z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = bquote(-infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        ##
        if ((z2 - z1) <= 0.5 ) {
          xg <- (z2 + z1) / 2
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        if ((z2 - z1) > 0.5  & (z2 - z1) < 2.2) {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        if ((z2 - z1) >= 2.2) {
          xg <- (z2 + z1) / 3
          yg <- (dnorm(z2) + dnorm(z1)) / 10
        }
        ##
        pz2 <- round(pnorm(z2), rounding)
        ##
        arrows((z1 + z2) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows((z1 + z2) / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(z1, z2), par("usr")[3], c(z1, z2), 0.35, lty = 2, col = "red")
        text((z1 + z2) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(2.75, 0.23, labels = pz2, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z2), labels = FALSE, pos = -0.065)
        text(z2, -0.075, labels = z2, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = c(z1), labels = FALSE, pos = -0.065)
        text(z1, -0.075, labels = bquote(-infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = 0, labels = FALSE, pos = -0.065)
        text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 4)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, -infinity, b) == prob), list(mu = mean, sigma = sd, b = b, prob = pz2)),
              col.main = "blue")
      } else {
        if (b == mean) {
          z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
          z2 <- 0
          y <- seq(z1, 0, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- round(0.5000000, rounding)
          pz1 <- round(pnorm(z1), rounding)
          pzt <- round(pz2 - pz1, rounding)
          ##
          arrows(z1, 0.35, -4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z1, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, -4), par("usr")[3], c(0, -4), 0.35, lty = 2, col = "red")
          text(-2, 0.38, labels = pz2, col = "red", cex = 1.4)
          ##
          arrows((z1-4) / 2, 0.25, -4, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1-4) / 2, 0.25, z1, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, -4), par("usr")[3], c(z1, -4), 0.25, lty = 2, col = "red")
          text((z1 - 4) / 2, 0.28, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z1), labels = FALSE, pos = -0.065)
          text(c(z1), -0.075, labels = c(z1), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 4)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pz1 - pz2)),
                col.main = "blue")

        } else {
          z2 <- round((b - mean) / sd, rounding)
          z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 3
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(pnorm(z1), rounding)
          pz2 <- round(pnorm(z2), rounding)
          pzt <- round(pz2 - pz1, rounding)
          ##
          arrows((z2 - 4) / 2, 0.35, -4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2 - 4) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, -4), par("usr")[3], c(z2, -4), 0.35, lty = 2, col = "red")
          text((z2 - 4) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          #
          arrows((z1 - 4) / 2, 0.15, -4, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1 - 4) / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, -4), par("usr")[3], c(z1, -4), 0.15, lty = 2, col = "red")
          text((z1 - 4) / 2, 0.18, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 4)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
    if (type == 5) {
      if (b == Inf) {
        z1 <- round((a - mean) / sd, rounding)
        z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = bquote(infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        ##
        if (z1 < 0 & z2 > 0) {
          xg <- 0
          yg <- 0.1
        } else {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        ##
        pz1 <- round(pnorm(z1, lower.tail = FALSE), rounding)
        ##
        arrows((z1 + z2) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows((z1 + z2) / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(z1, z2), par("usr")[3], c(z1, z2), 0.35, lty = 2, col = "red")
        text((z1 + z2) / 2, 0.38, labels = pz1, col = "red", cex = 1.4)
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(-2.75, 0.23, labels = pz1, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z1), labels = FALSE, pos = -0.065)
        text(z1, -0.075, labels = z1, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = c(z2), labels = FALSE, pos = -0.065)
        text(z2, -0.075, labels = bquote(infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = 0, labels = FALSE, pos = -0.065)
        text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 5)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, infinity) == prob), list(mu = mean, sigma = sd, a = a, prob = pz1)),
              col.main = "blue")
      } else {
        if (a == mean) {
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          z1 <- 0
          y <- seq(0, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(pnorm(z1, lower.tail = FALSE), rounding)
          pz2 <- round(pnorm(z2, lower.tail = FALSE), rounding)
          pzt <- round(pz1 - pz2, rounding)
          ##
          arrows(z2, 0.35, 4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z2, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, 4), par("usr")[3], c(0, 4), 0.35, lty = 2, col = "red")
          text(2, 0.38, labels = pz1, col = "red", cex = 1.4)
          ##
          arrows((z2+4) / 2, 0.25, 4, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2+4) / 2, 0.25, z2, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, 4), par("usr")[3], c(z2, 4), 0.25, lty = 2, col = "red")
          text((z2 + 4) / 2, 0.28, labels = pz2, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0)
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z2), labels = FALSE, pos = -0.065)
          text(c(z2), -0.075, labels = c(z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 5)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")

        } else {
          z1 <- round((a - mean) / sd, rounding)
          z2 <- if (b == Inf) 4 else round((b - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(pnorm(z1, lower.tail = FALSE), rounding)
          pz2 <- round(pnorm(z2, lower.tail = FALSE), rounding)
          pzt <- round(pz1 - pz2, rounding)
          ##
          arrows((z2 + 4) / 2, 0.35, 4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2 + 4) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, 4), par("usr")[3], c(z2, 4), 0.35, lty = 2, col = "red")
          text((z2 + 4) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          #
          arrows((z1 + 4) / 2, 0.15, 4, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1 + 4) / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, 4), par("usr")[3], c(z1, 4), 0.15, lty = 2, col = "red")
          text((z1 + 4) / 2, 0.17, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, -2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(-2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 5)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
    if (type == 6) {
      if (a == -Inf) {
        z2 <- round((b - mean) / sd, rounding)
        z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
        y <- seq(z1, z2, by = 0.01)
        dy <- dnorm(y)
        polygon(c(y, rev(y)),
                c(dy, rep(0, length(dy))),
                col="lightblue4")
        # Labels
        axis(1, at = z2, labels = FALSE)
        text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = z1, labels = FALSE)
        text(z1, par("usr")[3] - 0.01, labels = bquote(-infinity), srt = xang, pos = 1, adj = 0, xpd = TRUE)
        ##
        if (z1 < 0 & z2 > 0) {
          xg <- 0
          yg <- 0.1
        } else {
          xg <- (z2 + z1) / 2.3
          yg <- (dnorm(z2) + dnorm(z1)) / 8
        }
        ##
        pz2 <- round(pnorm(z2), rounding)
        ##
        arrows((z1 + z2) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        arrows((z1 + z2) / 2, 0.35, z1, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
        segments(c(z1, z2), par("usr")[3], c(z1, z2), 0.35, lty = 2, col = "red")
        text((z1 + z2) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
        ##
        axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
        ##
        arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
        text(2.75, 0.23, labels = pz2, col = "blue", cex = 1.4)
        ##
        axis(1, at = c(z2), labels = FALSE, pos = -0.065)
        text(z2, -0.075, labels = z2, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = c(z1), labels = FALSE, pos = -0.065)
        text(z1, -0.075, labels = bquote(-infinity), srt = zang, pos = 1, adj = 0, xpd = TRUE)
        axis(1, at = 0, labels = FALSE, pos = -0.065)
        text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
        segments(0, par("usr")[3], 0, dnorm(0), lty = 2)
        mtext("Z", side  = 1, line = 2.3, adj = 1)
        title(main = substitute(atop("Understanding the probability of the normal (type 6)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, -infinity, b) == prob), list(mu = mean, sigma = sd, b = b, prob = pz2)),
              col.main = "blue")
      } else {
        if (b == mean) {
          z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
          z2 <- 0
          y <- seq(z1, 0, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz2 <- round(0.5000000, rounding)
          pz1 <- round(pnorm(z1), rounding)
          pzt <- round(pz2 - pz1, rounding)
          ##
          arrows(z1, 0.35, -4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows(z1, 0.35, 0, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(0, -4), par("usr")[3], c(0, -4), 0.35, lty = 2, col = "red")
          text(-2, 0.38, labels = pz2, col = "red", cex = 1.4)
          ##
          arrows((z1-4) / 2, 0.25, -4, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1-4) / 2, 0.25, z1, 0.25, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, -4), par("usr")[3], c(z1, -4), 0.25, lty = 2, col = "red")
          text((z1 - 4) / 2, 0.28, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          ##
          axis(1, at = c(z1), labels = FALSE, pos = -0.065)
          text(c(z1), -0.075, labels = c(z1), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 6)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pz1 - pz2)),
                col.main = "blue")

        } else {
          z2 <- round((b - mean) / sd, rounding)
          z1 <- if (a == -Inf) -4 else round((a - mean) / sd, rounding)
          y <- seq(z1, z2, by = 0.01)
          dy <- dnorm(y)
          polygon(c(y, rev(y)),
                  c(dy, rep(0, length(dy))),
                  col="lightblue4")
          # Labels
          axis(1, at = z1, labels = FALSE)
          text(z1, par("usr")[3] - 0.01, labels = a, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = z2, labels = FALSE)
          text(z2, par("usr")[3] - 0.01, labels = b, srt = xang, pos = 1, adj = 0, xpd = TRUE)
          # Labels
          if ((z2 - z1) >= 2 ) {
            xg <- (z2 + z1) / 3
            yg <- (dnorm(z2) + dnorm(z1)) / 10
          } else {
            xg <- (z2 + z1) / 2
            yg <- (dnorm(z2) + dnorm(z1)) / 8
          }
          ##
          pz1 <- round(pnorm(z1), rounding)
          pz2 <- round(pnorm(z2), rounding)
          pzt <- round(pz2 - pz1, rounding)
          ##
          arrows((z2 - 4) / 2, 0.35, -4, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z2 - 4) / 2, 0.35, z2, 0.35, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z2, -4), par("usr")[3], c(z2, -4), 0.35, lty = 2, col = "red")
          text((z2 - 4) / 2, 0.38, labels = pz2, col = "red", cex = 1.4)
          #
          arrows((z1 - 4) / 2, 0.15, -4, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          arrows((z1 - 4) / 2, 0.15, z1, 0.15, length = 0.1, code = 2, lwd = 1, col = "red")
          segments(c(z1, -4), par("usr")[3], c(z1, -4), 0.15, lty = 2, col = "red")
          text((z1 - 4) / 2, 0.18, labels = pz1, col = "red", cex = 1.4)
          ##
          axis(1, pos = -0.065, at = c(-4, 4), labels = c(NA, NA), lwd = 2, lwd.ticks = 0, )
          ##
          arrows(xg, yg, 2.2, 0.22, length = 0.1, code = 2, lwd = 2, col = "blue")
          text(2.75, 0.23, labels = pzt, col = "blue", cex = 1.4)
          ##
          axis(1, at = c(z1, z2), labels = FALSE, pos = -0.065)
          text(c(z1, z2), -0.075, labels = c(z1, z2), srt = zang, pos = 1, adj = 0, xpd = TRUE)
          axis(1, at = 0, labels = FALSE, pos = -0.065)
          text(0, -0.075, labels = 0, srt = zang, pos = 1, adj = 0, xpd = TRUE)
          mtext("Z", side  = 1, line = 2.3, adj = 1)
          title(main = substitute(atop("Understanding the probability of the normal (type 6)", integral(frac(1, sqrt(2*pi%*%sigma^2))%*% e^{-frac(1, 2)%*%bgroup("(", frac(mu-x, sigma), ")")^2}*dx, a, b) == prob), list(mu = mean, sigma = sd, a = a, b = b, prob = pzt)),
                col.main = "blue")
        }
      }
    }
  }
}

