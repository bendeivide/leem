#' Understanding the Confiance Indice
#'
#' Detailing the confiance indice plot, showing the main information contained in this type of graph.
#'
#' @param dist Parameter to indicate the distribution of the graphic, fixed for now.
#' @param ci Parameter to indicate the region of the confiance indice.
#' @param main Parameter to indicate the title of the graphic.
#'
#' @examples
#' library(leem)
#' # Example 1
#' showci()
#'
#' @export
showci <- function(dist = "normal", ci = "two.sided", main = NULL) {
  if (dist == "symmetric") {
    if (ci == "two.sided") {
      p <- 0.05
      p <- c(p / 2, 1 - p / 2)
      mu <- 0; sigma <- 1
      q <- qnorm(p, mu, sigma)
      minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
      maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
      x <- seq(minimo, q[1], by = 0.01)
      z <- seq(q[2], maximo, by = 0.01)
      y <- seq(minimo, maximo, by = 0.01)
      fx <- dnorm(x, mean = mu, sd = sigma)
      fz <- dnorm(z, mean = mu, sd = sigma)
      fy <- dnorm(y, mean = mu, sd = sigma)
      # Change global variable
      op <- par(mar=c(10,4,4,2)+0.1)
      # Curve
      curve(
        dnorm(x, mean = mu, sd = sigma),
        minimo,
        maximo,
        ylim = c(0, 1.2 * max(fx, fy, fz)),
        xlab = "",
        ylab = "",
        lwd = 3,
        axes = FALSE
      )
      polygon(c(y, rev(y)),
              c(fy, rep(0, length(fy))),
              col = "gray90")
      polygon(c(x, rev(x)),
              c(fx, rep(0, length(fx))),
              col = "red")
      polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
              col = "red")


      # Confidence level
      cl <- gettext("Confidence level", domain = "R-leem")
      text(0, 0.1, substitute(atop(bold(cl), 1 - alpha), list(cl = cl)), cex = 1)
      # Axis
      axis(1, at = -1.959964, labels = bquote(-q[(alpha/2)]), cex.axis = 1.3)
      axis(1, at = 1.959964, labels = bquote(q[(alpha/2)]), cex.axis = 1.3)
      axis(1, at = 4.2, labels = "Q", cex.axis = 1.5, tick = FALSE, pos = 0.01)
      axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Other axis
      axis(1, pos = -0.1, at = -1.959964, labels = bquote(hat(theta)-q[(alpha/2)]%*%S[(hat(theta))]), cex.axis = 1.3)
      axis(1, pos = -0.1, at = 1.959964, labels = bquote(hat(theta)+q[(alpha/2)]%*%S[(hat(theta))]), cex.axis = 1.3)
      axis(1, pos = -0.06, at = 4.2, labels = bquote(hat(theta)), cex.axis = 1.5, tick = FALSE)
      axis(1, pos = -0.1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Errors region
      text(-3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      text(3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      # Pivotal quantity
      qp <- gettext("Pivotal quantity", domain = "R-leem")
      text(2.5, 0.3, labels = substitute(atop(qp, Q == frac(hat(theta) - theta, S[(hat(theta))])%~%Dist), list(qp = qp)), cex = 1.3, col = "blue")
      # Title
      if (is.null(main)) {
        cl <- gettext("Confidence interval for", domain = "R-leem")
        cl2 <- gettext("of symmetric distribution", domain = "R-leem")
        title(main = substitute(atop(bold(cl~theta~cl2)), list(cl = cl, cl2 = cl2)), cex.main = 1.2)
      }
      # Formula
      mtext(bquote(IC(theta)[1-alpha]:bgroup("[",hat(theta) - q[(alpha/2)] %*% S[(hat(theta))]<=~theta<= hat(theta) - q[(alpha/2)] %*% S[(hat(theta))], "]")), side = 3, line = -2)
      # Return global variable
      par(op)
    }
  }
  if (dist == "normal") {
    if (ci == "two.sided") {
      p <- 0.05
      p <- c(p / 2, 1 - p / 2)
      mu <- 0; sigma <- 1
      q <- qnorm(p, mu, sigma)
      minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
      maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
      x <- seq(minimo, q[1], by = 0.01)
      z <- seq(q[2], maximo, by = 0.01)
      y <- seq(minimo, maximo, by = 0.01)
      fx <- dnorm(x, mean = mu, sd = sigma)
      fz <- dnorm(z, mean = mu, sd = sigma)
      fy <- dnorm(y, mean = mu, sd = sigma)
      # Change global variable
      op <- par(mar=c(10,4,4,2)+0.1)
      # Curve
      curve(
        dnorm(x, mean = mu, sd = sigma),
        minimo,
        maximo,
        ylim = c(0, 1.2 * max(fx, fy, fz)),
        xlab = "",
        ylab = "",
        lwd = 3,
        axes = FALSE
      )
      polygon(c(y, rev(y)),
              c(fy, rep(0, length(fy))),
              col = "gray90")
      polygon(c(x, rev(x)),
              c(fx, rep(0, length(fx))),
              col = "red")
      polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
              col = "red")


      # Confidence level
      cl <- gettext("Confidence level", domain = "R-leem")
      text(0, 0.1, substitute(atop(bold(cl), 1 - alpha), list(cl = cl)), cex = 1)
      # Axis
      axis(1, at = -1.959964, labels = bquote(-Z[alpha / 2]), cex.axis = 1.3)
      axis(1, at = 1.959964, labels = bquote(Z[alpha / 2]), cex.axis = 1.3)
      axis(1, at = 4.2, labels = "Z", cex.axis = 1.5, tick = FALSE, pos = 0.01)
      axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Other axis
      axis(1, pos = -0.1, at = -1.959964, labels = bquote(bar(X)-Z[alpha / 2]%*%sigma / sqrt(n)), cex.axis = 1.3)
      axis(1, pos = -0.1, at = 1.959964, labels = bquote(bar(X)+Z[alpha / 2]%*%sigma / sqrt(n)), cex.axis = 1.3)
      axis(1, pos = -0.06, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
      axis(1, pos = -0.1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Errors region
      text(-3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      text(3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      # Pivotal quantity
      qp <- gettext("Pivotal quantity", domain = "R-leem")
      text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(S, sqrt(n)))%~%N(0,1)), list(qp = qp)), cex = 1.3, col = "blue")
      # Title
      if (is.null(main)) {
        cl <- gettext("Confidence interval for", domain = "R-leem")
        cl2 <- gettext("of normal population", domain = "R-leem")
        cl3 <- gettext("is known", domain = "R-leem")
        title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
      }
      # Formula
      mtext(bquote(IC(mu)[1-alpha]:bgroup("[",bar(X) - Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n))<=~mu<= bar(X) + Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n)),"]")), side = 3, line = -2)
      # Return global variable
      par(op)
    }
  }
  if (dist == "t-student") {
    if (ci == "two.sided") {
      p <- 0.05
      p <- c(p / 2, 1 - p / 2)
      mu <- 0; sigma <- 1
      q <- qnorm(p, mu, sigma)
      minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
      maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
      x <- seq(minimo, q[1], by = 0.01)
      z <- seq(q[2], maximo, by = 0.01)
      y <- seq(minimo, maximo, by = 0.01)
      fx <- dnorm(x, mean = mu, sd = sigma)
      fz <- dnorm(z, mean = mu, sd = sigma)
      fy <- dnorm(y, mean = mu, sd = sigma)
      # Change global variable
      op <- par(mar=c(10,4,4,2)+0.1)
      # Curve
      curve(
        dnorm(x, mean = mu, sd = sigma),
        minimo,
        maximo,
        ylim = c(0, 1.2 * max(fx, fy, fz)),
        xlab = "",
        ylab = "",
        lwd = 3,
        axes = FALSE
      )
      polygon(c(y, rev(y)),
              c(fy, rep(0, length(fy))),
              col = "gray90")
      polygon(c(x, rev(x)),
              c(fx, rep(0, length(fx))),
              col = "red")
      polygon(c(z, rev(z)), c(fz, rep(0, length(fz))),
              col = "red")


      # Confidence level
      cl <- gettext("Confidence level", domain = "R-leem")
      text(0, 0.1, substitute(atop(bold(cl), 1 - alpha), list(cl = cl)), cex = 1)
      # Axis
      axis(1, at = -1.959964, labels = bquote(-t[(alpha / 2*";"~nu)]), cex.axis = 1.3)
      axis(1, at = 1.959964, labels = bquote(t[(alpha / 2*";"~nu)]), cex.axis = 1.3)
      axis(1, at = 4.2, labels = "T", cex.axis = 1.5, tick = FALSE, pos = 0.01)
      axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Other axis
      axis(1, pos = -0.1, at = -1.959964, labels = bquote(bar(X)-t[(alpha / 2*";"~nu)]%*%S / sqrt(n)), cex.axis = 1.3)
      axis(1, pos = -0.1, at = 1.959964, labels = bquote(bar(X)+t[(alpha / 2*";"~nu)]%*%S / sqrt(n)), cex.axis = 1.3)
      axis(1, pos = -0.06, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
      axis(1, pos = -0.1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
      # Errors region
      text(-3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      text(3, 0.05, labels = bquote(alpha / 2), cex = 1.5, col = "red")
      # Pivotal quantity
      qp <- gettext("Pivotal quantity", domain = "R-leem")
      text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(S, sqrt(n)))%~%t(nu)), list(qp = qp)), cex = 1.2, col = "blue")
      # Degrees of freedom
      df <- gettext("Degrees of freedom", domain = "R-leem")
      text(-2.5, 0.3, labels = substitute(atop(df, nu == n - 1), list(df = df)), cex = 1.3, col = "blue")
      # Title
      if (is.null(main)) {
        cl <- gettext("Confidence interval for", domain = "R-leem")
        cl2 <- gettext("of normal population", domain = "R-leem")
        cl3 <- gettext("is unknown", domain = "R-leem")
        title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
      }
      # Formula
      mtext(bquote(IC(mu)[1-alpha]:bgroup("[",bar(X) - t[bgroup("(", frac(alpha, 2)*";"~nu, ")")] %*% frac(S, sqrt(n))<=~mu<= bar(X) + t[bgroup("(", frac(alpha, 2)*";"~nu, ")")] %*% frac(S, sqrt(n)),"]")), side = 3, line = -2)
      # Return global variable
      par(op)
    }
  }
}


