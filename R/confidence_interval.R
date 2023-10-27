#' @export
ci <- function(x, y = NULL, pop = "normal", interval = c("two.sided","L",
                                                 "less", "greater","G"), alpha = 0.05, exact = TRUE,
               correct = FALSE, paired = FALSE, plot = FALSE, details.plot = FALSE, main = NULL, rounding = 2, cex.exp = 1, dist.exp = -2, col.exp = "black", locpiv = FALSE, ...) {
  interval <- match.arg(interval)
  argaddit <- list(...)

  # Enter data
  if(missing(x)) {
    xfile <- file.choose(new = TRUE)
    x <- read.table(xfile, header = TRUE)
  }

  # Type of distributions
  if(pop == "normal") {
    # Insert standard deviation
    if (is.null(y)) {
      if (!any(names(argaddit) == "sd")) {
        sdev <- readline("Insert the value of population standard deviation? ")
        sdev <- as.numeric(sdev)
      } else sdev <- argaddit$sd
    # Confidence interval type
      if (any(interval == c("two.sided", "t", "T"))) {
        if (is.null(y)) {
          title <- paste(gettext(" Bilateral Confidence interval (Normal population) \n", domain = "R-leem"))
          conflevel <- paste(gettext("  Confidence level ", domain = "R-leem"), round((1 - alpha)*100, 2),
                           "%\n", sep = "")
          signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
                             sep = "")
          ztab <- qnorm(1 - alpha / 2)
          clevel <- 1 - alpha
          n <- length(x)
          xbar <- mean(x)
          confint <- round(xbar + c(-1, 1) * sdev / sqrt(n), rounding)

          results <- list(title = title, mean = xbar, sd = sdev, n = n, alpha = alpha, clevel = clevel, cp = ztab, confint = confint)


        } else {

        }
        if(plot == TRUE) {
          if(details.plot) {
            p <- alpha
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
            text(0, 0.1, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = -ztab, labels = substitute(-Z[alpha / 2]==-ztab, list(alpha = alpha, ztab = ztab)), cex.axis = 1.3)
            axis(1, pos = 0, at = ztab, labels = substitute(Z[alpha / 2]==ztab, list(alpha = alpha, ztab = ztab)), cex.axis = 1.3)
            axis(1, pos = 0.02, at = 4.2, labels = "Z", cex.axis = 1.5, tick = FALSE)
            #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
            axis(1, pos = -0.05, at = -ztab, labels = substitute(bar(X)-Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.1, at = -ztab, labels = substitute(xbar-ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.13, at = -ztab, labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.05, at = ztab, labels = substitute(bar(X)+Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.1, at = ztab, labels = substitute(xbar+ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.13, at = ztab, labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.04, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.05, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(-3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            text(3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("of normal population", domain = "R-leem")
              cl3 <- gettext("is known", domain = "R-leem")
              title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(mu)[clevel]:bgroup("[",bar(X) - Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n))<=~mu<= bar(X) + Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n)),"]"), list(alpha = alpha, n = n, sigma = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%N(0,1)), list(qp = qp)), cex = 1.3, col = "blue")
            } else {
              text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%N(0,1)), list(qp = qp)), cex = 1.3, col = "blue")
            }
          } else{
            p <- alpha
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
            text(0, 0.1, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = -ztab, labels = substitute(-Z[alpha / 2]==-ztab, list(alpha = alpha, ztab = ztab)), cex.axis = 1.3)
            axis(1, pos = 0, at = ztab, labels = substitute(Z[alpha / 2]==ztab, list(alpha = alpha, ztab = ztab)), cex.axis = 1.3)
            axis(1, pos = 0.02, at = 4.2, labels = "Z", cex.axis = 1.5, tick = FALSE)
            #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
            #axis(1, pos = -0.05, at = -ztab, labels = substitute(bar(X)-Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
            #axis(1, pos = -0.1, at = -ztab, labels = substitute(xbar-ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.05, at = -ztab, labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, col.axis = "blue")
            #axis(1, pos = -0.05, at = ztab, labels = substitute(bar(X)+Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
            #axis(1, pos = -0.1, at = ztab, labels = substitute(xbar+ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.05, at = ztab, labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, col.axis = "blue")
            axis(1, pos = -0.04, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.05, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(-3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            text(3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("of normal population", domain = "R-leem")
              cl3 <- gettext("is known", domain = "R-leem")
              title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(mu)[clevel]:bgroup("[",bar(X) - Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n))<=~mu<= bar(X) + Z[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n)),"]"), list(alpha = alpha, n = n, sigma = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%N(0,1)), list(qp = qp)), cex = 1.3, col = "blue")
            } else {
              text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%N(0,1)), list(qp = qp)), cex = 1.3, col = "blue")
            }
          }
        }
      }
    }
  }
  attr(results, "output") <- "confint"
  class(results) <- "leem"
  results
}
