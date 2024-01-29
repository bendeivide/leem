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
    if (!is.null(y)) stop("The Normal pop is only for one population.", call. = FALSE, domain = "R-leem")

    # Insert standard deviation
      if (!any(names(argaddit) == "sd")) {
        sdev <- readline("Insert the value of population standard deviation? ")
        sdev <- as.numeric(sdev)
      } else sdev <- argaddit$sd
    # Confidence interval type
      if (any(interval == c("two.sided", "t", "T"))) {
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

          results <- list(pop = pop, title = title, mean = xbar, sd = sdev, n = n, alpha = alpha, clevel = clevel, cp = ztab, confint = confint)

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
  if(pop == "tstudent"){
    if (!is.null(y)) stop("The Normal pop is only for one population.", call. = FALSE, domain = "R-leem")
    # Insert standard deviation
        if (!any(names(argaddit) == "sd")) {
          sdev <- readline("Insert the value of sample standard deviation: ")
          sdev <- as.numeric(sdev)
        } else sdev <- argaddit$sd
        # Confidence interval type
        if (any(interval == c("two.sided", "t", "T"))) {
            title <- paste(gettext(" Bilateral Confidence interval (TStudent population) \n", domain = "R-leem"))
            conflevel <- paste(gettext("  Confidence level ", domain = "R-leem"), round((1 - alpha)*100, 2),
                               "%\n", sep = "")
            signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
                               sep = "")
            n <- length(x)
            ttab <- qt(1 - alpha / 2, n-1)
            clevel <- 1 - alpha
            xbar <- mean(x)
            confint <- round(xbar + c(-1, 1) * sdev / sqrt(n), rounding)

            results <- list(pop = pop, title = title, mean = xbar, sd = sdev, n = n, alpha = alpha, clevel = clevel, cp = c(ttab[2], ttab[1]), confint = confint)


          } else {

          }
          if(plot == TRUE) {
            if(details.plot) {
              p <- alpha
              p <- c(p / 2, 1 - p / 2)
              mu <- 0; sigma <- 1
              q <- qt(p, n-1)
              minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
              maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
              x <- seq(minimo, q[1], by = 0.01)
              z <- seq(q[2], maximo, by = 0.01)
              y <- seq(minimo, maximo, by = 0.01)
              fx <- dt(x, n-1)
              fz <- dt(z, n-1)
              fy <- dt(y, n-1)
              # Change global variable
              op <- par(mar=c(10,4,4,2)+0.1)
              # Curve
              curve(
                dt(x, n-1),
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
              axis(1, pos = 0, at = -ttab, labels = substitute(-T[alpha / 2]==-ttab, list(alpha = alpha, ttab = ttab)), cex.axis = 1.3)
              axis(1, pos = 0, at = ttab, labels = substitute(T[alpha / 2]==ttab, list(alpha = alpha, ttab = ttab)), cex.axis = 1.3)
              axis(1, pos = 0.02, at = 4.2, labels = "T", cex.axis = 1.5, tick = FALSE)
              #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
              # Other axis
              axis(1, pos = -0.05, at = -ttab, labels = substitute(bar(X)-T[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
              axis(1, pos = -0.1, at = -ttab, labels = substitute(xbar-ttab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ttab = ttab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.13, at = -ttab, labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.05, at = ttab, labels = substitute(bar(X)+T[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
              axis(1, pos = -0.1, at = ttab, labels = substitute(xbar+ttab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ttab = ttab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.13, at = ttab, labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.04, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
              axis(1, pos = -0.05, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
              # Errors region
              text(-3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
              text(3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
              # Title
              if (is.null(main)) {
                cl <- gettext("Confidence interval for", domain = "R-leem")
                cl2 <- gettext("of TStudent population", domain = "R-leem")
                cl3 <- gettext("is unknown", domain = "R-leem")
                title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
              }
              # Formula
              mtext(substitute(IC(mu)[clevel]:bgroup("[",bar(X) - T[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n))<=~mu<= bar(X) + T[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n)),"]"), list(alpha = alpha, n = n, sigma = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
              # Return global variable
              par(op)
              # Pivotal quantity
              qp <- gettext("Pivotal quantity", domain = "R-leem")
              if (locpiv) {
                loc <- locator(1)
                text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
              } else {
                text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
              }
            } else{
              p <- alpha
              p <- c(p / 2, 1 - p / 2)
              mu <- 0; sigma <- 1
              q <- qt(p, n-1)
              minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
              maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
              x <- seq(minimo, q[1], by = 0.01)
              z <- seq(q[2], maximo, by = 0.01)
              y <- seq(minimo, maximo, by = 0.01)
              fx <- dt(x, n-1)
              fz <- dt(z, n-1)
              fy <- dt(y, n-1)
              # Change global variable
              op <- par(mar=c(10,4,4,2)+0.1)
              # Curve
              curve(
                dt(x, n-1),
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
              axis(1, pos = 0, at = -ttab, labels = substitute(-T[alpha / 2]==-ttab, list(alpha = alpha, ttab = ttab)), cex.axis = 1.3)
              axis(1, pos = 0, at = ttab, labels = substitute(T[alpha / 2]==ttab, list(alpha = alpha, ttab = ttab)), cex.axis = 1.3)
              axis(1, pos = 0.02, at = 4.2, labels = "T", cex.axis = 1.5, tick = FALSE)
              #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
              # Other axis
              #axis(1, pos = -0.05, at = -ztab, labels = substitute(bar(X)-Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
              #axis(1, pos = -0.1, at = -ztab, labels = substitute(xbar-ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.05, at = -ttab, labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, col.axis = "blue")
              #axis(1, pos = -0.05, at = ztab, labels = substitute(bar(X)+Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
              #axis(1, pos = -0.1, at = ztab, labels = substitute(xbar+ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
              axis(1, pos = -0.05, at = ttab, labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, col.axis = "blue")
              axis(1, pos = -0.04, at = 4.2, labels = bquote(bar(X)), cex.axis = 1.5, tick = FALSE)
              axis(1, pos = -0.05, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
              # Errors region
              text(-3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
              text(3, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
              # Title
              if (is.null(main)) {
                cl <- gettext("Confidence interval for", domain = "R-leem")
                cl2 <- gettext("of TStudent population", domain = "R-leem")
                cl3 <- gettext("is unknown", domain = "R-leem")
                title(main = substitute(atop(bold(cl~mu~cl2), bold(sigma^2~cl3)), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
              }
              # Formula
              mtext(substitute(IC(mu)[clevel]:bgroup("[",bar(X) - T[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n))<=~mu<= bar(X) + T[bgroup("(", frac(alpha, 2), ")")] %*% frac(sigma, sqrt(n)),"]"), list(alpha = alpha, n = n, sigma = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
              # Return global variable
              par(op)
              # Pivotal quantity
              qp <- gettext("Pivotal quantity", domain = "R-leem")
              if (locpiv) {
                loc <- locator(1)
                text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
              } else {
                text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
              }

        }
      }
  }
  if(pop == "chisq"){
    if (!is.null(y)) stop("The Normal pop is only for one population.", call. = FALSE, domain = "R-leem")

    # Insert standard deviation
      if (!any(names(argaddit) == "sd")) {
        sdev <- readline("Insert the value of sample standard deviation: ")
        sdev <- as.numeric(sdev)
      } else sdev <- argaddit$sd
      # Confidence interval type
      if (any(interval == c("two.sided", "t", "T"))) {
          title <- paste(gettext(" Bilateral Confidence interval (Chi-Squared population) \n", domain = "R-leem"))
          conflevel <- paste(gettext("  Confidence level ", domain = "R-leem"), round((1 - alpha)*100, 2),
                             "%\n", sep = "")
          signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
                             sep = "")
          n <- length(x)
          df <- n-1
          ttab <- c(qchisq(1 - alpha / 2, n-1), qchisq(alpha/2, n-1))

          clevel <- 1 - alpha
          xbar <- mean(x)
          confint <- round(c((n-1) * sdev^2 / qchisq(1-alpha/2, n-1), (n-1) * sdev^2 / qchisq(alpha/2, n-1)), rounding)
          results <- list(pop = pop, title = title, mean = xbar, sd = sdev, n = n, alpha = alpha, clevel = clevel, cp = c(ttab[2], ttab[1]), confint = confint)


        } else {

        }
        if(plot == TRUE) {
          if(details.plot) {
            p <- alpha
            p <- c(p / 2, 1 - p / 2)
            mu <- 0; sigma <- 1
            q <- qchisq(p, n-1)
            minimo <- if (q[1] <= df - 2 * df) q[1] - 2*df else df - 2 * df
            maximo <- if (q[2] > df + 2 * df) q[2] + 2&df else df + 2 * df
            x <- seq(minimo, q[1], by = 0.01)
            z <- seq(q[2], maximo, by = 0.01)
            y <- seq(minimo, maximo, by = 0.01)
            fx <- dchisq(x, n-1)
            fz <- dchisq(z, n-1)
            fy <- dchisq(y, n-1)
            # Change global variable
            op <- par(mar=c(10,4,4,2)+0.1)
            # Curve
            curve(
              dchisq(x, n-1),
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
            text(ttab[1]+5, 0.05, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = ttab[2], labels = substitute(X[inf]^2==ttab, list(alpha = alpha, ttab = ttab[2])), cex.axis = 1.3)
            axis(1, pos = 0, at = ttab[1], labels = substitute(X[sup]^2==ttab, list(alpha = alpha, ttab = ttab[1])), cex.axis = 1.3)
            axis(1, pos = 0.02, at = maximo, labels = bquote(X^2), cex.axis = 1.5, tick = FALSE)
            #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
            axis(1, pos = -0.02, at = ttab[2], labels = substitute(bar(X)-T[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.04, at = ttab[2], labels = substitute(xbar-ttab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ttab = ttab[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.05, at = ttab[2], labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.02, at = ttab[1], labels = substitute(bar(X)+T[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.04, at = ttab[1], labels = substitute(xbar+ttab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ttab = ttab[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.05, at = ttab[1], labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.01, at = maximo, labels = bquote(sigma^2), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.02, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(ttab[2]-3, 0.02, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            text(ttab[1]+3, 0.02, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("and", domain = "R-leem")
              cl3 <- gettext("of Chi-Squared population", domain = "R-leem")
              title(main = substitute(bold(cl~sigma~cl2~sigma^2~cl3), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(sigma^2)[clevel]:bgroup("[",frac((n-1) * s^2, X^2["inf"]) <=~sigma^2<= frac((n-1) * s^2, X^2["sup"]),"]"), list(alpha = alpha, n1 = n, sigma = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
            } else {
              text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
            }
          } else{
            p <- alpha
            p <- c(p / 2, 1 - p / 2)
            mu <- 0; sigma <- 1
            q <- qchisq(p, n-1)
            minimo <- if (q[1] <= df - 2 * df) q[1] - 2*df else df - 2 * df
            maximo <- if (q[2] > df + 2 * df) q[2] + 2&df else df + 2 * df
            x <- seq(minimo, q[1], by = 0.01)
            z <- seq(q[2], maximo, by = 0.01)
            y <- seq(minimo, maximo, by = 0.01)
            fx <- dchisq(x, n-1)
            fz <- dchisq(z, n-1)
            fy <- dchisq(y, n-1)
            # Change global variable
            op <- par(mar=c(10,4,4,2)+0.1)
            # Curve
            curve(
              dchisq(x, n-1),
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
            text(ttab[1]+5, 0.05, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = ttab[2], labels = substitute(X[inf]^2==ttab, list(alpha = alpha, ttab = ttab[2])), cex.axis = 1.3)
            axis(1, pos = 0, at = ttab[1], labels = substitute(X[sup]^2==ttab, list(alpha = alpha, ttab = ttab[1])), cex.axis = 1.3)
            axis(1, pos = 0.02, at = maximo, labels = bquote(X^2), cex.axis = 1.5, tick = FALSE)
            #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
            #axis(1, pos = -0.05, at = -ztab, labels = substitute(bar(X)-Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
            #axis(1, pos = -0.1, at = -ztab, labels = substitute(xbar-ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.02, at = ttab[2], labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, col.axis = "blue")
            #axis(1, pos = -0.05, at = ztab, labels = substitute(bar(X)+Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
            #axis(1, pos = -0.1, at = ztab, labels = substitute(xbar+ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.02, at = ttab[1], labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, col.axis = "blue")
            axis(1, pos = -0.01, at = maximo, labels = bquote(sigma^2), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.02, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(ttab[2]-3, 0.02, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            text(ttab[1]+3, 0.02, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1.5, col = "red")
            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("and", domain = "R-leem")
              cl3 <- gettext("of Chi-Squared population", domain = "R-leem")
              title(main = substitute(bold(cl~sigma~cl2~sigma^2~cl3), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(sigma^2)[clevel]:bgroup("[",frac((n-1) * s^2, X^2["inf"]) <=~sigma^2<= frac((n-1) * s^2, X^2["sup"]),"]"), list(alpha = alpha, n1 = n, sigmas = sdev, clevel = 1 - alpha)), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
            } else {
              text(2.5, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n)))%~%T(desv)), list(qp = qp, desv = n-1)), cex = 1.3, col = "blue")
            }
          }

      }
    }
  if(pop == "f"){
    if (is.null(y)) stop("The 'y' argument is empty.", call. = FALSE, domain = "R-leem")
    sd <- NULL
    # Insert standard deviation
      if (!any(names(argaddit) == "sd1")) {
        sdev1 <- readline("Insert the value of sample 1 standard deviation: ")
        sdev1 <- as.numeric(sdev1)
      } else sdev1 <- argaddit$sd1
      if (!any(names(argaddit) == "sd2")) {
        sdev2 <- readline("Insert the value of sample 2 standard deviation: ")
        sdev2 <- as.numeric(sdev2)
      } else sdev2 <- argaddit$sd2


      # Confidence interval type
      if (any(interval == c("two.sided", "t", "T"))) {
          title <- paste(gettext(" Bilateral Confidence interval (F population) \n", domain = "R-leem"))
          conflevel <- paste(gettext("  Confidence level ", domain = "R-leem"), round((1 - alpha)*100, 2),
                             "%\n", sep = "")
          signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
                             sep = "")
          n1 <- length(x)
          n2 <- length(y)
          df1 <- n1-1
          df2 <- n2-1

          ttab <- c(qf(alpha/2, n2-1, n1-1), 1/qf(alpha/2, n1-1, n2-1))

          clevel <- 1 - alpha
          xbar <- mean(x)
          confint <- c((sdev1^2/sdev2^2)*ttab[1], (sdev1^2/sdev2^2)*ttab[2])
          results <- list(pop = pop, title = title, mean = xbar, sd = sd, sd1 = sdev1, sd2 = sdev2, n1 = n1, n2 = n2, alpha = alpha, clevel = clevel, cp = c(ttab[1], ttab[2]), confint = confint)
          if(plot == TRUE) {
          if(details.plot) {
            p <- alpha
            p <- c(p / 2, 1 - p / 2)
            mu <- 0;
            q <- qf(p, n1-1, n2-1)
            df <- length(x)+length(y)-2
            minimo <- if (ttab[2] <= (-4) * df) ttab[2] - 4 * df else 0
            maximo <- if (ttab[1] > 4 * df) ttab[1] + 4 * df else 4 * df
            x <- seq(minimo, q[1], by = 0.01)
            z <- seq(q[2], maximo, by = 0.01)
            y <- seq(minimo, maximo, by = 0.01)
            fx <- df(x, n1-1, n2-1)
            fz <- df(z, n1-1, n2-1)
            fy <- df(y, n1-1, n2-1)
            # Change global variable
            op <- par(mar=c(10,4,4,2)+0.1)
            # Curve
            curve(
              df(x, n1-1, n2-1),
              minimo,
              maximo,
              ylim = c(0, 1.5 * max(fx, fy, fz)),
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
            text(ttab[1]+5, 0.3, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = ttab[2], labels = substitute(F[inf]^2==ttab, list(alpha = alpha, ttab = ttab[2])), cex.axis = 1.3)
            axis(1, pos = 0, at = ttab[1], labels = substitute(F[sup]^2==ttab, list(alpha = alpha, ttab = ttab[1])), cex.axis = 1.3)
            axis(1, pos = 0.02, at = maximo, labels = bquote(F), cex.axis = 1.5, tick = FALSE)
                 #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
            axis(1, pos = -0.2, at = confint[1], labels = substitute(bar(X)-F[(alpha / 2)]%*%sigma / sqrt(n1), list(alpha = alpha, n1 = n1, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.2, at = confint[1], labels = substitute(xbar-ttab%*%sigma / sqrt(n1), list(n1 = n1, sigma = sdev, xbar = xbar, ttab = ttab[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = confint[1], labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = confint[2], labels = substitute(bar(X)+F[(alpha / 2)]%*%sigma / sqrt(n1), list(alpha = alpha, n1 =n1, sigma = sdev)), cex.axis = 1.3)
            axis(1, pos = -0.2, at = confint[2], labels = substitute(xbar+ttab%*%sigma / sqrt(n1), list(n1 = n1, sigma = sdev, xbar = xbar, ttab = ttab[1])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = confint[2], labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = maximo, labels = bquote(sigma[1]^2*","~sigma[2]^2), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.2, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(ttab[1]+0.6, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1, col = "red")
            text(ttab[2]+5, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1, col = "red")




            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("and", domain = "R-leem")
              cl3 <- gettext("of Two F population", domain = "R-leem")
              title(main = substitute(bold(cl~sigma[1]^2~cl2~sigma[2]^2~cl3), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(sigma[1]^2~","~sigma[2]^2)[clevel]:bgroup("[",frac(s[1]^2, s[2]^2)* F["inf"] <=~frac(sigma[1]^2,sigma[2]^2)<=~ frac(s[1]^2, s[2]^2)* F["sup"],"]"), list(sigma = symbol(sigma))), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n1)))%~%T(desv)), list(qp = qp, desv = n1-1)), cex = 1.3, col = "blue")
            } else {
              text(3, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n1)))%~%T(desv)), list(qp = qp, desv = n1-1)), cex = 1.3, col = "blue")
            }
          } else{
            p <- alpha
            p <- c(p / 2, 1 - p / 2)
            mu <- 0
            q <- qf(p, n1-1, n2-1)
            df <- length(x)+length(y)-2
            minimo <- if (ttab[2] <= (-4) * df) ttab[2] - 2 * sqrt(df) else 0
            maximo <- if (ttab[1] > 4 * df) ttab[1] + 2 * sqrt(df) else 2 * sqrt(df)
            x <- seq(minimo, q[1], by = 0.01)
            z <- seq(q[2], maximo, by = 0.01)
            y <- seq(minimo, maximo, by = 0.01)
            fx <- df(x, n1-1, n2-1)
            fz <- df(z, n1-1, n2-1)
            fy <- df(y, n1-1, n2-1)
            # Change global variable
            op <- par(mar=c(10,4,4,2)+0.1)
            # Curve
            curve(
              df(x, n1-1, n2-1),
              minimo,
              maximo,
              ylim = c(0, 1.5 * max(fx, fy, fz)),
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
            text(ttab[1]+5, 0.3, substitute(atop(bold(cl), 1 - alpha == clevel), list(cl = cl, alpha = alpha, clevel = 1 - alpha)), cex = 1)
            # Axis
            axis(1, pos = 0, at = ttab[2], labels = substitute(F[sup]^2==ttab, list(alpha = alpha, ttab = ttab[2])), cex.axis = 1.3)
            axis(1, pos = 0, at = ttab[1], labels = substitute(F[inf]^2==ttab, list(alpha = alpha, ttab = ttab[1])), cex.axis = 1.3)
            axis(1, pos = 0.02, at = maximo, labels = bquote(F), cex.axis = 1.5, tick = FALSE)
              #axis(1, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Other axis
              #axis(1, pos = -0.05, at = -ztab, labels = substitute(bar(X)-Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n = n, sigma = sdev)), cex.axis = 1.3)
              #axis(1, pos = -0.1, at = -ztab, labels = substitute(xbar-ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = confint[1], labels = substitute(bolditalic(LL)== confint, list(confint = confint[1])), cex.axis = 1.3, col.axis = "blue")
              #axis(1, pos = -0.05, at = ztab, labels = substitute(bar(X)+Z[(alpha / 2)]%*%sigma / sqrt(n), list(alpha = alpha, n =n, sigma = sdev)), cex.axis = 1.3)
              #axis(1, pos = -0.1, at = ztab, labels = substitute(xbar+ztab%*%sigma / sqrt(n), list(n = n, sigma = sdev, xbar = xbar, ztab = ztab)), cex.axis = 1.3, tick = FALSE, col.axis = "blue")
            axis(1, pos = -0.2, at = confint[2], labels = substitute(bolditalic(UL)== confint, list(confint = confint[2])), cex.axis = 1.3, col.axis = "blue")
            axis(1, pos = -0.2, at = maximo, labels = bquote(sigma[1]^2*","~sigma[2]^2), cex.axis = 1.5, tick = FALSE)
            axis(1, pos = -0.2, tick = TRUE, lwd.ticks = 0, lwd = 1, labels = FALSE)
            # Errors region
            text(ttab[1]+0.6, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1, col = "red")
            text(ttab[1]+5, 0.05, labels = substitute(alpha / 2, list(alpha = alpha)), cex = 1, col = "red")
            # Title
            if (is.null(main)) {
              cl <- gettext("Confidence interval for", domain = "R-leem")
              cl2 <- gettext("and", domain = "R-leem")
              cl3 <- gettext("of Two F population", domain = "R-leem")
              title(main = substitute(bold(cl~sigma[1]^2~cl2~sigma[2]^2~cl3), list(cl = cl, cl2 = cl2, cl3 = cl3)), cex.main = 1)
            }
            # Formula
            mtext(substitute(IC(sigma[1]^2~","~sigma[2]^2)[clevel]:bgroup("[",frac(s[1]^2, s[2]^2)* F["inf"] <=~frac(sigma[1]^2,sigma[2]^2)<=~ frac(s[1]^2, s[2]^2)* F["sup"],"]")), side = 3, line = dist.exp, cex =  cex.exp, col = col.exp)
            # Return global variable
            par(op)
            # Pivotal quantity
            qp <- gettext("Pivotal quantity", domain = "R-leem")
            if (locpiv) {
              loc <- locator(1)
              text(loc$x, loc$y, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n1)))%~%T(desv)), list(qp = qp, desv = n1-1)), cex = 1.3, col = "blue")
            } else {
              text(3, 0.3, labels = substitute(atop(qp, frac(bar(X) - mu, frac(sigma, sqrt(n1)))%~%T(desv)), list(qp = qp, desv = n1-1)), cex = 1.3, col = "blue")
            }
          }
        }
      }
  }

  attr(results, "output") <- "confint"
  class(results) <- "leem"
  results
}
