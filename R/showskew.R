#' @export
showskew <- function(){
  # Change global variable
  op <- par(mar=c(5,4,4,2)+0.1) # mar=c(bottom, left, top, right)

  # Beta(5, 5)
  ############
  # parameters
  a <- 5
  b <- 5
  # Curve
  curve(
    dbeta(x, a, b),
    0,
    1,
    ylim = c(0, 4),
    xlab = "",
    ylab = "",
    lwd = 3,
    main = gettext("Skewness", domain = "R-leem"),
    axes = FALSE
  )
  x <- seq(0, 1, by = 0.01)
  fx <- dbeta(x, 5, 5)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90")
  # type
  mtext(gettext("Symmetric distribution", domain = "R-leem"), side = 1)
  skew <- gettext("skew", domain = "R-leem")
  text(0.5, 3, labels = substitute(skew*(X) == 0, list(skew = skew)))

  # Beta(2, 8)
  ############
  # parameters
  a <- 2
  b <- 8
  y <- seq(0, 1, by = 0.01)
  fy <- dbeta(x, a, b)
  # Curve
  lines(y, fy, lwd = 3, col = rgb(0, 175, 239, maxColorValue = 255,  alpha = 100))
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col = rgb(0, 175, 239, maxColorValue = 255, alpha = 100),
          border = rgb(0, 175, 239, maxColorValue = 255, alpha = 100))
  # type
  mtext(gettext("Positive skew", domain = "R-leem"), side = 1, adj = 0.1, col = rgb(0, 175, 239, maxColorValue = 255))
  skew <- gettext("skew", domain = "R-leem")
  text(0.1, 3.8, labels = substitute(skew(X) > 0, list(skew = skew)))

  # Beta(8, 2)
  ############
  # parameters
  a <- 8
  b <- 2
  z <- seq(0, 1, by = 0.01)
  fz <- dbeta(x, a, b)
  # Curve
  lines(z, fz, lwd = 3, col = rgb(62, 64, 144, maxColorValue = 255,  alpha = 100))
  polygon(c(z, rev(z)),
          c(fz, rep(0, length(fz))),
          col = rgb(62, 64, 144, maxColorValue = 255, alpha = 100),
          border = rgb(62, 64, 144, maxColorValue = 255, alpha = 100))
  # type
  mtext(gettext("Negative skew", domain = "R-leem"), side = 1, adj = 0.9, col = rgb(62, 64, 144, maxColorValue = 255))
  skew <- gettext("skew", domain = "R-leem")
  text(0.85, 3.8, labels = substitute(skew(X) < 0, list(skew = skew)))

  # Return global variable
  par(op)
}


