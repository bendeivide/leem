#' Plot of interpretation about skewsness
#'
#' \code{showskew} Interpretation of asymmetry based on frequency distributions
#'
#' @param measures shows the measures of position or not (default \code{FALSE}).

#' @return \code{showskew} returns a plot with the skewsness characteristics.
#'
#' @examples
#' # Loading package
#' library(leem)
#' \dontrun{
#' showskew()
#' }
#'
#' @export
showskew <- function(mpos = FALSE){
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

  # auxiliar parameters of mtext()
  line <- 0
  if (mpos) {
    line <- 1
    # Position measures
    media <- a / (a + b)
    axis(1, at = media, labels = bquote(bar(x)==~md==mo), pos = 0.1, tick = FALSE)
  }
  # type
  mtext(gettext("Symmetric distribution", domain = "R-leem"), side = 1, line = line)
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

  # auxiliar parameters of mtext()
  line <- 0
  if (mpos) {
    line <- 1
    # Position measures
    axis(1, at = 0.15, labels = bquote(bar(x)>~md>mo), pos = 0.1, tick = FALSE, col.axis = rgb(0, 175, 239, maxColorValue = 255))
  }

  # type
  mtext(gettext("Positive skew", domain = "R-leem"), side = 1, adj = 0.1, col = rgb(0, 175, 239, maxColorValue = 255),
        line = line)
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

  # auxiliar parameters of mtext()
  line <- 0
  if (mpos) {
    line <- 1
    # Position measures
    axis(1, at = 0.85, labels = bquote(bar(x)<~md<mo), pos = 0.1, tick = FALSE, col.axis = rgb(62, 64, 144, maxColorValue = 255))
  }

  # type
  mtext(gettext("Negative skew", domain = "R-leem"), side = 1, adj = 0.9, col = rgb(62, 64, 144, maxColorValue = 255),
        line = line)
  skew <- gettext("skew", domain = "R-leem")
  text(0.85, 3.8, labels = substitute(skew(X) < 0, list(skew = skew)))

  # Return global variable
  par(op)
}

# export png
#png(file = "assimetriacmedidas.png", width = 2500, height = 1500, res = 300)
#dev.off()


