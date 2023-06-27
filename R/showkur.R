#' Plot of interpretation about Kurtosis
#'
#' \code{showkur} Interpretation of kutosis
#'
#' @return \code{showkur} returns a plot with the kurtosis characteristics.
#'
#' @examples
#' # Loading package
#' library(leem)
#' \dontrun{
#' showkur()
#' }
#'
#' @export
showkur <- function(){
  # Supress warnings
  defaultW <- getOption("warn")
  options(warn = -1)
  # Limits
  a <- -12; b <- 12
  # Normal(0,1)
  ############
  # Curve
  curve(
    dnorm(x),
    a,
    b,
    xlim = c(a, b),
    xlab = "",
    ylab = "",
    lwd = 3,
    main = gettext("Kurtosis", domain = "R-leem"),
    col = "gray90",
    axes = FALSE
  )
  x <- seq(a, b, by = 0.01)
  fx <- dnorm(x)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = "gray90",
          border = "gray90")


  # type
  arrows(0, 0.3, 4, 0.35, length = 0.15, angle = 15, lwd = 2)
  kur <- gettext("Leptokurtic distribution", domain = "R-leem")
  text(5, 0.36, labels = kur)



  # # Normal(0, 2)
  # ############
  # Curve
  curve(
    dnorm(x, sd = 2),
    a,
    b,
    xlim = c(a, b),
    xlab = "",
    ylab = "",
    lwd = 3,
    add = TRUE,
    col = rgb(0, 175, 239, maxColorValue = 255,  alpha = 100),
    axes = FALSE
  )
  x <- seq(a, b, by = 0.01)
  fx <- dnorm(x, sd = 2)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = rgb(0, 175, 239, maxColorValue = 255,  alpha = 100),
          border = rgb(0, 175, 239, maxColorValue = 255,  alpha = 100))

  # type
  arrows(0, 0.15, -5, 0.2, length = 0.15, angle = 15, lwd = 2)
  kur <- gettext("Mesokurtic distribution", domain = "R-leem")
  text(-6, 0.22, labels = kur)


  # # Normal(0, 4)
  # ############
  # # Curve
  curve(
    dnorm(x, sd = 4),
    a,
    b,
    xlim = c(a, b),
    xlab = "",
    ylab = "",
    lwd = 3,
    add = TRUE,
    col = rgb(62, 64, 144, maxColorValue = 255,  alpha = 100),
    axes = FALSE
  )
  x <- seq(a, b, by = 0.01)
  fx <- dnorm(x, sd = 4)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = rgb(62, 64, 144, maxColorValue = 255,  alpha = 100),
          border = rgb(62, 64, 144, maxColorValue = 255,  alpha = 100))

  # type
  arrows(0, 0.05, 5, 0.1, length = 0.15, angle = 15, lwd = 2)
  kur <- gettext("Platykurtic distribution", domain = "R-leem")
  text(7, 0.112, labels = kur)

  # Desabilitar warnings global
  options(warn = defaultW)
}

# export png
#png(file = "E:/BEN_PROD/LIVROS/ESTATISTICA BASICA APLICADA AS CIENCIAS AGRARIAS/kutosis.png", width = 2500, height = 1500, res = 300, bg = "transparent")
#dev.off()


