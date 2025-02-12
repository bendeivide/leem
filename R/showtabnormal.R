#' showtabnormal
#'
#' Detailing of the Ztable, showing the main information contained in this type of table.
#'
#' @param z Parameter for lacate the z value on the table; default \code{NULL} means error if not insert a number.
#'
#' @examples
#' library(leem)
#' # Example 1
#' showtabnormal(1)
#' @importFrom diagram curvedarrow

#' @export
showtabnormal <- function(z) {
  # Change decimals
  # op <- options(OutDec = ",")
  # Restart decimals
  # options(op)
  #######################
  # Defensive programming
  if (z == 0) stop("Choose a value greater than 0", call. = FALSE, domain = "R-leem")
  #######################
  z <- round(z, 2)
  zchar <- as.character(z)
  zchar1 <-  substr(zchar, start = 1, stop = 1)
  zchar2 <-  substr(zchar, start = 3, stop = 3)
  zchar3 <-  substr(zchar, start = 4, stop = 4)
  if (z < 0.1) {
    z1 <- 0
  } else {
    z1 <- as.numeric(paste0(zchar1, ".", zchar2, sep = ""))
  }
  z2 <- as.numeric(paste0("0.0", zchar3, sep = ""))
  plot.new()
  plot.window(xlim = c(-4, 8), ylim = c(-0.6, 0.5))
  #temporario (eixos)
  # axis(1); axis(2)
  # axis(2, at = seq(-1, 0.5, 0.1), srt = -30, xpd = TRUE)
  # axis(1, at = seq(-4, 8, 0.7), srt = -30, xpd = TRUE)
  # abline(h = seq(-1, 0.5, 0.1))
  # abline(v = seq(-4, 8, 0.7))
  # Plot
  x <- seq(-4, 4, by = 0.01)
  dx <- dnorm(x)
  polygon(c(x, rev(x)),
          c(dx, rep(0, length(dx))),
          col="lightblue")
  axis(1, at = 0, pos = 0, cex.axis = 1.5)
  text(0, 0.5, labels = bquote(bold("Standard normal distribution")), cex = 1.5)

  # Background of table
  rect(4.4, -0.3, 7.9, 0.2, col = "lightblue", border = NA)

  ## rect digists
  rect(5.8, 0.4, 6.5, 0.5, col = "#FF0040", border = NA)
  rect(7.1, 0.4, 8.1, 0.5, col = "#FF0040", border = NA)
  # Critical point
  text(3.8, 0.45, labels = substitute(bold(z == zaux), list(zaux = z)), adj = 0,
       cex = 1.5)
  text(5.8, 0.45, labels = substitute(z1aux~~"+", list(z1aux = if (z1 == 0) "0.0" else z1)), adj = 0,
       cex = 1.5)
  text(7.2, 0.45, labels = substitute(z2aux, list(z2aux = if(z2 == 0) "0.00" else z2)), adj = 0,
       cex = 1.5)
  text(5.2, 0.45, labels = substitute(bold(a%->%b), list(a = "", b = "")), adj = 0,
       cex = 1.5)
  if (z1  == 0) {
    # Table
    rect(4.4, 0, 7.9, 0.1, col = "lightgray", border = NA)
    if (z2 == 0.01) {
      rect(5.86, -0.3, 6.5, 0.2, col = "lightgray", border = NA)
      rect(5.72, 0, 6.7, 0.1, col = "lightblue4", border = NA) # probability
      segments(4.4, 0.2, 7.9, 0.2, lwd = 2)
      segments(4.4, 0.1, 7.9, 0.1, lwd = 2)
      segments(5.1, 0.2, 5.1, -0.3, lwd = 2)
      arrows(5.2, 0.05, 5.72, 0.05, length = 0.1, code = 2, lwd = 1, col = "red")
      text(6.1, 0.25, labels = bquote(bold("Z Table")), cex = 1.5)
      text(4.75, 0.15, labels = bquote(bold("Z")), cex = 1.5)
      text(6.18, 0.15, labels = bquote(bold(0.01)), cex = 1.2)
      text(4.75, 0.05, labels = bquote(bold(0.0)), cex = 1.2)
      ##
      text(5.48,0.15, labels = bquote("0.00"), cex = 1.2)
      text(6.85,0.15, labels = bquote(bold(ldots)), cex = 1.2)
      ## arrows- first two digits
      diagram::curvedarrow(from = c(6.18, 0.4), to = c(4.4, 0.3),
                           curve = 0, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(4.4, 0.3), to = c(4.4, 0.05),
                           curve = 1.6, arr.pos = 1, lwd = 2,
                           lcol = "red")
      ## arrows - last digit
      diagram::curvedarrow(from = c(7.5, 0.4), to = c(7.9, 0.35),
                           curve = -0.02, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(7.9, 0.35), to = c(6.7, 0.18),
                           curve = -0.01, arr.pos = 0.9, lwd = 2,
                           lcol = "red")
      ## Probability
      text(6.18,0.05, labels = substitute(bold(prob), list(prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ##
      rect(4.75, -0.5, 8.2, -0.4, col = "lightblue4", border = NA)
      text(6.5,-0.45, labels = substitute(bold(P*"("*0<~Z<zaux*")"*"="*prob), list(zaux  = z,
                                                                                   prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ## Probability area
      y <- seq(0, z, by = 0.01)
      dy <- dnorm(y)
      polygon(c(y, rev(y)),
              c(dy, rep(0, length(dy))),
              col="lightblue4")
      axis(1, at = z, pos = 0, lwd = 0, tick = TRUE, lwd.ticks = 1,
           cex.axis = 1.5)
      segments(5.4, 0.42, z, 0.42, lwd = 2, col = "red")
      arrows(z, 0.42, z, 0, length = 0.1, code = 2, lwd = 2, col = "red")
      ## arrows of probability
      segments((0 + z) / 2, dnorm(z) / 2, (0 + z) / 2, -0.45, lwd = 2, col = "gray2")
      arrows((0 + z) / 2, -0.45, 4.4, -0.45, length = 0.1, code = 2, lwd = 2, col = "gray2")
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "lightblue4", pch = 19)
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "gray9", lwd = 2)
      arrows(6.18, 0, 5.86, -0.35, length = 0.1, code = 2, lwd = 2, col = "gray2")


    } else {
      rect(6.5, -0.3, 7.2, 0.2, col = "lightgray", border = NA)
      rect(6.2, 0, 7.4, 0.1, col = "lightblue4", border = NA) # probability
      segments(4.4, 0.2, 7.9, 0.2, lwd = 2)
      segments(4.4, 0.1, 7.9, 0.1, lwd = 2)
      segments(5.1, 0.2, 5.1, -0.3, lwd = 2)
      arrows(5.2, 0.05, 6, 0.05, length = 0.1, code = 2, lwd = 1, col = "red")
      text(6.1,0.25, labels = bquote(bold("Z Table")), cex = 1.5)
      text(4.75,0.15, labels = bquote(bold("Z")), cex = 1.5)
      text(5.48,0.15, labels = bquote(bold(ldots)), cex = 1.2)
      text(4.75, 0.05, labels = bquote(bold(0.00)), cex = 1.2)
      ##
      text(6.18,0.15, labels = substitute(bold(zaux2), list(zaux2 = z2 - 0.01)), cex = 1.2)
      text(6.85,0.15, labels = substitute(bold(zaux2), list(zaux2 = z2)), cex = 1.2)
      text(7.55,0.15, labels = bquote(bold(ldots)), cex = 1.2)
      ## arrows- first two digits
      diagram::curvedarrow(from = c(6.18, 0.4), to = c(4.4, 0.3),
                           curve = 0, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(4.4, 0.3), to = c(4.4, 0.05),
                           curve = 1.6, arr.pos = 1, lwd = 2,
                           lcol = "red")
      ## arrows - last digit
      diagram::curvedarrow(from = c(7.5, 0.4), to = c(7.9, 0.35),
                           curve = -0.02, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(7.9, 0.35), to = c(7.2, 0.2),
                           curve = -0.01, arr.pos = 0.9, lwd = 2,
                           lcol = "red")
      ## Probability
      text(6.78,0.05, labels = substitute(bold(prob), list(prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ##
      rect(4.75, -0.5, 8.2, -0.4, col = "lightblue4", border = NA)
      text(6.5,-0.45, labels = substitute(bold(P*"("*0<~Z<zaux*")"*"="*prob), list(zaux  = z,
                                                                                   prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ## Probability area
      y <- seq(0, z, by = 0.01)
      dy <- dnorm(y)
      polygon(c(y, rev(y)),
              c(dy, rep(0, length(dy))),
              col="lightblue4")
      axis(1, at = z, pos = 0, lwd = 0, tick = TRUE, lwd.ticks = 1,
           cex.axis = 1.5)
      segments(5.4, 0.42, z, 0.42, lwd = 2, col = "red")
      arrows(z, 0.42, z, 0, length = 0.1, code = 2, lwd = 2, col = "red")
      ## arrows of probability
      segments((0 + z) / 2, dnorm(z) / 2, (0 + z) / 2, -0.45, lwd = 2, col = "gray2")
      arrows((0 + z) / 2, -0.45, 4.4, -0.45, length = 0.1, code = 2, lwd = 2, col = "gray2")
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "lightblue4", pch = 19)
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "gray9", lwd = 2)
      arrows(6.78, 0, 5.86, -0.35, length = 0.1, code = 2, lwd = 2, col = "gray2")

    }

  } else{
    if (z2 == 0) {
      # Table
      rect(4.4, -0.2, 7.9, -0.1, col = "lightgray", border = NA)
      rect(5.1, -0.3, 5.86, 0.2, col = "lightgray", border = NA)
      ##
      rect(5.1, -0.2, 6.3, -0.1, col = "lightblue4", border = NA) # Probability
      arrows(5.48, 0.09, 5.48, -0.09, length = 0.1, code = 2, lwd = 1, col = "red")
      segments(4.4, 0.2, 7.9, 0.2, lwd = 2)
      segments(4.4, 0.1, 7.9, 0.1, lwd = 2)
      segments(5.1, 0.2, 5.1, -0.3, lwd = 2)
      text(6.1,0.25, labels = bquote(bold("Z Table")), cex = 1.5)
      text(4.75,0.15, labels = bquote(bold("Z")), cex = 1.5)
      text(5.48,0.15, labels = bquote("0.00"), cex = 1.2)
      text(4.75,0.05, labels = bquote(bold(cdots)), cex = 1.5, srt = 90, xpd = TRUE)
      ##
      text(4.75,-0.05, labels = substitute(bold(zaux3), list(zaux3 = z1 - 0.1)), cex = 1.2)
      text(4.75,-0.15, labels = substitute(bold(zaux2), list(zaux2 = z1)), cex = 1.2)
      text(4.75,-0.25, labels = bquote(bold(cdots)), cex = 1.5, srt = 90, xpd = TRUE)
      ##
      text(6.18,0.15, labels = substitute(0.01), cex = 1.2)
      text(6.85,0.15, labels = bquote(bold(ldots)), cex = 1.2)

      ## arrows- first two digits
      diagram::curvedarrow(from = c(6.18, 0.4), to = c(4.4, 0.3),
                           curve = -0.04, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(4.4, 0.3), to = c(4.3, -0.1),
                           curve = 1., arr.pos = 1, lwd = 2,
                           lcol = "red")
      ## arrows - last digit
      diagram::curvedarrow(from = c(7.5, 0.4), to = c(7.9, 0.35),
                           curve = -0.02, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(7.9, 0.35), to = c(5.86, 0.18),
                           curve = 0, arr.pos = 0.9, lwd = 2,
                           lcol = "red")
      ## Probability

      text(5.2,-0.15, labels = substitute(bold(prob), list(prob = round(pnorm(z) - 0.5, 4))), cex = 1.2, adj = 0)
      ##
      rect(4.75, -0.5, 8.2, -0.4, col = "lightblue4", border = NA)
      text(6.5,-0.45, labels = substitute(bold(P*"("*0<~Z<zaux*")"*"="*prob), list(zaux  = z,
                                                                                   prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ## Probability area
      y <- seq(0, z, by = 0.01)
      dy <- dnorm(y)
      polygon(c(y, rev(y)),
              c(dy, rep(0, length(dy))),
              col="lightblue4")
      axis(1, at = z, pos = 0, lwd = 0, tick = TRUE, lwd.ticks = 1,
           cex.axis = 1.5)
      segments(5.4, 0.42, z, 0.42, lwd = 2, col = "red")
      arrows(z, 0.42, z, 0, length = 0.1, code = 2, lwd = 2, col = "red")
      ## arrows of probability
      segments((0 + z) / 2, dnorm(z) / 2, (0 + z) / 2, -0.45, lwd = 2, col = "gray2")
      arrows((0 + z) / 2, -0.45, 4.4, -0.45, length = 0.1, code = 2, lwd = 2, col = "gray2")
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "lightblue4", pch = 19)
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "gray9", lwd = 2)
      arrows(5.48, -0.2, 6.3, -0.35, length = 0.1, code = 2, lwd = 2, col = "gray2")


    } else {
      # Table
      rect(4.4, -0.2, 7.9, -0.1, col = "lightgray", border = NA)
      rect(6.5, -0.3, 7.2, 0.2, col = "lightgray", border = NA)
      ##
      arrows(5.2, -0.15, 6.17, -0.15, length = 0.1, code = 2, lwd = 1, col = "red")
      arrows(6.85, 0.09, 6.85, -0.09, length = 0.1, code = 2, lwd = 1, col = "red")
      segments(4.4, 0.2, 7.9, 0.2, lwd = 2)
      segments(4.4, 0.1, 7.9, 0.1, lwd = 2)
      segments(5.1, 0.2, 5.1, -0.3, lwd = 2)
      text(6.1,0.25, labels = bquote(bold("Z Table")), cex = 1.5)
      text(4.75,0.15, labels = bquote(bold("Z")), cex = 1.5)
      text(5.48,0.15, labels = bquote(bold(ldots)), cex = 1.5)
      text(4.75,0.05, labels = bquote(bold(cdots)), cex = 1.5, srt = 90, xpd = TRUE)
      ##
      text(4.75,-0.05, labels = substitute(bold(zaux3), list(zaux3 = z1 - 0.1)), cex = 1.2)
      text(4.75,-0.15, labels = substitute(bold(zaux2), list(zaux2 = z1)), cex = 1.2)
      text(4.75,-0.25, labels = bquote(bold(cdots)), cex = 1.5, srt = 90, xpd = TRUE)
      ##
      text(6.18,0.15, labels = substitute(bold(zaux2), list(zaux2 = z2 - 0.01)), cex = 1.2)
      text(6.85,0.15, labels = substitute(bold(zaux2), list(zaux2 = z2)), cex = 1.2)
      text(7.55,0.15, labels = bquote(bold(ldots)), cex = 1.2)

      ## arrows- first two digits
      diagram::curvedarrow(from = c(6.18, 0.4), to = c(4.4, 0.3),
                           curve = -0.04, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(4.4, 0.3), to = c(4.3, -0.1),
                           curve = 1., arr.pos = 1, lwd = 2,
                           lcol = "red")
      ## arrows - last digit
      diagram::curvedarrow(from = c(7.5, 0.4), to = c(7.9, 0.35),
                           curve = -0.02, arr.pos = 1, arr.type = "none", lwd = 2,
                           lcol = "red")
      diagram::curvedarrow(from = c(7.9, 0.35), to = c(7.4, 0.2),
                           curve = -0.05, arr.pos = 0.8, lwd = 2,
                           lcol = "red")
      ## Probability
      rect(6.18, -0.2, 7.55, -0.1, col = "lightblue4", border = NA)
      text(6.85,-0.15, labels = substitute(bold(prob), list(prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ##
      rect(4.75, -0.5, 8.2, -0.4, col = "lightblue4", border = NA)
      text(6.5,-0.45, labels = substitute(bold(P*"("*0<~Z<zaux*")"*"="*prob), list(zaux  = z,
                                                                                   prob = round(pnorm(z) - 0.5, 4))), cex = 1.2)
      ## Probability area
      y <- seq(0, z, by = 0.01)
      dy <- dnorm(y)
      polygon(c(y, rev(y)),
              c(dy, rep(0, length(dy))),
              col="lightblue4")
      axis(1, at = z, pos = 0, lwd = 0, tick = TRUE, lwd.ticks = 1,
           cex.axis = 1.5)
      segments(5.4, 0.42, z, 0.42, lwd = 2, col = "red")
      arrows(z, 0.42, z, 0, length = 0.1, code = 2, lwd = 2, col = "red")
      ## arrows of probability
      segments((0 + z) / 2, dnorm(z) / 2, (0 + z) / 2, -0.45, lwd = 2, col = "gray2")
      arrows((0 + z) / 2, -0.45, 4.4, -0.45, length = 0.1, code = 2, lwd = 2, col = "gray2")
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "lightblue4", pch = 19)
      points((0 + z) / 2, dnorm(z) / 2, cex = 1, col = "gray9", lwd = 2)
      arrows(6.85, -0.25, 6.68, -0.35, length = 0.1, code = 2, lwd = 2, col = "gray2")
    }
  }



  ## Title
  title(main = "Understanding the standard normal distribution table",
        col.main = "blue", cex.main = 1.5)
}


