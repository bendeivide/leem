#' Ogive chart
#'
#' Generic function that plots the culmulative frequency curve.
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param decreasing Logical argument. Default is \code{FALSE}. If \code{decreasing = FALSE}, it represents the "ogive larger than", if \code{decreasing = TRUE}, it represents the "ogive less than".
#' @param both Logical argument. Default is \code{FALSE}. If \code{both = TRUE}, both o will be plotted. If \code{both = FALSE} otherside.
#' @param bars Logical argument. Default is \code{FALSE}. If \code{bars = TRUE}, the bars of the accumulated frequency will be inserted to plot, according to the \code{decreasing} argument. If \code{bars = FALSE} otherside.
#' @param histogram Logical argument. Default is \code{FALSE}. If \code{histogram = TRUE}, the histogram will be inserted to plot.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Insert the plot title. The default is \code{NULL}.
#' @param xlab Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgcol Insert the background color. This argument is only valid when \code{bg = TRUE}. The default is \code{bgcol="gray"}.
#' @param bgborder Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param barcol Insert the barplot color. The default is \code{barcol = "yellow"}. This argument is only valid when \code{bars = TRUE}.
#' @param histcol Insert the histogram color. The default is \code{histcol = barcol}. This argument is only valid when \code{histogram = TRUE}.
#' @param barborder Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
#' @param histborder Insert the histogram border color. This argument is only valid when \code{histogram = TRUE}. The default is histborder = barborder.
#' @param type Type of plot. The default is \code{type = "b"}, i.e., line and points. See \code{\link{graphical parameter}} for details.
#' @param lpcol Type of line color. The default is \code{lpcol = "black"}.
#' @param lwd numeric argument. The line width. The default is  \code{lwd = 2}.
#' @param pch Type of point. The default is  \code{pch = 19}.
#' @param lty Type of line. The default is  \code{lty = 2}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Ogive plot.
#'
#' @examples
#' library(leem)
#' # Example 1 - Both ogives
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE)
#'
#' # Example 2 - Insert barplot
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE, bars = TRUE)
#' # Example 3 - Insert histogram
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE, histogram = TRUE)
#'
#' @name ogive
#' @export
ogive <- function(x, ...) {
  UseMethod("ogive")
}
#' @rdname ogive
#' @method ogive leem
#' @export
ogive.leem <- function(x, freq = "a", decreasing = FALSE, both = FALSE, bars = FALSE, histogram = FALSE, bg = TRUE, main = NULL, xlab = NULL, ylab = NULL, grids = grid(col = "white"), bgcol = "gray", bgborder = NA, barcol = "yellow", histcol = barcol, barborder = "gray", histborder = barborder, type = "b", lpcol = "black", lwd = 2, pch = 19, lty = 2, ...) {
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!")
  if (is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if (!is.logical(both)) stop("The both argument must be logical!", call. = FALSE,
                              domain = "R-leem")
  if (!is.logical(decreasing)) stop("The decreasing argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (!is.logical(bg)) stop("The bg argument must be logical!", call. = FALSE,
                            domain = "R-leem")
  if (!is.logical(histogram)) stop("The histogram argument must be logical!", call. = FALSE,
                                   domain = "R-leem")
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$table$Groups)
    if (numchar) {
      # Auxiliar variables
      xmin <- x$table$Groups[1]
      xmax <- max(x$table$Groups)
      xvar <- x$table$Groups
      xvaraux <-  c(xmin - 1, x$table$Groups, xmax + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      yvar <- x$table$Fi
      yr <- x$table$Fr
      yp <- x$table$Fp
      yvar1 <- x$table$Fac1
      yvar2 <- x$table$Fac2
      yr1 <- x$table$Fac1p/100
      yr2 <- x$table$Fac2p/100
      yp1 <- x$table$Fac1p
      yp2 <- x$table$Fac2p
      if (both) {
        # Limiares
        xlim <- c(min(xvaraux), max(xvaraux))
        if (freq == "a") ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))
        if (freq == "r") ylim <- c(0, 1.2 * max(c(yr1, yr2)))
        if (freq == "p") ylim <- c(0, 1.2 * max(c(yp1, yp2)))


        # Area de plotagem
        plot.new()
        plot.window(xlim, ylim)

        # Labels
        if (is.null(main)) {
          main <- gettext("Ogives", domain = "R-leem")
        }
        if (is.null(xlab)) {
          xlab <- gettext("Groups", domain = "R-leem")
        }
        if (is.null(ylab)) {
          if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
          if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
          if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
        }
        title(main = main, xlab = xlab, ylab = ylab)

        if(bg) {
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
                 bgcol, border = bgborder)
        }

        # Grid
        grids

        # Inserindo barras
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar1, col = barcol1, border = barborder)

            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar2, col = barcol2, border = barborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr1, col = barcol1, border = barborder)

            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr2, col = barcol2, border = barborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp1, col = barcol1, border = barborder)

            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp2, col = barcol2, border = barborder)
          }
        }
        if (histogram) {
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar, col = histcol[1], border = histborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr, col = histcol[1], border = histborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp, col = histcol[1], border = histborder)
          }
        }
        # Pontos
        if (freq == "a") {
          points(c(xvar[1] - 1, xvar), c(0, yvar1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
          points(c(xvar, max(xvar) + 1), c(yvar2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "r") {
          points(c(xvar[1] - 1, xvar), c(0, yr1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
          points(c(xvar, max(xvar) + 1), c(yr2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "p") {
          points(c(xvar[1] - 1, xvar), c(0, yp1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
          points(c(xvar, max(xvar) + 1), c(yp2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }

        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }
      if (decreasing == FALSE & both == FALSE) {
        # Limiares
        xlim <- c(min(xvaraux), max(xvaraux))
        if (freq == "a") ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))
        if (freq == "r") ylim <- c(0, 1.2 * max(c(yr1, yr2)))
        if (freq == "p") ylim <- c(0, 1.2 * max(c(yp1, yp2)))


        # Area de plotagem
        plot.new()
        plot.window(xlim, ylim)

        # Labels
        if (is.null(main)) {
          main <- gettext("Ogive larger than", domain = "R-leem")
        }
        if (is.null(xlab)) {
          xlab <- gettext("Groups", domain = "R-leem")
        }
        if (is.null(ylab)) {
          if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
          if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
          if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
        }

        title(main = main, xlab = xlab, ylab = ylab)

        if(bg) {
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
                 bgcol, border = bgborder)
        }

        # Grid
        grids

        # Inserindo barras
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar1, col = barcol1, border = barborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr1, col = barcol1, border = barborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp1, col = barcol1, border = barborder)
          }
        }
        if (histogram) {
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar, col = histcol[1], border = histborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr, col = histcol[1], border = histborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp, col = histcol[1], border = histborder)
          }

        }

        # Pontos
        # Pontos
        if (freq == "a") {
          points(c(xvar[1] - 1, xvar), c(0, yvar1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "r") {
          points(c(xvar[1] - 1, xvar), c(0, yr1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "p") {
          points(c(xvar[1] - 1, xvar), c(0, yp1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }

        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }
      if (decreasing == TRUE & both == FALSE) {
        # Limiares
        xlim <- c(min(xvaraux), max(xvaraux))
        if (freq == "a") ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))
        if (freq == "r") ylim <- c(0, 1.2 * max(c(yr1, yr2)))
        if (freq == "p") ylim <- c(0, 1.2 * max(c(yp1, yp2)))

        # Area de plotagem
        plot.new()
        plot.window(xlim, ylim)

        # Labels
        if (is.null(main)) {
          main <- gettext("Ogive less than", domain = "R-leem")
        }
        if (is.null(xlab)) {
          xlab <- gettext("Groups", domain = "R-leem")
        }
        if (is.null(ylab)) {
          if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
          if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
          if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
        }

        title(main = main, xlab = xlab, ylab = ylab)

        # Retangulo do grafico (lembrar de corrigir com on.exit())
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)

        # Grid
        grids


        # Inserindo barras
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar2, col = barcol2, border = barborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr2, col = barcol2, border = barborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp2, col = barcol2, border = barborder)
          }
        }
        if (histogram) {
          if (freq == "a") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yvar, col = histcol[1], border = histborder)
          }
          if (freq == "r") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yr, col = histcol[1], border = histborder)
          }
          if (freq == "p") {
            rect(xvar - 0.5,
                 0,
                 xvar + 0.5,
                 yp, col = histcol[1], border = histborder)
          }

        }

        # Pontos
        if (freq == "a") {
          points(c(xvar, max(xvar) + 1), c(yvar2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "r") {
          points(c(xvar, max(xvar) + 1), c(yr2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }
        if (freq == "p") {
          points(c(xvar, max(xvar) + 1), c(yp2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        }

        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }
      # Alert
      cat(note(gettext("\n Note: The ogive graph has interpretation problems for the discrete quantitative variable", domain = "R-leem")))
    } else {
      cat(warn(gettext("\n Warning: The ogive graph is not used for this data type!", domain = "R-leem")), "\n")
    }

  }
  if (attr(x, "variable") == "continuous") {
    if (both) {
      xvar <- c(x$statistics$lower_lim[1], x$statistics$upper_lim)
      if (freq == "a") {
        yvar <- c(0, x$table$Fac1)
        yvar2 <- c(x$table$Fac2, 0)
      }
      if (freq == "r") {
        yvar <- c(0, x$table$Fac1p/100)
        yvar2 <- c(x$table$Fac2p/100, 0)
      }
      if (freq == "p") {
        yvar <- c(0, x$table$Fac1p)
        yvar2 <- c(x$table$Fac2p, 0)
      }



      # Limiares
      xlim <- c(min(xvar), max(xvar))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Ogives", domain = "R-leem")
      }
      if (is.null(xlab)) {
        xlab <- gettext("Classes", domain = "R-leem")
      }
      if (is.null(ylab)) {
        if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
        if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
        if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
      }
      title(main = main, xlab = xlab, ylab = ylab)

      if(bg) {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)
      }

      # Grid
      grids

      # Inserindo barras
      if (bars) {
        if(length(barcol) > 1) {
          barcol1 <- barcol[1]
          barcol2 <- barcol[2]
        } else barcol1 <- barcol2 <- barcol
        if (freq == "a") {
          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac1, col = barcol1, border = barborder)

          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac2, col = barcol2, border = barborder)
        }
        if (freq == "r") {
          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac1p/100, col = barcol1, border = barborder)

          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac2p/100, col = barcol2, border = barborder)
        }
        if (freq == "p") {
          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac1p, col = barcol1, border = barborder)

          rect(x$statistics$lower_lim,
               0,
               x$statistics$upper_lim,
               x$table$Fac2p, col = barcol2, border = barborder)
        }
      }
      if (histogram) {
        xvar3 <- x$statistics$lower_lim
        xvar4 <- x$statistics$upper_lim
        if (freq == "a") yvar3 <- x$table$Fi
        if (freq == "r") yvar3 <- x$table$Fr
        if (freq == "p") yvar3 <- x$table$Fp

        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol[1], border = histborder)

      }
      # Pontos
      points(xvar, yvar, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
      points(xvar, yvar2, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
    if (decreasing == FALSE & both == FALSE) {
      xvar <- c(x$statistics$lower_lim[1], x$statistics$upper_lim)
      if (freq == "a") {
        yvar <- c(0, x$table$Fac1)
      }
      if (freq == "r") {
        yvar <- c(0, x$table$Fac1p/100)
      }
      if (freq == "p") {
        yvar <- c(0, x$table$Fac1p)
      }

      # Limiares
      xlim <- c(min(xvar), max(xvar))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Ogive larger than", domain = "R-leem")
      }
      if (is.null(xlab)) {
        xlab <- gettext("Classes", domain = "R-leem")
      }
      if (is.null(ylab)) {
        if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
        if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
        if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
      }

      title(main = main, xlab = xlab, ylab = ylab)

      if(bg) {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)
      }

      # Grid
      grids

      # Inserindo barras
      if (bars) {
        if (freq == "a") aux <- x$table$Fac1
        if (freq == "r") aux <- x$table$Fac1p/100
        if (freq == "p") aux <- x$table$Fac1p
        rect(x$statistics$lower_lim,
             0,
             x$statistics$upper_lim,
             aux, col = barcol, border = barborder)
      }
      # Histograma
      if (histogram) {
        xvar3 <- x$statistics$lower_lim
        xvar4 <- x$statistics$upper_lim
        if (freq == "a") yvar3 <- x$table$Fi
        if (freq == "r") yvar3 <- x$table$Fr
        if (freq == "p") yvar3 <- x$table$Fp
        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol[1], border = histborder)

      }

      # Pontos
      points(xvar, yvar, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
    if (decreasing == TRUE & both == FALSE) {
      xvar <- c(x$statistics$lower_lim[1], x$statistics$upper_lim)
      if (freq == "a") {
        yvar <- c(x$table$Fac2, 0)
      }
      if (freq == "r") {
        yvar <- c(x$table$Fac2p/100, 0)
      }
      if (freq == "p") {
        yvar <- c(x$table$Fac2p, 0)
      }

      # Limiares
      xlim <- c(min(xvar), max(xvar))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Ogive less than", domain = "R-leem")
      }
      if (is.null(xlab)) {
        xlab <- gettext("Classes", domain = "R-leem")
      }
      if (is.null(ylab)) {
        if (freq == "a") ylab <- gettext("Frequency", domain = "R-leem")
        if (freq == "r") ylab <- gettext("Relative frequency", domain = "R-leem")
        if (freq == "p") ylab <- gettext("Percentage frequency (%)", domain = "R-leem")
      }

      title(main = main, xlab = xlab, ylab = ylab)

      if(bg) {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)
      }

      # Grid
      grids


      # Inserindo barras
      if (bars) {
        if (freq == "a") aux <- x$table$Fac2
        if (freq == "r") aux <- x$table$Fac2p/100
        if (freq == "p") aux <- x$table$Fac2p
        rect(x$statistics$lower_lim,
             0,
             x$statistics$upper_lim,
             aux, col = barcol, border = barborder)
      }
      # Histograma
      if (histogram) {
        xvar3 <- x$statistics$lower_lim
        xvar4 <- x$statistics$upper_lim
        if (freq == "a") yvar3 <- x$table$Fi
        if (freq == "r") yvar3 <- x$table$Fr
        if (freq == "p") yvar3 <- x$table$Fp
        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol[1], border = histborder)

      }


      # Pontos
      points(xvar, yvar, col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
  }
  invisible(x)
}
