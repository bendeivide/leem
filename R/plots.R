#' Pie Chart
#'
#' Draw a pie chart.
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param labels One or more expressions or character strings giving names for the slices
#' @param col Character vector. Default \code{col = heat.colors(5)}.
#' @param border Logical argument (default \code{FALSE}).
#' @param main Title name.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' library(leem)
#' # Example 1
#' school <- rep(c("high", "university", "basic"), 3:5)
#' x <- sample(school, 30, TRUE) |>
#'   new_leem() |>
#'   tabfreq(ordered = c("basic", "high", "university"))
#' # Example 2
#' x <- rbinom(36, 10, 0.6)
#' x <- new_leem(x, variable = "discrete")
#' x <- tabfreq(x)
#' piechart(x)
#' @export
piechart <- function(x, labels = NULL, col = heat.colors(5, 1), border = FALSE, main = NULL, ...) {
  # defensive programming
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if(is.null(main)) main <- gettext("Pie Chart", domain = "R-leem")
  if (attr(x, "variable") == "discrete") {
    # defensive programming
    if (!is.null(labels)) {
      if (length(labels) != length(x$table$Groups)) {
        stop("The length of the labels argument must equal the number categories!", call. = FALSE, domain = "R-leem")
      }
    }
    if (is.null(labels)) {
      labels <- paste0(x$table$Groups, " (", x$table$Fp, "%", ")")
    } else{
      labels <- paste0(labels, " (", x$table$Fp, "%", ")")
    }

    graphics::pie(x$table$Fi, labels = labels, col = col, border = border,
                  main = main, ...)
  }
  if (attr(x, "variable") == "continuous") {
    # defensive programming
    if (!is.null(labels)) {
      if (length(labels) != length(x$table$Classes)) {
        stop("The length of the labels argument must equal the number classes!", call. = FALSE, domain = "R-leem")
      }
    }
    if (is.null(labels)) {
      labels <- paste0("[", x$statistics$lower_lim, ";",  x$statistics$upper_lim, ")", " (", x$table$Fp, "%", ")")
    } else{
      labels <- paste0(labels, " (", x$table$Fp, "%", ")")
    }

    graphics::pie(x$table$Fi, labels = labels, col = col, border = border,
                  main = main, ...)
  }
}




#' Stick chart
#'
#' Stick chart for discrete data
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Insert the plot title. The default is \code{NULL}.
#' @param xlab Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgcol Insert the background color. This argument is only valid when \code{bg = TRUE}. The default is \code{bgcol="gray"}.
#' @param bgborder Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param posx1 Numeric argument.Distance of the labels (horizontal) in relation to the x axis.
#' @param posx2 Numeric argument.Distance of the labels (vertical) in relation to the x axis.
#' @param xang  Numeric argument.Angle of the labels in relation to the x axis
#' @param labels Character argument. Labels name vector.
#' @param lcol Line color. The default is \code{lcol = "black"}.
#' @param pcol Point color. The default is \code{pcol = lcol}.
#' @param pty Point type. The default is  \code{pty = 19}.
#' @param pwd Point width. The default is  \code{pwd = 3}.
#' @param lty Line type. The default is  \code{lty = 2}.
#' @param lwd Line width. The default is  \code{lwd = 2}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The result of \code{stickchart()} is \code{x} object.
#' @examples
#' library(leem)
#' # Example 1
#' rbinom(30, 10, 0.4) |>
#'   new_leem() |>
#'   tabfreq() |>
#'   stickchart()
#' # Example 2
#' school <- rep(c("high", "university", "basic"), 3:5)
#' sample(school, 30, TRUE) |>
#'   new_leem() |>
#'   tabfreq(ordered = c("basic", "high", "university")) |>
#'   stickchart(xang = 15, posx2 = -0.5)
#' @usage
#' stickchartunction(x, ...)
#' @export
stickchart <- function(x,
                      freq = "a",
                      bg = TRUE,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      grids = grid(col = "white"),
                      bgcol = "gray",
                      bgborder = NA,
                      posx1 = 0,
                      posx2 = 0,
                      xang = 0,
                      labels = NULL,
                      lcol = "black",
                      pcol = lcol,
                      pty = 19,
                      pwd = 3,
                      lty = 1,
                      lwd = 2,
                      ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE, domain = "R-leem")
  if (attr(x, "variable") == "continuous") stop("The function only applies to discrete variables.", call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$table$Groups)
    if (numchar) {
      xmin <- x$table$Groups[1]
      xmax <- max(x$table$Groups)
      xvar <- x$table$Groups
      if (freq == "a") yvar <- c(0, x$table$Fi, 0)
      if (freq == "r") yvar <- c(0, x$table$Fr, 0)
      if (freq == "p") yvar <- c(0, x$table$Fp, 0)

      # Limiares
      xlim <- c(xmin - 0.5, xmax + 0.5)
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Stick chart", domain = "R-leem")
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

      # Eixos
      axis(1, at = xvar)
      axis(2)

      # Grid
      grids

      # Inserindo hastes
      lines(1:length(x$table$Groups), x$table$Fi, type = "h",
            lty = lty, lwd = lwd, col = lcol)
      points(1:length(x$table$Groups), x$table$Fi, pch  = pty, lwd = pwd,
             col = pcol)
    } else {
      ngroups <- length(x$table$Groups)
      aux <- 1:ngroups
      xvar <- x$table$Groups
      xvaraux <-  c(0, aux, ngroups + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      if (freq == "a") yvar <- c(0, x$table$Fi, 0)
      if (freq == "r") yvar <- c(0, x$table$Fr, 0)
      if (freq == "p") yvar <- c(0, x$table$Fp, 0)
      yvar1 <- x$table$Fac1
      yvar2 <- x$table$Fac2


      # Limiares
      xlim <- c(min(xvaraux), max(xvaraux))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Stick chart", domain = "R-leem")
      }
      if (is.null(xlab)) {
        xlab <- gettext("Groups", domain = "R-leem")
      }
      if (is.null(ylab)) {
        ylab <- gettext("Frequency", domain = "R-leem")
      }

      title(main = main, xlab = xlab, ylab = ylab)

      if(bg) {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)
      }

      # Eixos
      axis(1, at = aux, labels = FALSE)
      if (is.null(labels)) labels <- xvar
      text(x = aux + posx1,  y = par("usr")[3] + posx2, labels = labels, srt = xang, pos = 1, xpd = TRUE)
      axis(2)

      # Grid
      grids

      # Inserindo hastes
      lines(1:length(x$table$Groups), x$table$Fi, type = "h",
            lty = lty, lwd = lwd, col = lcol)
      points(1:length(x$table$Groups), x$table$Fi, pch  = pty, lwd = pwd,
             col = pcol)

    }
  }
  invisible(x)
}


#' Ogives Graph
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
#' @examples
#' library(leem)
#' # Example 1 - Both ogives
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE)
#'
#' # Example 2 - Insert barplot
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE, bars = TRUE)
#' # Example 3 - Insert histogram
#' rnorm(36, 100, 50) |> new_leem(variable = 2) |> tabfreq() |> ogive(both = TRUE, hist = TRUE)

#' @usage
#' ogive(x, ...)
#'
#' ## Leem S3 method:
#' ogive(x, decreasing = FALSE, both = FALSE, bars = FALSE,
#'       histogram = FALSE, bg = TRUE, main = NULL, xlab = NULL, ylab = NULL,
#'       grids = grid(col = "white"), bgcol = "gray", bgborder = NA,
#'       barcol = "yellow", histcol = barcol, barborder = "gray",
#'       histborder = barborder, type = "b", lpcol = "black", lwd = 2,
#'       pch = 19, lty = 2)
#'
#' @export
ogive <- function(x, ...) {
  UseMethod("ogive")
}
# S3Method ogive of leem class
#' @export
ogive.leem <- function(x, freq = "a", decreasing = FALSE, both = FALSE,
                       bars = FALSE,
                       histogram = FALSE,
                       bg = TRUE,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grids = grid(col = "white"),
                       bgcol = "gray",
                       bgborder = NA,
                       barcol = "yellow",
                       histcol = barcol,
                       barborder = "gray",
                       histborder = barborder,
                       type = "b",
                       lpcol = "black",
                       lwd = 2,
                       pch = 19,
                       lty = 2,
...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
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



#' Frequency polygon Graph
#'
#' Generic function that plots the frequency polygon curve.
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param type Type of plot. The default is \code{type = "b"}, i.e., line and points. See \code{\link{graphical parameter}} for details.
#' @param bars Logical argument. Default is \code{FALSE}. If \code{bars = TRUE}, the histogram will be inserted to plot.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Insert the plot title.  The default is \code{NULL}.
#' @param xlab Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgcol Insert the background color. This argument is only valid when \code{bg = TRUE}. The default is \code{bgcol="gray"}.
#' @param bgborder Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param barcol Insert the barplot color. The default is \code{barcol = "yellow"}. This argument is only valid when \code{bars = TRUE}.
#' @param histcol Insert the histogram color. The default is \code{histcol = barcol}. This argument is only valid when \code{histogram = TRUE}.
#' @param barborder Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
#' @param histborder Insert the histogram border color. This argument is only valid when \code{histogram = TRUE}. The default is histborder = barborder.
#' @param lpcol Type of line color. The default is \code{lpcol = "black"}.
#' @param lwd numeric argument. The line width. The default is  \code{lwd = 2}.
#' @param pch Type of point. The default is  \code{pch = 19}.
#' @param lty Type of line. The default is  \code{lty = 2}.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' # Example 1
#' library(leem)
#' rnorm(36, 100, 50) |> new_leem(variable = "continuous") |> tabfreq() |> polyfreq()
#'
#' @usage
#' polyfreq(x, ...)
#'
#' ## Leem S3 method:
#' polyfreq.leem(x, type = "b", bars = TRUE, bg = TRUE, main = NULL,
#'                     xlab = NULL, ylab = NULL, grids = grid(col = "white"),
#'                     bgcol = "gray", bgborder = NA, barcol = "yellow",
#'                     barborder = "gray", lpcol = "black", lwd = 2, pch = 19,
#'                     lty = 2)
#'
#' @export
polyfreq <- function(x, ...) {
  UseMethod("polyfreq")
}
#' @export
polyfreq.leem <- function(x,
                          freq = "a",
                          type = "b",
                          bars = TRUE,
                          bg = TRUE,
                          main = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          grids = grid(col = "white"),
                          bgcol = "gray",
                          bgborder = NA,
                          barcol = "yellow",
                          barborder = "gray",
                          lpcol = "black",
                          lwd = 2,
                          pch = 19,
                          lty = 2, # Type of line
                          ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if (attr(x, "variable") == "continuous") {
    xvar1 <- c(min(x$statistics$lower_lim) - x$statistics$len_class_interval, x$statistics$lower_lim,
               max(x$statistics$lower_lim) + x$statistics$len_class_interval)
    xvar2 <- c(min(x$statistics$upper_lim) - x$statistics$len_class_interval, x$statistics$upper_lim,
               max(x$statistics$upper_lim) + x$statistics$len_class_interval)
    if (freq == "a") yvar <- c(0, x$table$Fi, 0)
    if (freq == "r") yvar <- c(0, x$table$Fr, 0)
    if (freq == "p") yvar <- c(0, x$table$Fp, 0)

    pm <- c(min(x$statistics$lower_lim) - x$statistics$len_class_interval / 2,
            x$table$PM,
            max(x$statistics$upper_lim) + x$statistics$len_class_interval / 2)
    # Limiares
    xlim <- c(min(xvar1), max(xvar2))
    ylim <- c(0, 1.2 * max(yvar))

    # Area de plotagem
    plot.new()
    plot.window(xlim, ylim)

    # Labels
    if (is.null(main)) {
      main <- gettext("Polygon", domain = "R-leem")
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

    # Eixos
    xvar <- c(xvar1, max(xvar2))
    axis(1, at = xvar)
    axis(2)

    # Grid
    grids

    # Inserindo barras
    if (bars) {
      rect(xvar1,
           0,
           xvar2,
           yvar, col = barcol, border = barborder)
    }
    # Pontos
    points(pm, yvar, col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)
  }
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$table$Groups)
    if (numchar) {
      xmin <- x$table$Groups[1]
      xmax <- max(x$table$Groups)
      xvar <- x$table$Groups
      xvaraux <-  c(xmin - 1, x$table$Groups, xmax + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      if (freq == "a") {
        yvar <- x$table$Fi
        yvar1 <- x$table$Fac1
        yvar2 <- x$table$Fac2
      }
      if (freq == "p") {
        yvar <- x$table$Fp
        yvar1 <- x$table$Fac1p
        yvar2 <- x$table$Fac2p
      }
      if (freq == "r") {
        yvar <- x$table$Fr
        yvar1 <- x$table$Fac1p/100
        yvar2 <- x$table$Fac2p/100
      }


      # Limiares
      xlim <- c(min(xvaraux) - 1, max(xvaraux) + 1)
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Polygon", domain = "R-leem")
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

      # Eixos
      axis(1, at = xvaraux)
      axis(2)

      # Grid
      grids

      # Inserindo barras
      if (bars) {
        rect(xvar - 0.5,
             0,
             xvar + 0.5,
             yvar, col = barcol, border = barborder)
      }
      # Pontos
      points(xvaraux, c(0, yvar, 0), col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)
      # Alert
      cat(note(gettext("\n Note: The polygon graph has interpretation problems for the discrete quantitative variable", domain = "R-leem")))
    } else {
      stop("The polygon graph is not used for this data type!", domain = "R-leem")
    }
  }
  invisible(x)
}


#' Histogram graph
#'
#' Class method leem for generic hist
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Insert the plot title.  The default is \code{NULL}.
#' @param xlab Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgcol Insert the background color. This argument is only valid when \code{bg = TRUE}. The default is \code{bgcol="gray"}.
#' @param bgborder Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param barcol Insert the barplot color. The default is \code{barcol = "yellow"}. This argument is only valid when \code{bars = TRUE}.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' # Example 1
#' library(leem)
#' rnorm(36, 100, 50) |> new_leem(variable = "continuous") |> tabfreq() |> hist()
#'
#' # Example 2
#' library(leem)
#' school <- rep(c("high", "university", "basic"), 3:5)
#' sample(school, 30, TRUE) |>
#'  new_leem() |>
#'  tabfreq(ordered = c("basic", "high", "university"))
#'
#' @usage
#'
#' hist(x, ...)
#'
#' ## Leem S3 method:
#' hist.leem(x, bg = TRUE, main = NULL, xlab = NULL, ylab = NULL,
#'           grids = grid(col = "white"), bgcol = "gray", bgborder = NA,
#'           barcol = "yellow", barborder = "gray", ...)
#'
#' @export
hist.leem <- function(x,
                      freq = "a",
                      bg = TRUE,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      grids = grid(col = "white"),
                      bgcol = "gray",
                      bgborder = NA,
                      barcol = "yellow",
                      barborder = "gray",
                      ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if (attr(x, "variable") == "discrete") {
    warning("Coerced to barplot!", call. = FALSE, domain = "R-leem")
    barplot(x, freq, bg, main, xlab, xlab, grids, bgcol, bgborder,
            barcol, barborder, ...)
  }
  if (attr(x, "variable") == "continuous") {
    xvar1 <- x$statistics$lower_lim
    xvar2 <- x$statistics$upper_lim
    if (freq == "a") yvar <- x$table$Fi
    if (freq == "r") yvar <- x$table$Fr
    if (freq == "p") yvar <- x$table$Fp
    # Limiares
    xlim <- c(min(xvar1), max(xvar2))
    ylim <- c(0, 1.2 * max(yvar))

    # Area de plotagem
    plot.new()
    plot.window(xlim, ylim)

    # Labels
    if (is.null(main)) {
      main <- gettext("Histogram", domain = "R-leem")
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
    rect(xvar1,
         0,
         xvar2,
         yvar, col = barcol, border = barborder)

    # Eixos
    xvar <- c(xvar1, max(xvar2))
    axis(1, at = xvar)
    axis(2)
  }
  invisible(x)
}



#' Barplot graph
#'
#' Class method leem for generic barplot
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Character argument. Insert the plot title.  The default is \code{NULL}.
#' @param xlab Character argument. Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Character argument. Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgborder Character argument. Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param barcol Character argument. Insert the barplot color. The default is \code{barcol = "yellow"}. This argument is only valid when \code{bars = TRUE}.
#' @param barborder Numeric argument. Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
#' @param posx1 Numeric argument.Distance of the labels (horizontal) in relation to the x axis.
#' @param posx2 Numeric argument.Distance of the labels (vertical) in relation to the x axis.
#' @param xang  Numeric argument.Angle of the labels in relation to the x axis
#' @param labels Character argument. Labels name vector.
#' @param ... further arguments passed to or from other methods.
#'
#' @usage
#' barplot(x, ...)
#'
#' ## Leem S3 method:
#' barplot.leem(x, bg = TRUE, main = NULL, xlab = NULL, ylab = NULL,
#'   grids = grid(col = "white"), bgcol = "gray", bgborder = NA,
#'   barcol = "yellow", barborder = "gray", posx1 = 0, posx2 = 0,
#'   xang = 0, labels = NULL, ...)
#'
#' @examples
#' # Example 1 - Simple example
#' library(leem)
#' rep(1:5, 5:1) |>
#'   new_leem() |>
#'   barplot()
#' # Example 2 - Color bars
#' rep(1:5, 5:1) |>
#'   new_leem() |>
#'   barplot(barcol = heat.colors(5))
#' # Example 3 - Ordered data
#' library(leem)
#' school <- rep(c("high", "university", "basic"), 3:5)
#' sample(school, 30, TRUE) |>
#'  new_leem() |>
#'  tabfreq(ordered = c("basic", "high", "university")) |>
#'  barplot(xang = 15, posx2 = -0.2)
#' # Example 4 - Coerced to histogram
#' rnorm(100, 10, 2) |>
#'   new_leem(variable = 2) |>
#'   barplot(barcol = heat.colors(10))
#' @export
barplot.leem <- function(x,
                         freq = "a",
                         bg = TRUE,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         grids = grid(col = "white"),
                         bgcol = "gray",
                         bgborder = NA,
                         barcol = "yellow",
                         barborder = "gray",
                         posx1 = 0,
                         posx2 = 0,
                         xang = 0,
                         labels = NULL,
                         ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x)
  if (attr(x, "variable") == "continuous") {
    warning("Coerced to histogram!", call. = FALSE, domain = "R-leem")
    hist(x, freq, bg, main, xlab, ylab, grids,
         bgcol, bgborder, barcol, barborder, ...)
  }
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$table$Groups)
    if (numchar) {
      xmin <- x$table$Groups[1]
      xmax <- max(x$table$Groups)
      xvar <- x$table$Groups
      xvaraux <-  c(xmin - 1, x$table$Groups, xmax + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      if (freq == "a") {
        yvar <- x$table$Fi
        yvar1 <- x$table$Fac1
        yvar2 <- x$table$Fac2
      }
      if (freq == "p") {
        yvar <- x$table$Fp
        yvar1 <- x$table$Fac1p
        yvar2 <- x$table$Fac2p
      }
      if (freq == "r") {
        yvar <- x$table$Fr
        yvar1 <- x$table$Fac1p/100
        yvar2 <- x$table$Fac2p/100
      }


      # Limiares
      xlim <- c(min(xvaraux), max(xvaraux))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Bar plot", domain = "R-leem")
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

      # Eixos
      axis(1, at = xvar)
      axis(2)

      # Grid
      grids

      # Inserindo barras
      rect(xvar - 0.5,
           0,
           xvar + 0.5,
           yvar, col = barcol, border = barborder)
    } else {
      ngroups <- length(x$table$Groups)
      aux <- 1:ngroups
      xvar <- x$table$Groups
      xvaraux <-  c(0, aux, ngroups + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      if (freq == "a") {
        yvar <- x$table$Fi
        yvar1 <- x$table$Fac1
        yvar2 <- x$table$Fac2
      }
      if (freq == "p") {
        yvar <- x$table$Fp
        yvar1 <- x$table$Fac1p
        yvar2 <- x$table$Fac2p
      }
      if (freq == "r") {
        yvar <- x$table$Fr
        yvar1 <- x$table$Fac1p/100
        yvar2 <- x$table$Fac2p/100
      }


      # Limiares
      xlim <- c(min(xvaraux), max(xvaraux))
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Bar plot", domain = "R-leem")
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

      # Eixos
      axis(1, at = aux, labels = FALSE)
      if (is.null(labels)) labels <- xvar
      text(x = aux + posx1,  y = par("usr")[3] + posx2, labels = labels, srt = xang, pos = 1, xpd = TRUE)
      axis(2)

      # Grid
      grids

      # Inserindo barras
      rect(aux - 0.5,
           0,
           aux + 0.5,
           yvar, col = barcol, border = barborder)
    }
  }
  invisible(x)
}
