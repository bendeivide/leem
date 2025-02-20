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
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!")
  if (!is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
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
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE, domain = "R-leem")
  if (attr(x, "variable") == "continuous") stop("The function only applies to discrete variables.", call. = FALSE, domain = "R-leem")
  if (!is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$table$Groups)
    if (numchar) {
      xmin <- x$table$Groups[1]
      xmax <- max(x$table$Groups)
      xvar <- x$table$Groups
      if (freq == "a") yvar <- x$table$Fi
      if (freq == "r") yvar <- x$table$Fr
      if (freq == "p") yvar <- x$table$Fp

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
      if (freq == "a") {
        lines(x$table$Groups, x$table$Fi, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(x$table$Groups, x$table$Fi, pch  = pty, lwd = pwd,
               col = pcol)
      }
      if (freq == "r") {
        lines(x$table$Groups, x$table$Fr, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(x$table$Groups, x$table$Fr, pch  = pty, lwd = pwd,
               col = pcol)
      }
      if (freq == "p") {
        lines(x$table$Groups, x$table$Fp, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(x$table$Groups, x$table$Fp, pch  = pty, lwd = pwd,
               col = pcol)
      }

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
      # Inserindo hastes
      if (freq == "a") {
        lines(1:length(x$table$Groups), x$table$Fi, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(1:length(x$table$Groups), x$table$Fi, pch  = pty, lwd = pwd,
               col = pcol)
      }
      if (freq == "r") {
        lines(1:length(x$table$Groups), x$table$Fr, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(1:length(x$table$Groups), x$table$Fr, pch  = pty, lwd = pwd,
               col = pcol)
      }
      if (freq == "p") {
        lines(1:length(x$table$Groups), x$table$Fp, type = "h",
              lty = lty, lwd = lwd, col = lcol)
        points(1:length(x$table$Groups), x$table$Fp, pch  = pty, lwd = pwd,
               col = pcol)
      }
    }
  } else {
    stop("Chart not used for this data type", call. = FALSE, domain = "R-leem")
  }
  invisible(x)
}


#' Frequency polygon Graph
#'
#' Generic function that plots the frequency polygon curve.
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' # Example 1
#' library(leem)
#' rnorm(36, 100, 50) |> new_leem(variable = "continuous") |> tabfreq() |> polyfreq()
polyfreq <- function(x, ...) {
  UseMethod("polyfreq")
}

#' Frequency polygon Graph
#'
#' Plot the frequency polygon curve.
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
#' @param barborder Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
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
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!")
  if (!is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
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
#' @param barborder Numeric argument. Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
#' @param ... further arguments passed to or from other methods.
#' @export
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
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!")
  if (!is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
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
#' @param height R object (list) of class leem. Use \code{new_leem()} function.
#' @param freq Character argument. Type of frequency with options: \code{"a"} (absolute and default), \code{"r"} relative and \code{"p"} percentage.
#' @param bg Logical argument. Default is \code{TRUE}, it displays the background, and \code{bg = FALSE} otherwise.
#' @param main Character argument. Insert the plot title.  The default is \code{NULL}.
#' @param xlab Character argument. Insert the title of the x-axis graphic label. The default is \code{NULL}.
#' @param ylab Character argument. Insert the title of the y-axis graphic label. The default is \code{NULL}.
#' @param grids Insert grids to plot. The default is \code{grid(col = "white")}.
#' @param bgcol Insert the background color. This argument is only valid when \code{bg = TRUE}. The default is \code{bgcol="gray"}.
#' @param bgborder Character argument. Insert the background border color. This argument is only valid when \code{bg = TRUE}. The default is bgborder = NA.
#' @param barcol Character argument. Insert the barplot color. The default is \code{barcol = "yellow"}. This argument is only valid when \code{bars = TRUE}.
#' @param barborder Numeric argument. Insert the barplot border color. This argument is only valid when \code{bars = TRUE}. The default is barborder = "gray".
#' @param posx1 Numeric argument.Distance of the labels (horizontal) in relation to the x axis.
#' @param posx2 Numeric argument.Distance of the labels (vertical) in relation to the x axis.
#' @param xang  Numeric argument.Angle of the labels in relation to the x axis
#' @param labels Character argument. Labels name vector.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' library(graphics)
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
#' @importFrom graphics barplot
barplot.leem <- function(height,
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
  x <- height
  if (!is(x, "leem")) stop("Use the 'new_leem()' function to create an object of class leem!")
  if (is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x)
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
