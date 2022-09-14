# Histograma
#' @export
hist.leem <- function(x,
                      bg = TRUE,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      panel.first = grid(col = "white"),
                      bgcol = "gray",
                      bgborder = NA,
                      barcol = "yellow",
                      barborder = "gray",
                      ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    warning("Coerced to Histogram!", call. = FALSE, domain = "R-leem")
    numchar <- is.numeric(x$tabela$Groups)
    if (numchar) {
      xmin <- x$tabela$Groups[1]
      xmax <- max(x$tabela$Groups)
      xvar <- x$tabela$Groups
      xvaraux <-  c(xmin - 1, x$tabela$Groups, xmax + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      yvar <- x$tabela$Fi
      yvar1 <- x$tabela$Fac1
      yvar2 <- x$tabela$Fac2


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
        ylab <- gettext("Frequency", domain = "R-leem")
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
      panel.first

      # Inserindo barras
        rect(xvar - 0.5,
             0,
             xvar + 0.5,
             yvar, col = barcol, border = barborder)
    } else {
      stop("Em desenvolvimento!")
    }
  }
  if (attr(x, "variable") == "continuous") {
    xvar1 <- x$estat$LI_classes
    xvar2 <- x$estat$LS_classes
    yvar <- x$tabela$Fi
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
      ylab <- gettext("Frequency", domain = "R-leem")
    }

    title(main = main, xlab = xlab, ylab = ylab)

    if(bg) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
             bgcol, border = bgborder)
    }

    # Grid
    panel.first

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

# Grafico de hastes ou bastao
#' @export
stickplot <- function(x,
                      bg = TRUE,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      panel.first = grid(col = "white"),
                      bgcol = "gray",
                      bgborder = NA,
                      lcol = "black",
                      pcol = lcol,
                      pty = 19,
                      pwd = 3,
                      lty = 1,
                      lwd = 2,
                      ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (attr(x, "variable") == "continuous") stop("The function only applies to discrete variables.", call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$tabela$Groups)
    if (numchar) {
      xmin <- x$tabela$Groups[1]
      xmax <- max(x$tabela$Groups)
      xvar <- x$tabela$Groups
      yvar <- x$tabela$Fi



      # Limiares
      xlim <- c(xmin - 0.5, xmax + 0.5)
      ylim <- c(0, 1.2 * max(yvar))

      # Area de plotagem
      plot.new()
      plot.window(xlim, ylim)

      # Labels
      if (is.null(main)) {
        main <- gettext("Stick plot", domain = "R-leem")
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
      axis(1, at = xvar)
      axis(2)

      # Grid
      panel.first

      # Inserindo hastes
      lines(x$tabela$Groups, x$tabela$Fi, type = "h",
            lty = lty, lwd = lwd, col = lcol)
      points(x$tabela$Groups, x$tabela$Fi, pch  = pty, lwd = pwd,
             col = pcol)
    } else {
      stop("Em desenvolvimento!")
    }
  }
  invisible(x)
}


# Grafico de barras
# rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
#   plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n")
#   text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# }
# rotate_x(mtcars, 'mpg', row.names(mtcars), 45)
#' @export
barplot.leem <- function(x,
                         bg = TRUE,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         panel.first = grid(col = "white"),
                         bgcol = "gray",
                         bgborder = NA,
                         barcol = "yellow",
                         barborder = "gray",
                         posx1 = 0,
                         posx2 = 0,
                         xangf = 0,
                         labels = NULL,
                         ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (attr(x, "variable") == "continuous") stop("The function only applies to discrete variables.", call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$tabela$Groups)
    if (numchar) {
      xmin <- x$tabela$Groups[1]
      xmax <- max(x$tabela$Groups)
      xvar <- x$tabela$Groups
      xvaraux <-  c(xmin - 1, x$tabela$Groups, xmax + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      yvar <- x$tabela$Fi
      yvar1 <- x$tabela$Fac1
      yvar2 <- x$tabela$Fac2


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
        ylab <- gettext("Frequency", domain = "R-leem")
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
      panel.first

      # Inserindo barras
      rect(xvar - 0.5,
           0,
           xvar + 0.5,
           yvar, col = barcol, border = barborder)
    } else {
      ngroups <- length(x$tabela$Groups)
      aux <- 1:ngroups
      xvar <- x$tabela$Groups
      xvaraux <-  c(0, aux, ngroups + 1)
      xvar1 <- xvaraux - 0.5
      xvar2 <- xvaraux + 0.5
      yvar <- x$tabela$Fi
      yvar1 <- x$tabela$Fac1
      yvar2 <- x$tabela$Fac2


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
      text(x = aux + posx1,  y = par("usr")[3] + posx2, labels = labels, srt = xangf, pos = 1, xpd = TRUE)
      axis(2)

      # Grid
      panel.first

      # Inserindo barras
      rect(aux - 0.5,
           0,
           aux + 0.5,
           yvar, col = barcol, border = barborder)
    }
  }
  invisible(x)
}

# Impressao da classe 'leem'
#' @export
print.leem <- function(x, ...) {
  if (!is.null(attr(x, "table"))) {
    print(x$table)
  } else {
    attributes(x) <- NULL
    print(x)
  }
}
# print.leem <- function(x, ...) {
#   aux <- attr(x, "output")
#   # Print
#   switch(aux,
#          htest = output_htest(x))
# }

output_htest <- function(x) {
  if (x$test == "ztest") {
    cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
    # Step 1
    cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
        crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
    cat(crayon::bold(x$nullhyp), "\n")
    cat(crayon::bold(x$althyp), "\n\n")
    # Step 2
    cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
        crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
    cat(crayon::bold(x$signlevel), "\n\n")
    # Step 3
    cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
        crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
    cat(crayon::green$bold(gettext("   If |ztest| > |ztab| => Reject H0!", domain = "R-leem")), "\n")
    cat(crayon::green(gettext("   ztest - test statistic; ztab - critical point", domain = "R-leem")), "\n")
    cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
    cat(crayon::bold(x$decision), "\n")
    cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
    cat(crayon::bold(x$decision2), "\n\n")

    # Step 4
    cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
        crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
    cat(crayon::bold(x$conclusion))
  }
}




# Inserindo medidas nos graficos

#' @export
insert <- function(x, ...) {
  UseMethod("insert")
}

#' @export
insert.leem <- function(x, type = "mean",
                        lty = 1,
                        lcol = "black",
                        tcol = lcol,
                        acol = lcol,
                        parrow = 0.5,
                        larrow = 0.2,
                        ptext = 0.6,
                        side = "right",
                        lwd = 2,
                        lwdarrow = lwd,
                        ...) {
  # https://vanderleidebastiani.github.io/tutoriais/Graficos_com_R.html#Adicionando_segmentos
  if (type == "mean") {
    abline(v = mean(x),
           lty = lty, lwd = lwd, col = lcol)
    # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
    if (side == "right") {
      x0 <- mean(x) + mean(x) * larrow
      y0 <- max(x$tabela$Fi) * parrow
      arrows(x0 = x0, y0 = y0,
             x1 = mean(x), y1 = max(x$tabela$Fi) * parrow,
             length = 0.1, col = acol, lwd = lwdarrow)

      text(x = mean(x) + 1.1 * mean(x) * larrow, y = ptext + y0,
           labels = gettext("Mean", domain = "R-leem"), col = tcol)

      # ?plotmath
      text(x = mean(x) + 1.1 * mean(x) * larrow, y = y0 - ptext,
           labels = bquote(bar(x) ==.(format(mean(x, rouding = 4), digits = 4))), col = tcol)
    }
    if (side == "left") {
      x0 <- mean(x) - mean(x) * larrow
      y0 <- max(x$tabela$Fi) * parrow
      arrows(x0 = x0, y0 = y0,
             x1 = mean(x), y1 = y0,
             length = 0.1, col = acol, lwd = lwdarrow)

      text(x = mean(x) - 1.1 * mean(x) * larrow, y = ptext + y0,
           labels = gettext("Mean", domain = "R-leem"), col = tcol)

      # ?plotmath
      text(x = mean(x) - 1.1 * mean(x) * larrow, y = y0 - ptext,
           labels = bquote(bar(x) ==.(format(mean(x, rouding = 4), digits = 4))), col = tcol)
    }
  }
  if (type == "median") {
    abline(v = median(x),
           lty = lty, lwd = lwd, col = lcol)
    # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
    if (side == "right") {
      x0 <- median(x) + median(x) * larrow
      y0 <- max(x$tabela$Fi) * parrow
      arrows(x0 = x0, y0 = y0,
             x1 = median(x), y1 = max(x$tabela$Fi) * parrow,
             length = 0.1, col = acol, lwd = lwdarrow)

      text(x = median(x) + 1.1 * median(x) * larrow, y =  ptext + y0,
           labels = gettext("Median", domain = "R-leem"), col = tcol)

      # ?plotmath
      text(x = median(x) + 1.1 * median(x) * larrow, y = y0 - ptext,
           labels = bquote(bar(x) ==.(format(median(x, rouding = 4), digits = 4))), col = tcol)
    }
    if (side == "left") {
      x0 <- median(x) - median(x) * larrow
      y0 <- max(x$tabela$Fi) * parrow
      arrows(x0 = x0, y0 = max(x$tabela$Fi) * parrow,
             x1 = median(x), y1 = max(x$tabela$Fi) * parrow,
             length = 0.1, col = acol, lwd = lwdarrow)

      text(x = median(x) - 1.1 * median(x) * larrow, y = ptext + y0,
           labels = gettext("Median", domain = "R-leem"), col = tcol)

      # ?plotmath
      text(x = median(x) - 1.1 * median(x) * larrow, y = y0 - ptext,
           labels = bquote(bar(x) ==.(format(median(x, rouding = 4), digits = 4))), col = tcol)
    }
  }
  invisible(x)
}
