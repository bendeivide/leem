## Grafico de ogiva

ogive <- function(x, decreasing = FALSE, bars = TRUE,
                  bg = TRUE,
                  main = NULL,
                  xlab = NULL,
                  ylab = NULL,
                  panel.first = grid(),
                  gridcol = "lightgray",
                  bgcol = "gray",
                  bgborder = NA,
                  barcol = "yellow",
                  barborder = "gray",
                  type = "b",
                  lpcol = "black",
                  lwd = 2,
                  pch = 19,
                  lty = 2,
                  ...
) {
  if (!is.logical(decreasing)) stop("The decreasing argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (!decreasing) {
    xvar <- c(x$estat$LI_classes[1], x$estat$LS_classes)
    yvar <- c(0, x$tabela$Fac1)

    # Limiares
    xlim <- c(min(xvar), max(xvar))
    ylim <- c(0, 1.2 * max(yvar))

    # Area de plotagem
    plot.new()
    plot.window(xlim, ylim)

    # Labels
    if (is.null(main)) {
      main <- "Ogive larger than"
    }
    if (is.null(xlab)) {
      xlab <- "Classes"
    }
    if (is.null(ylab)) {
      ylab <- "Frequency"
    }

    title(main = main, xlab = xlab, ylab = ylab)

    if(bg) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
             bgcol, border = bgborder)
    }

    # Grid
    panel.first

    # Inserindo barras
    if (bars) {
      rect(x$estat$LI_classes,
           0,
           x$estat$LS_classes,
           x$tabela$Fac1, col = barcol, border = barborder)
    }

    # Pontos
    points(xvar, yvar, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

    # Eixos
    axis(1, at = xvar)
    axis(2)
  } else{
    xvar <- c(x$estat$LI_classes, max(x$estat$LS_classes))
    yvar <- c(x$tabela$Fac2, 0)

    # Limiares
    xlim <- c(min(xvar), max(xvar))
    ylim <- c(0, 1.2 * max(yvar))

    # Area de plotagem
    plot.new()
    plot.window(xlim, ylim)

    # Labels
    if (is.null(main)) {
      main <- "Ogive less than"
    }
    if (is.null(xlab)) {
      xlab <- "Classes"
    }
    if (is.null(ylab)) {
      ylab <- "Frequency"
    }

    title(main = main, xlab = xlab, ylab = ylab)

    # Retangulo do grafico (lembrar de corrigir com on.exit())
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
           bgcol, border = bgborder)

    # Grid
    panel.first


    # Inserindo barras
    if (bars) {
      rect(x$estat$LI_classes,
           0,
           x$estat$LS_classes,
           x$tabela$Fac2, col = barcol, border = barborder)
    }

    # Pontos
    points(xvar, yvar, col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)

    # Eixos
    axis(1, at = xvar)
    axis(2)
  }
}
