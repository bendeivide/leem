# Construtor
#' @export
new_leem <- function(x = vector(), variable = "discrete") {
  stopifnot("The x argument should be vector!" = is.vector(x))
  if (variable == 1) variable <- "discrete"
  if (variable == 2) variable <- "continuous"
  if(!any(variable == c("discrete", "continuous"))) stop("The variable argument must be 'discrete' or 'continuous'.")
  structure(x, class = "leem", variable = variable)
}

# Generico
#' @export
tabfreq <- function(dados, ...) {
  UseMethod("tabfreq")
}

# Tabela em distribuicao de frequencias
#' @export
tabfreq.leem <- function(dados, k = NULL, ordered = NULL){
  if (class(dados) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (attr(dados, "variable") == "discrete") {
    # numeric ou character
    numchar <- is.numeric(dados)
    # tamanho da amostra
    n <- length(dados)
    # Dist freq
    aux <- table(dados)
    if (numchar) {
      groups <- as.numeric(names(aux))
    } else{
      if (is.null(ordered)) {
        groups <- as.character(names(aux))
      } else {
        pos <- match(as.character(names(aux)), ordered)
        aux <- aux[pos]
        groups <- as.character(names(aux))
      }

    }

    names(aux) <- NULL
    if (numchar) {
      #Minimum and maximum values
      x1 <- min(groups)
      xn <- max(groups)
    }
    # Frequencia absoluta
    fi <- as.numeric(aux)
    # Frequencia relativa
    fr <- round(fi/n, 2)
    # Frequencia acumulada (abaixo de)
    fac1 <- cumsum(fi)
    # Frequencia acumulada (acima de)
    # x: representa as frequencias absolutas
    fac2 <- function(x) {
      f1 <- sum(x)
      n <- length(x)
      vet <- c(f1, rep(0,n - 1))
      for (j in 2:n) {
        vet[j] <- vet[j - 1] - x[j - 1]
      }
      return(vet)
    }
    fac22 <- fac2(fi)
    # Frequencia percentual
    fp <- round(fr*100, 2)
    # Fac (abaixo de) percentual
    fac1p <- round((fac1/n)*100, 2)
    # Fac (acima de) percentual
    fac22p <- round((fac22/n)*100, 2)
    # Estatisticas
    dados2 <- dados
    attributes(dados2) <- NULL
    if (numchar) {
      estat <- list(
        "Numero_de_grupos" = length(groups),
        "Valor_minimo" = x1,
        "Valor_maximo" = xn,
        "raw_data" = dados2
      )
    }
    # Tabela de frequencias
    tabela <- list(Groups = groups, Fi = fi, Fr = fr, Fac1 = fac1, Fac2 = fac22,
                         Fp = fp, Fac1p = fac1p, Fac2p = fac22p)
    tabela <- data.frame(tabela)

    if (numchar) {
      listres <- list(tabela = tabela, estat = estat)
    } else {
      listres <- list(tabela = tabela)
    }

    attr(listres, "variable") <- attr(dados, "variable")
    attr(listres, "table") <- "tabfreq"
    if (!is.null(ordered)) attr(listres, "levels") <- "ordered"
    class(listres) <- "leem"
    return(listres)
  }
  if (attr(dados, "variable") == "continuous") {
  # Dados em Rol (Elaborados)
  sort(dados)
  # tamanho da amostra
  n <- length(dados)
  if (is.null(k)) {
    # Numero de classes
    if (n <= 100) {
      k <- round(sqrt(n))
      # OBS.: O valor de k nao necessariamente precisa ser
      #       sqrt(n). Esse eh um valor base
    }
    if (n > 100) {
      k <- 5 * log10(n)
    }
  } else {
    if(!is.numeric(k)) stop("The argument should be numerical!", call. = FALSE,
                            domain = "R-leem")
  }
  # Number of classes
  k <- round(k)
  # Amplitude
  At <- diff(range(dados))
  # Minimum value
  x1 <- min(dados)
  # Amplitude of class
  c <- round(At/(k - 1), 2)
  # Lower limit of the first class
  LI1 <- x1 - c/2; LI1
  vi <- c(LI1, rep(0, k - 1))
  vs <- c(LI1 + c, rep(0, k - 1))
  #  Lower and upper limits
  for (i in 2:k) {
    vi[i] <- vi[i - 1] + c
    vs[i] <- vs[i - 1] + c
  }
  vi <- round(vi, 2)
  vs <- round(vs, 2)
  # Frequency
  freq <- function(x, vi, vs, k) {
    freq <- rep(0, k)
    for (i in 1:(k - 1)) {
      freq[i] <- length(x[x >= vi[i] & x < vs[i]])
    }
    freq[k] <- length(x[x >= vi[k] & x <= vs[k]])
    return(freq)
  }
  # Frequencia absoluta
  fi <- freq(dados, vi, vs, k)
  # Construindo as classes
  classe <- paste(round(vi, 2), "|--- ", round(vs, 2))
  classe[k] <- paste(round(vi[k], 2), "|--- ", round(vs[k], 2))
  classe
  # Ponto medio
  pm <- (vi + vs)/2
  # Frequencia relativa
  fr <- round(fi/n, 2)
  # Frequencia acumulada (abaixo de)
  fac1 <- cumsum(fi)
  # Frequencia acumulada (acima de)
  # x: representa as frequencias absolutas
  fac2 <- function(x) {
    f1 <- sum(x)
    n <- length(x)
    vet <- c(f1, rep(0,n - 1))
    for (j in 2:n) {
      vet[j] <- vet[j - 1] - x[j - 1]
    }
    return(vet)
  }
  fac22 <- fac2(fi)
  # Frequencia percentual
  fp <- round(fr*100, 2)
  # Fac (abaixo de) percentual
  fac1p = round((fac1/n)*100, 2)
  # Fac (acima de) percentual
  fac22p <- round((fac22/n)*100, 2)
  # Estatisticas
  dados2 <- dados
  attributes(dados2) <- NULL
  estat <- list(
    "Numero_amostra" = n,
    "Numero_de_classes" = k,
    "Amplitude_total" = At,
    "Valor_minimo" = x1,
    "Ampl_clas" = c,
    "LI_da_1_Classe" = LI1,
    "LI_classes" = vi,
    "LS_classes" = vs,
    "raw_data" = dados2
  )
  # Tabela de frequencias
  tabela <- data.frame(Classe = classe, Fi = fi,
                       PM = pm, Fr = fr,
                       Fac1 = fac1, Fac2 = fac22,
                       Fp = fp, Fac1p = fac1p,
                       Fac2p = fac22p)
  # Continuacao...
  # Fi => Frequencia absoluta # PM => Ponto medio da classe
  # Fr => Frequencia relativa # Fac1 => Frequencia acumulada (abaixo de)
  # Fac2 => Frequencia acumulada (acima de)
  # Fp => Frequencia percentual
  # Fac1p => Fac1 percentual  # Fac2p => Fac2 percentual
  listres <- list(tabela = tabela, estat = estat)
  attr(listres, "variable") <- attr(dados, "variable")
  attr(listres, "table") <- "tabfreq"
  class(listres) <- "leem"
  return(listres)
  }
}


# Generico
#' @export
ogive <- function(x, ...) {
  UseMethod("ogive")
}

## Grafico de ogiva
#' @export
ogive.leem <- function(x, decreasing = FALSE, both = FALSE,
                  bars = FALSE,
                  histogram = FALSE,
                  bg = TRUE,
                  main = NULL,
                  xlab = NULL,
                  ylab = NULL,
                  panel.first = grid(col = "white"),
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
                  ...
                  ) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (!is.logical(both)) stop("The both argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (!is.logical(decreasing)) stop("The decreasing argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (!is.logical(bg)) stop("The bg argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (!is.logical(histogram)) stop("The histogram argument must be logical!", call. = FALSE,
                                    domain = "R-leem")
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$tabela$Groups)
    if (numchar) {
      if (both) {
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
        ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))

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
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar1, col = barcol1, border = barborder)

          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar2, col = barcol2, border = barborder)
        }
        if (histogram) {
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar, col = histcol, border = histborder)

        }
        # Pontos
        points(c(xvar[1] - 1, xvar), c(0, yvar1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
        points(c(xvar, max(xvar) + 1), c(yvar2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }
      if (decreasing == FALSE & both == FALSE) {
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
        ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))

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
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar1, col = barcol1, border = barborder)
        }
        if (histogram) {
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar, col = histcol, border = histborder)

        }

        # Pontos
        points(c(xvar[1] - 1, xvar), c(0, yvar1), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)


        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }
      if (decreasing == TRUE & both == FALSE) {
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
        ylim <- c(0, 1.2 * max(c(yvar, yvar1, yvar2)))

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
          ylab <- gettext("Frequency", domain = "R-leem")
        }

        title(main = main, xlab = xlab, ylab = ylab)

        # Retangulo do grafico (lembrar de corrigir com on.exit())
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
               bgcol, border = bgborder)

        # Grid
        panel.first


        # Inserindo barras
        if (bars) {
          if(length(barcol) > 1) {
            barcol1 <- barcol[1]
            barcol2 <- barcol[2]
          } else barcol1 <- barcol2 <- barcol
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar2, col = barcol2, border = barborder)
        }
        if (histogram) {
          rect(xvar - 0.5,
               0,
               xvar + 0.5,
               yvar, col = histcol, border = histborder)

        }

        # Pontos
        points(c(xvar, max(xvar) + 1), c(yvar2, 0), col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

        # Eixos
        axis(1, at = xvaraux)
        axis(2)
      }

    } else {
      stop("Em desencolvimento!")
    }

  }
  if (attr(x, "variable") == "continuous") {
    if (both) {
      xvar <- c(x$estat$LI_classes[1], x$estat$LS_classes)
      yvar <- c(0, x$tabela$Fac1)
      yvar2 <- c(x$tabela$Fac2, 0)

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
      if (bars) {
        if(length(barcol) > 1) {
          barcol1 <- barcol[1]
          barcol2 <- barcol[2]
        } else barcol1 <- barcol2 <- barcol
        rect(x$estat$LI_classes,
             0,
             x$estat$LS_classes,
             x$tabela$Fac1, col = barcol1, border = barborder)

        rect(x$estat$LI_classes,
             0,
             x$estat$LS_classes,
             x$tabela$Fac2, col = barcol2, border = barborder)
      }
      if (histogram) {
        xvar3 <- x$estat$LI_classes
        xvar4 <- x$estat$LS_classes
        yvar3 <- x$tabela$Fi
        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol, border = histborder)

      }
      # Pontos
      points(xvar, yvar, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)
      points(xvar, yvar2, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
    if (decreasing == FALSE & both == FALSE) {
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
        main <- gettext("Ogive larger than", domain = "R-leem")
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
      if (bars) {
        rect(x$estat$LI_classes,
             0,
             x$estat$LS_classes,
             x$tabela$Fac1, col = barcol, border = barborder)
      }
      # Histograma
      if (histogram) {
        xvar3 <- x$estat$LI_classes
        xvar4 <- x$estat$LS_classes
        yvar3 <- x$tabela$Fi
        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol, border = histborder)

      }

      # Pontos
      points(xvar, yvar, col = lpcol, type = type, lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
    if (decreasing == TRUE & both == FALSE) {
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
        main <- gettext("Ogive less than", domain = "R-leem")
      }
      if (is.null(xlab)) {
        xlab <- gettext("Classes", domain = "R-leem")
      }
      if (is.null(ylab)) {
        ylab <- gettext("Frequency", domain = "R-leem")
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
      # Histograma
      if (histogram) {
        xvar3 <- x$estat$LI_classes
        xvar4 <- x$estat$LS_classes
        yvar3 <- x$tabela$Fi
        rect(xvar3,
             0,
             xvar4,
             yvar3, col = histcol, border = histborder)

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



# Generico
#' @export
polyfreq <- function(x, ...) {
  UseMethod("polyfreq")
}
#' @export
polyfreq.leem <- function(x,
                         type = "b",
                         bars = TRUE,
                         bg = TRUE,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         panel.first = grid(col = "white"),
                         gridcol = "lightgray",
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
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "continuous") {
    xvar1 <- c(min(x$estat$LI_classes) - x$estat$Ampl_clas, x$estat$LI_classes,
               max(x$estat$LI_classes) + x$estat$Ampl_clas)
    xvar2 <- c(min(x$estat$LS_classes) - x$estat$Ampl_clas, x$estat$LS_classes,
               max(x$estat$LS_classes) + x$estat$Ampl_clas)
    yvar <- c(0, x$tabela$Fi, 0)
    pm <- c(min(x$estat$LI_classes) - x$estat$Ampl_clas / 2,
            x$tabela$PM,
            max(x$estat$LS_classes) + x$estat$Ampl_clas / 2)
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
      ylab <- gettext("Frequency", domain = "R-leem")
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
    panel.first

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
      if (bars) {
        rect(xvar - 0.5,
             0,
             xvar + 0.5,
             yvar, col = barcol, border = barborder)
      }
      # Pontos
      points(xvaraux, c(0, yvar, 0), col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)
    } else {
      stop("Em desenvolvimento!")
    }
  }
  invisible(x)
}


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
  if (!is.null(attr(x, "table"))) print(x$tabela) else print(unclass(x))
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
