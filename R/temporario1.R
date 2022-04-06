# Construtor
new_leem <- function(x = vector(), variable = "discrete") {
  stopifnot("The x argument should be vector!" = is.vector(x))
  if(!any(variable == c("discrete", "continuous"))) stop("The variable argument must be 'discrete' or 'continuous'.")
  structure(x, class = "leem", variable = variable)
}

# Generico
tabfreq <- function(dados, ...) {
  UseMethod("tabfreq")
}

# Tabela em distribuicao de frequencias
tabfreq.leem <- function(dados, k = NULL){
  if (attr(dados, "variable") == "discrete") {
    return("Em construção...")
  }
  if (attr(dados, "variable") == "continuous") {
  # Dados em Rol (Elaborados)
  sort(dados)
  # tamanho da amostra
  n <- length(dados); n
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
  classe[k] <- paste(round(vi[k], 2), "|---|", round(vs[k], 2))
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
  estat <- list(
    "Numero_de_classes" = k,
    "Amplitude_total" = At,
    "Valor_minimo" = x1,
    "Ampl_clas" = c,
    "LI_da_1_Classe" = LI1,
    "LI_classes" = vi,
    "LS_classes" = vs,
    "raw_data" = dados
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


## Dados
#set.seed(10)
#x <- new_leem(rnorm(36, 100, 50), variable = "continuous")
#y <- new_leem(rnorm(36, 100, 50), variable = "discrete")
#tab2 <- tabfreq(x);tab2
#tab2$estat$LS_classes

# Generico
ogive <- function(x, ...) {
  UseMethod("ogive")
}

## Grafico de ogiva

ogive.leem <- function(x, decreasing = FALSE, bars = TRUE,
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
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") return("Em construção...")
  if (attr(x, "variable") == "continuous") {
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

      # Pontos
      points(xvar, yvar, col = lpcol, type = type,  lwd = lwd, pch = pch, lty = lty)

      # Eixos
      axis(1, at = xvar)
      axis(2)
    }
  }
}

#ogive(tab2, decreasing = FALSE)
#ogive(x, decreasing = FALSE)
#ogive(y, decreasing = FALSE)


# Generico
polyfreq <- function(x, ...) {
  UseMethod("polyfreq")
}

polyfreq.leem <- function(x,
                         type = "b",
                         bars = TRUE,
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
                         lpcol = "black",
                         lwd = 2,
                         pch = 19,
                         lty = 2, # Type of line
                         ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    return("Em construção...")
  }
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
}

# Poligono
#polyfreq(tab2, lty = 5, type = "b", bars = FALSE)

# Histograma
hist.leem <- function(x,
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
                      ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    return("Em construção...")
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
}

#hist.leem(tab2, barcol = heat.colors(6))
#hist.leem(x, barcol = heat.colors(6))
