#' Frequency distribution table
#'
#' Generic function that allows you to tabulate continuous and categorical data (quantitative or qualitative) in frequency distribution. Depending on the nature of the data, they can be grouped into class ranges or not.
#'
#' @param data R object (data structure vector) of class leem. Use \code{new_leem()} function.
#' @param k Number of classes. Default is \code{NULL}.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#' @param ordered Ordered vector of the same length and elements of data object. Default is \code{NULL}.
#' @param namereduction Logical argument. If \code{TRUE} (default), the group names are reduzed the 10 characters. If \code{FALSE}, otherwise.
#' @return The result of \code{tabfreq()} is a list. This list has two elements: \code{table} and \code{statistics}. The first is the data frequency table, and the second represents some useful statistics for methods of leem class.
#' @examples
#' # Example 1
#' library(leem)
#' x <- rbinom(36, 10, 0.6)
#' x <- new_leem(x, variable = "discrete")
#' tabfreq(x)
#'
#' # Example 2 (Pipe operator)
#' rnorm(36, 100, 4) |> new_leem(variable = "continuous") |> tabfreq()
#'
#' # Example 3
#' x <- rbinom(36, 10, 0.6)
#' # Constructor (object of leem class)
#' x <- new_leem(x, variable = "discrete")
#' tab <- tabfreq(x)
#' # Details
#' tab$table
#' tab$statistics
#'
#' # Example 3 - ordered categories ("d","a", "b", "c")
#' w <- rep(letters[1:4], 1:4)
#' w |> new_leem(variable = "discrete") |> tabfreq(ordered = c("d","a", "b", "c"))
#' @usage
#' tabfreq(dados, ...)
#'
#' ## Leem S3 method:
#' tabfreq(data, k = NULL, na.rm = FALSE, ordered = NULL, namereduction = TRUE, ...)
#'
#' ## Default S3 method:
#' tabfreq(data)
#' @export
tabfreq <- function(dados, ...) {
  UseMethod("tabfreq")
}


#' @export
tabfreq.leem <- function(data, k = NULL, na.rm = FALSE, ordered = NULL, namereduction = TRUE, ...){
  if (class(data) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE,
                                   domain = "R-leem")
  # Output object
  listres <- list(table = NULL, statistics = NULL)
  # defensive programming
  if (anyNA(data)) {
    if (na.rm) {
      if (attr(data, "variable") == "discrete") {
        data <- new_leem(data[!is.na(data)])
      } else{
        data <- new_leem(data[!is.na(data)], variable = 2)
      }
    } else {
      # The data was coerced to string!
      data <- paste0(data)
      data <- new_leem(data)
      attr(listres, "NA") <- "NA"
    }
  }
  if (attr(data, "variable") == "discrete") {
    # numeric ou character
    numchar <- is.numeric(data)
    # tamanho da amostra
    n <- length(data)
    # Dist freq
    aux <- table(data, exclude = NULL)
    if (numchar) {
      groups <- as.numeric(names(aux))
    } else {
      if (is.null(ordered)) {
        groups <- as.character(names(aux))
        if (namereduction) groups <- subtnames(groups)
      } else {
        pos <- match(ordered, as.character(names(aux)))
        aux <- aux[pos]
        names(aux) <- ordered
        groups <- as.character(names(aux))
        if (namereduction) groups <- subtnames(groups)
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
      if (n == 1) {
        vet <- f1
      } else {
        vet <- c(f1, rep(0,n - 1))
        for (j in 2:n) {
          vet[j] <- vet[j - 1] - x[j - 1]
        }
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
    data2 <- data
    attributes(data2) <- NULL
    if (numchar) {
      estat <- list(
        "ngroups" = length(groups),
        "minv" = x1,
        "maxv" = xn,
        "raw_data" = data2
      )
    }
    # Tabela de frequencias
    tabela <- list(Groups = groups, Fi = fi, Fr = fr, Fac1 = fac1, Fac2 = fac22,
                   Fp = fp, Fac1p = fac1p, Fac2p = fac22p)
    tabela <- data.frame(tabela)

    if (numchar) {
      listres$table <- tabela
      listres$statistics <- estat
    } else {
      listres$table <- tabela
    }

    attr(listres, "variable") <- attr(data, "variable")
    attr(listres, "table") <- "tabfreq"
    if (!is.null(ordered)) attr(listres, "levels") <- "ordered"
    class(listres) <- "leem"
    attr(listres, "output") <- "table"
    return(listres)
  }
  if (attr(data, "variable") == "continuous") {
    # data em Rol (Elaborados)
    # sort(data)
    # tamanho da amostra
    n <- length(data)
    if (is.null(k)) {
      # Numero de classes
      if (n < 4) {
        k <- n
      }
      if (n >= 4 & n <= 100) {
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
    if (k < 1) stop("The k argument should be greater than 1!", call. = FALSE,
                    domain = "R-leem")
    # Number of classes
    k <- round(k)
    # Amplitude
    At <- diff(range(data))
    # Minimum value
    x1 <- min(data)
    # Amplitude of class
    c <- round(At/(k - 1), 2)
    # Lower limit of the first class
    LI1 <- x1 - c/2
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
    fi <- freq(data, vi, vs, k)
    # Construindo as classes
    classe <- paste(round(vi, 2), "|--- ", round(vs, 2))
    classe[k] <- paste(round(vi[k], 2), "|--- ", round(vs[k], 2))
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
    fac1p <- round((fac1/n)*100, 2)
    # Fac (acima de) percentual
    fac22p <- round((fac22/n)*100, 2)
    # Estatisticas
    data2 <- data
    attributes(data2) <- NULL
    estat <- list(
      "nsample" = n,
      "nclasses" = k,
      "amplitude" = At,
      "minv" = x1,
      "len_class_interval" = c,
      "lower_lim_1_class" = LI1,
      "lower_lim" = vi,
      "upper_lim" = vs,
      "raw_data" = data2
    )
    # Tabela de frequencias
    tabela <- data.frame(Classes = classe, Fi = fi,
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
    #listres <- list(table = tabela, statistics = estat)
    listres$table <- tabela
    listres$statistics <- estat
    attr(listres, "variable") <- attr(data, "variable")
    attr(listres, "table") <- "tabfreq"
    attr(listres, "output") <- "table"
    class(listres) <- "leem"
    return(listres)
  }
}


