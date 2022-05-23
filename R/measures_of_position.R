# Dados
# set.seed(10)
# x <- rnorm(36, 100, 50)
# y <- rbinom(36, 10, 0.8)
# y <- rep(letters[1:4], 1:4)
# y |> new_leem(variable = "discrete") |> tabfreq() |> mean()
#x |> new_leem(variable = "continuous") |> tabfreq() |> mean()

# Mean

#' @export
mean.leem <- function(x, trim = 0, na.rm = FALSE, rounding = 2, grouped = TRUE, details = FALSE, ...){
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      average <- round(mean(x = x$estat$raw_data,
                            trim = trim,
                            na.rm = na.rm), digits = rounding)
      resume <- list(average = average, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(average)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped) {
      # Implementar o argumento 'trim' depois!
      average <- round(sum(x$tabela$PM * x$tabela$Fi) / sum(x$tabela$Fi), rounding)
      resume <- list(average = average, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(average)
      }
    } else {
      average <- round(mean(x = x$estat$raw_data,
                            trim = trim,
                            na.rm = na.rm), digits = rounding)
      resume <- list(average = average, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(average)
      }
    }
  }
}

# Median
#' @export
median.leem <- function(x, na.rm = FALSE, rounding = 2, grouped = TRUE, details = FALSE, ...) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      mediana <- round(median(x = x$estat$raw_data,
                            na.rm = na.rm), digits = rounding)
      resume <- list(median = mediana, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(mediana)
      }
    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped) {
      # Implementar o argumento 'trim' depois!
      classem <- x$tabela$Fac1 < x$estat$Numero_amostra / 2
      posm <- which(classem == FALSE)[1]
      l1m <- x$estat$LI_da_1_Classe + x$estat$Ampl_clas * (posm - 1)
      fant <- if (posm == 1) 0 else x$tabela$Fac1[posm - 1]
      fi <- x$tabela$Fi[posm]
      mediana <- round(l1m + ((x$estat$Numero_amostra / 2 -  fant) / fi ) * x$estat$Ampl_clas, digits = rounding)
      resume <- list(median = mediana, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(mediana)
      }
    }
    if (grouped == FALSE){
      mediana <- round(median(x = x$estat$raw_data,
                              na.rm = na.rm), digits = rounding)
      resume <- list(median = mediana, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(mediana)
      }
    }
  }
}

# Mode
#' @export
mfreq <- function(x, details = FALSE) {

  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE)
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar == 0) {
     if (all(x$tabela$Fi == x$tabela$Fi[1])) {
       mo <- "The data set has no mode!"
     } else {
       pos <- which(x$tabela$Fi == max(x$tabela$Fi))
       mo <- x$tabela$Groups[pos]
     }
    } else {
      if (all(x$tabela$Fi == x$tabela$Fi[1])) {
        mo <- "The data set has no mode!"
      } else {
        pos <- which(x$tabela$Fi == max(x$tabela$Fi))
        mo <- as.numeric(x$tabela$Groups[pos])
      }
    }
    resume <- list(mode = mo, table = x$tabela, rawdata = x$estat$raw_data)
    if (details) {
      return(resume)
    } else {
      return(mo)
    }
  }
  if (attr(x, "variable") == "continuous") {
    pos <- which.max(x$tabela$Fi)
    if (pos == 1) {
      aux1 <- 0
    } else{
      aux1 <- x$tabela$Fi[pos - 1]
    }
    if(pos == x$estat$Numero_de_classes){
      aux2 <- 0
    } else{
      aux2 <- x$tabela$Fi[pos + 1]
    }
    del1 <- x$tabela$Fi[pos] - aux1
    del2 <- x$tabela$Fi[pos] - aux2
    mo <- x$estat$LI_classes[pos] + (del1 / (del1 + del2)) * x$estat$Ampl_clas
  }
}
