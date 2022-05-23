# Calculate of variance
#' @export
variance <- function (x, rounding = 2, na.rm = FALSE, details = FALSE, grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem")
    stop("Use the 'new_leem()' function to create an object of class leem!",
         call. = FALSE)
  if (class(x) == "leem" & is.null(attr(x, "table")))
    x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      vari <- round(var(x = x$estat$raw_data, na.rm = na.rm),
                    digits = rounding)
      resume <- list(variance = vari, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(vari)
      }
    }
    else {
      stop("Measure not used for this data type!",
           call. = FALSE, domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      vari <- sum((x$tabela$PM - mean(x))^2 * x$tabela$Fi)/(x$estat$Numero_amostra -
                                                              1)
      resume <- list(variance = vari, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(vari)
      }
    } else {
      vari <- round(var(x = x$estat$raw_data, na.rm = na.rm),
                    digits = rounding)
      resume <- list(variance = vari, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(vari)
      }

    }
  }
}



# Standard Deviation
#' @export
sdev <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                 grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      desvpad <- round(sd(x = x$estat$raw_data,
                          na.rm = na.rm), digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(desvpad)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      desvpad <- sqrt((variance(x)))
      resume <- list(sdeviation = desvpad, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    } else {
      desvpad <- round(sd(x = x$estat$raw_data, na.rm = na.rm),
                       digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    }
  }
}
