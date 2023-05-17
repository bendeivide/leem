# All
# mdis <-function(){
#
# }


#' Variance value
#'
#' Compute the sample variance
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   variance()
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   variance(grouped = FALSE)
#'
#' @export
variance <- function (x, rounding = 2, na.rm = FALSE, details = FALSE, grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      vari <- round(var(x = x$statistics$raw_data, na.rm = na.rm),
                    digits = rounding)
      resume <- list(variance = vari, table = x$table,
                     rawdata = x$statistics$raw_data)
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
      vari <- sum((x$table$PM - mean(x))^2 * x$table$Fi)/(x$statistics$nsample -
                                                              1)
      resume <- list(variance = vari, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(vari)
      }
    } else {
      vari <- round(var(x = x$statistics$raw_data, na.rm = na.rm),
                    digits = rounding)
      resume <- list(variance = vari, table = x$table,
                     rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(vari)
      }

    }
  }
}



#' Standard Deviation
#'
#' Compute the sample standard deviation
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   sdev()
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   sdev(grouped = FALSE)
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
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      desvpad <- round(sd(x = x$statistics$raw_data,
                          na.rm = na.rm), digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$table, rawdata = x$statistics$raw_data)
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
      desvpad <- round(sqrt((variance(x))), digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$table,
                     rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    } else {
      desvpad <- round(sd(x = x$statistics$raw_data, na.rm = na.rm),
                       digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$table,
                     rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    }
  }
}

#' Coefficient of variation
#'
#' Compute the sample coeffient of variation
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   cv()
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   cv(grouped = FALSE)
#'
#' @export
cv <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
               grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      cvariation <- round((sdev(x = x, na.rm = na.rm) / mean(x = x,
                                                             na.rm = na.rm)) * 100, digits = rounding)
      resume <- list(`coeffient of variation` = cvariation, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(cvariation)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      cvariation <- round((sdev(x = x, na.rm = na.rm) / mean(x = x,
                                                             na.rm = na.rm)) * 100, digits = rounding)
      resume <- list(`coeffient of variation` = cvariation, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(cvariation)
      }
    } else {
      cvariation <- round((sdev(x = x, na.rm = na.rm, grouped = FALSE) / mean(x = x,
                                                             na.rm = na.rm, grouped = FALSE)) * 100, digits = rounding)
      resume <- list(`coeffient of variation` = cvariation, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(cvariation)
      }
    }
  }
}


#' Mean standard error
#'
#' Compute the sample mean standard error
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' set.seed(10)
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   mse(rounding = 4)
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   mse(grouped = FALSE)
#' @export
mse <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      mserror <- round(sdev(x = x, na.rm = na.rm) / sqrt(length(x$statistics$raw_data)), digits = rounding)
      resume <- list(`mean standard error` = mserror, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(mserror)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      mserror <- round(sdev(x = x, na.rm = na.rm, grouped = TRUE) / sqrt(length(x$statistics$raw_data)), digits = rounding)
      resume <- list(`mean standard error` = mserror, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(mserror)
      }
    } else {
        mserror <- round(sdev(x = x, na.rm = na.rm, grouped = FALSE) / sqrt(length(x$statistics$raw_data)), digits = rounding)
      resume <- list(`mean standard error` = mserror, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(mserror)
      }
    }
  }
}

#' Mean absolute deviation
#'
#' Compute the sample mean absolute deviation
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' set.seed(10)
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   madev(grouped = FALSE)
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   madev()
#' @export
madev <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                  grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      meandev <- round(sum(abs(x$statistics$raw_data - mean(x = x, na.rm = na.rm))), digits = rounding)
      resume <- list(`mean absolute deviation` = meandev, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(meandev)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      meandev <- round(sum(abs(x$table$PM - mean(x = x, na.rm = na.rm, grouped = TRUE)) * x$table$Fi), digits = rounding)
      resume <- list(`mean absolute deviation` = meandev, table = x$table, rawdata = x$statistics$raw_data)

      if (details) {
        return(resume)
      }
      else {
        return(meandev)
      }
    } else {
      meandev <- round(sum(abs(x$statistics$raw_data - mean(x = x, na.rm = na.rm, , grouped = FALSE))), digits = rounding)
      resume <- list(`mean absolute deviation` = meandev, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(meandev)
      }
    }
  }
  }





#' Median absolute deviation
#'
#' Compute the sample median absolute deviation
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' set.seed(10)
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   medev(grouped = FALSE)
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   medev()
#' @export
medev <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                  grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      mediandev <- round(sum(abs(x$statistics$raw_data - median(x = x, na.rm = na.rm))), digits = rounding)
      resume <- list(`median absolute deviation` = mediandev, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(mediandev)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      mediandev <- round(sum(abs(x$table$PM - median(x = x, na.rm = na.rm, grouped = TRUE)) * x$table$Fi), digits = rounding)
      resume <- list(`median absolute deviation` = mediandev, table = x$table, rawdata = x$statistics$raw_data)

      if (details) {
        return(resume)
      }
      else {
        return(mediandev)
      }
    } else {
      mediandev <- round(sum(abs(x$statistics$raw_data - median(x = x, na.rm = na.rm, , grouped = FALSE))), digits = rounding)
      resume <- list(`median absolute deviation` = mediandev, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(mediandev)
      }
    }
  }
}

#' Range
#'
#' Compute the sample range
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param rounding Numerical object. Rounds the values in its first argument to the specified number of decimal places (default \code{2}).
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param details Logical object. Details of data (default \code{FALSE}).
#' @param grouped Logical object. Determines whether the measure of position result will be based on grouped data or not (default \code{TRUE}).
#'
#' @examples
#' # Example 1: Poisson data
#' set.seed(10)
#' rpois(30, 2.5) |>
#'   new_leem() |>
#'   amplitude(grouped = FALSE)
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   amplitude()
#' @export
amplitude <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                       grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x, na.rm = na.rm)
  if (!is.null(attr(x, "NA"))) return(NA)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$statistics$raw_data)
    if (numchar) {
      amp <- round(diff(range(x$statistics$raw_data)), digits = rounding)
      resume <- list(`amplitude` = amp, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(amp)
      }

    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      amp <- round(diff(range(x$table$PM)), digits = rounding)
      resume <- list(`amplitude` = amp, table = x$table, rawdata = x$statistics$raw_data)

      if (details) {
        return(resume)
      }
      else {
        return(amp)
      }
    } else {
      amp <- round(diff(range(x$statistics$raw_data)), digits = rounding)
      resume <- list(`amplitude` = amp, table = x$table, rawdata = x$statistics$raw_data)
      if (details) {
        return(resume)
      } else {
        return(amp)
      }
    }
  }
}
