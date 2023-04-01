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
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x)
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
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x)
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
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x)
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


# Mean standard error
#mse

# Mean deviation
#mdev

# Mean absolute deviation
#madev
