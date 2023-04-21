#' Skewness value
#'
#' Compute the skewness
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
#'   skewness()
#' # Example 2: Normal data
#' rnorm(50, 100, 2.5) |>
#'   new_leem(variable = 2) |>
#'   skewness(grouped = TRUE)
#'
#' @export
skewness <- function (x, type = "pearson", rounding = 2, na.rm = FALSE, details = FALSE, grouped = TRUE) {
  if (!is.logical(details)) stop("The 'details' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(grouped)) stop("The 'grouped' argument must be logical!",
                                 call. = FALSE, domain = "R-leem")
  if (!is.logical(na.rm)) stop("The 'na.rm' argument must be logical!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!",
                               call. = FALSE, domain = "R-leem")
  if (class(x) == "leem" & attr(x, "output") == "newleem") x <- tabfreq(x)
  if (type == "pearson") {
    if (attr(x, "variable") == "discrete") {
      numchar <- is.numeric(x$statistics$raw_data)
      if (numchar) {
        dados <- x$statistics$raw_data
        skew <- (mean(x) - mfreq(x)) / sdev(x)
        skew <- round(skew,
                      digits = rounding)
        resume <- list(`skewness`= skew, table = x$table,
                       rawdata = x$statistics$raw_data)
        if (details) {
          return(resume)
        }
        else {
          return(skew)
        }
      }
      else {
        stop("Measure not used for this data type!",
             call. = FALSE, domain = "R-leem")
      }
    }
    if (attr(x, "variable") == "continuous") {
      if (grouped == TRUE) {
        skew <- (mean(x) - mfreq(x)) / sdev(x)
        skew <- round(skew,
                      digits = rounding)
        resume <- list(`skewness`= skew, table = x$table,
                       rawdata = x$statistics$raw_data)-
        if (details) {
          return(resume)
        }
        else {
          return(skew)
        }
      } else {
        skew <- (mean(x, grouped = FALSE) - mfreq(x, grouped = FALSE)) / sdev(x, grouped = FALSE)
        skew <- round(skew,
                      digits = rounding)
        resume <- list(`skewness`= skew, table = x$table,
                       rawdata = x$statistics$raw_data)-
        if (details) {
          return(resume)
        }
        else {
          return(skew)
        }
      }
    }
    if (length(skew) > 1) warning("Pearson's asymmetry is not appropriate for bimodal, trimodal or higher situations!", domain = "R-leem")
  }
}
