#' Regions of probability
#'
#' @description
#' These binary operators return a vector of
#' length 2, describing the desired probability
#' region.
#'
#' @param a scalar. when referring to a discrete random variable, use the syntax "\code{L}" after the number.
#' @param b scalar. when referring to a discrete random variable, use the syntax "\code{L}" after the number.
#' @export
#' @examples
#' # Example 1 - Discrete
#' 2L %>x>% 5L
#' 2L %>X>% 5L
#' 2L %<X<% 5L
#' 2L %<x<% 5L
#' 2L %>=X>=% 5L
#' 2L %>=x>=% 5L
#' 2L %<=X<=% 5L
#' 2L %<=x<=% 5L
#' 2L %>=X>% 5L
#' 2L %>=x>% 5L
#' 2L %>x>=% 5L
#' 2L %>X>=% 5L
#' 2L %<=X<% 5L
#' 2L %<=x<% 5L
#' 2L %<X<=% 5L
#' 2L %<x<=% 5L
#'
#' # Example 2 - Continuous
#' 2 %>x>% 5
#' 2 %>X>% 5
#' 2 %<X<% 5
#' 2 %<x<% 5
#' 2 %>=X>=% 5
#' 2 %>=x>=% 5
#' 2 %<=X<=% 5
#' 2 %<=x<=% 5
#' 2 %>=X>% 5
#' 2 %>=x>% 5
#' 2 %>x>=% 5
#' 2 %>X>=% 5
#' 2 %<=X<% 5
#' 2 %<=x<% 5
#' 2 %<X<=% 5
#' 2 %<x<=% 5
#'
#' @name Regions_of_probability
#' @return A vector of lenght 2.
`%>x>%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @aliases
#' @export
`%>X>%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%<X<%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%<x<%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2", output = "rprob")
    return(x)
  }
}


#' @rdname Regions_of_probability
#' @export
`%>=X>=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3", output = "rprob")
  return(x)
}

#' @rdname Regions_of_probability
#' @export
`%>=x>=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3", output = "rprob")
  return(x)
}

#' @rdname Regions_of_probability
#' @export
`%<=X<=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4", output = "rprob")
  return(x)
}

#' @rdname Regions_of_probability
#' @export
`%<=x<=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4", output = "rprob")
  return(x)
}

#' @rdname Regions_of_probability
#' @export
`%>=X>%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%>=x>%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%>X>=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%>x>=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6", output = "rprob")
    return(x)
  }
}


#' @rdname Regions_of_probability
#' @export
`%<=X<%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%<=x<%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%<X<=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8", output = "rprob")
    return(x)
  }
}

#' @rdname Regions_of_probability
#' @export
`%<x<=%` <- function(a, b) {
  if (b <= a) stop("The 'b' argument must be greater than 'a' argument!", call. = FALSE, domain = "R-leem")
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8", output = "rprob")
    return(x)
  }
}
