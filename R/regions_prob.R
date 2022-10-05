#' @export
`%>x>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1", output = "rprob")
    return(x)
  }
}

#' @export
`%>X>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1", output = "rprob")
    return(x)
  }
}


#' @export
`%<X<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2", output = "rprob")
    return(x)
  }
}

#' @export
`%<x<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2", output = "rprob")
    return(x)
  }
}


#' @export
`%>=X>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3", output = "rprob")
  return(x)
}

#' @export
`%>=x>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3", output = "rprob")
  return(x)
}

#' @export
`%<=X<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4", output = "rprob")
  return(x)
}

#' @export
`%<=x<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4", output = "rprob")
  return(x)
}

#' @export
`%>=X>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5", output = "rprob")
    return(x)
  }
}

#' @export
`%>=x>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5", output = "rprob")
    return(x)
  }
}

#' @export
`%>X>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6", output = "rprob")
    return(x)
  }
}

#' @export
`%>x>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6", output = "rprob")
    return(x)
  }
}


#' @export
`%<=X<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7", output = "rprob")
    return(x)
  }
}

#' @export
`%<=x<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7", output = "rprob")
    return(x)
  }
}

#' @export
`%<X<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8", output = "rprob")
    return(x)
  }
}

#' @export
`%<x<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8", output = "rprob")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8", output = "rprob")
    return(x)
  }
}
