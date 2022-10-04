#' @export
`%>x>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1")
    return(x)
  }
}

#' @export
`%>X>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region1")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2] + 1)
    x <- structure(x, class = "leem", region = "region1")
    return(x)
  }
}


#' @export
`%<X<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2")
    return(x)
  }
}

#' @export
`%<x<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region2")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2] - 1)
    x <- structure(x, class = "leem", region = "region2")
    return(x)
  }
}


#' @export
`%>=X>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3")
  return(x)
}

#' @export
`%>=x>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region3")
  return(x)
}

#' @export
`%<=X<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4")
  return(x)
}

#' @export
`%<=x<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region4")
  return(x)
}

#' @export
`%>=X>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5")
    return(x)
  }
}

#' @export
`%>=x>%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region5")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] + 1)
    x <- structure(x, class = "leem", region = "region5")
    return(x)
  }
}

#' @export
`%>X>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6")
    return(x)
  }
}

#' @export
`%>x>=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region6")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] - 1, x[2])
    x <- structure(x, class = "leem", region = "region6")
    return(x)
  }
}


#' @export
`%<=X<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7")
    return(x)
  }
}

#' @export
`%<=x<%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region7")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1], x[2] - 1)
    x <- structure(x, class = "leem", region = "region7")
    return(x)
  }
}

#' @export
`%<X<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8")
    return(x)
  }
}

#' @export
`%<x<=%` <- function(a, b) {
  x <- c(a, b)
  x <- structure(x, class = "leem", region = "region8")
  if (is.double(x)) return(x)
  if (is.integer(x)) {
    x <- c(x[1] + 1, x[2])
    x <- structure(x, class = "leem", region = "region8")
    return(x)
  }
}
