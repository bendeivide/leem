#' @export
pgumbel <- function(q, location = 0, scale = 1, lower.tail = TRUE) {
  z <- (q - location)/scale
  p <- exp(-(z+exp(-z)))/scale
  prob <- exp(-exp(-z))
  if(lower.tail == FALSE){
    prob <- 1 - prob
  }
  return(prob)
}

#' @export
dgumbel <- function(x, location, scale){
  z <- (x - location)/scale
  density <- (exp(-(z + exp(-z))))/scale
  return(density)
}
