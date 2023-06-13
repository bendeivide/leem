subtnames_aux <- function(x) {
  if (nchar(x) > 10) {
    x <- substr(x, 1, 10)
    x <- paste(x, "~", sep = "")
  }
  return(x)
}

subtnames <- function(x) {
  x <- as.list(x)
  sapply(x, subtnames_aux)
}

