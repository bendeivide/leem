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

# Standard error of the mean of the noncentral Student's t-distribution
.erro_padrao_t_nc <- function(nu, delta) {
  if (nu <= 2) {
    stop("A variância (e portanto o erro padrão) só existe para nu > 2.")
  }

  # Termo da média (não é necessário para EP, mas aparece na fórmula da variância)
  gamma_ratio <- gamma((nu - 1) / 2) / gamma(nu / 2)
  media <- delta * sqrt(nu / 2) * gamma_ratio / sqrt(pi)

  # Variância
  variancia <- (nu * (1 + delta^2)) / (nu - 2) - media^2

  # Erro padrão
  sqrt(variancia)
}

# Average of the noncentral Student's t-distribution
.media_t_nc <- function(nu, delta) {
  if (nu <= 1) {
    stop("A média da distribuição t não central só existe para nu > 1.")
  }

  delta * sqrt(nu / 2) * gamma((nu - 1) / 2) / (sqrt(pi) * gamma(nu / 2))
}

# Standard deviation of the noncentral F distribution
.desvio_padrao_f_nc <- function(d1, d2, ncp) {
  if (d2 <= 4) {
    stop("The variance exists only for df2 > 4.")
  }

  term1 <- 2 * (d2^2) / (d1^2 * (d2 - 2)^2 * (d2 - 4))
  term2 <- d1 * (d2 - 2) + 2 * ncp * (d2 - 4)
  sqrt(term1 * term2)
}
