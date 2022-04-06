# Tabela em distribuicao de frequencias
tabfreq <- function(dados, k = NULL){
  # Dados em Rol (Elaborados)
  sort(dados)
  # tamanho da amostra
  n <- length(dados); n
  if (is.null(k)) {
    # Numero de classes
    if (n <= 100) {
      k <- round(sqrt(n))
      # OBS.: O valor de k nao necessariamente precisa ser
      #       sqrt(n). Esse eh um valor base
    }
    if (n > 100) {
      k <- 5 * log10(n)
    }
  } else {
    if(!is.numeric(k)) stop("O argumento deve ser numérico!")
  }
  # Amplitude total
  At <- diff(range(dados))
  # Valor minimo
  x1 <- min(dados)
  # Amplitude da classe
  c <- round(At/(k - 1), 2)
  # Limite inferior da primeira classe
  LI1 <- x1 - c/2; LI1
  vi <- c(LI1, rep(0, k - 1))
  vs <- c(LI1 + c, rep(0, k - 1))
  # Calculando os limites inferior e superior
  for (i in 2:k) {
    vi[i] <- vi[i - 1] + c
    vs[i] <- vs[i - 1] + c
  }
  vi <- round(vi, 2)
  vs <- round(vs, 2)
  # Funcao para calculo das frequencias das classes
  freq <- function(x, vi, vs, k) {
    freq <- rep(0, k)
    for (i in 1:(k - 1)) {
      freq[i] <- length(x[x >= vi[i] & x < vs[i]])
    }
    freq[k] <- length(x[x >= vi[k] & x <= vs[k]])
    return(freq)
  }
  # Frequencia absoluta
  fi <- freq(dados, vi, vs, k)
  # Construindo as classes
  classe <- paste(round(vi, 2), "|--- ", round(vs, 2))
  classe[k] <- paste(round(vi[k], 2), "|---|", round(vs[k], 2))
  classe
  # Ponto medio
  pm <- (vi + vs)/2
  # Frequencia relativa
  fr <- round(fi/n, 2)
  # Frequencia acumulada (abaixo de)
  fac1 <- cumsum(fi)
  # Frequencia acumulada (acima de)
  # x: representa as frequencias absolutas
  fac2 <- function(x) {
    f1 <- sum(x)
    n <- length(x)
    vet <- c(f1, rep(0,n - 1))
    for (j in 2:n) {
      vet[j] <- vet[j - 1] - x[j - 1]
    }
    return(vet)
  }
  fac22 <- fac2(fi)
  # Frequencia percentual
  fp <- round(fr*100, 2)
  # Fac (abaixo de) percentual
  fac1p = round((fac1/n)*100, 2)
  # Fac (acima de) percentual
  fac22p <- round((fac22/n)*100, 2)
  # Estatisticas
  estat <- list(
    "Numero_de_classes" = k,
    "Amplitude_total" = At,
    "Valor_minimo" = x1,
    "Ampl_clas" = c,
    "LI_da_1_Classe" = LI1,
    "LI_classes" = vi,
    "LS_classes" = vs
  )
  # Tabela de frequencias
  tabela <- data.frame(Classe = classe, Fi = fi,
                       PM = pm, Fr = fr,
                       Fac1 = fac1, Fac2 = fac22,
                       Fp = fp, Fac1p = fac1p,
                       Fac2p = fac22p)
  # Continuacao...
  # Fi => Frequencia absoluta # PM => Ponto medio da classe
  # Fr => Frequencia relativa # Fac1 => Frequencia acumulada (abaixo de)
  # Fac2 => Frequencia acumulada (acima de)
  # Fp => Frequencia percentual
  # Fac1p => Fac1 percentual  # Fac2p => Fac2 percentual
  listres <- list(tabela = tabela, estat = estat)
  return(listres)
}
