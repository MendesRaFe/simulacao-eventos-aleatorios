## Balcão de Atendimento com fila ##

# Faz a simulação de uma sequêcia de atendimento
atendimento <- function(tempo_total, rate, n, mi) {
  # Inicialização das variáveis de interesse e dos vetores (tdisp e tcheg)
  tempo_acum <- 0
  atendidos <- 0
  naoatendidos <- 0
  clientes_novos <- 0
  tempo_max <- 0
  tdisp <- numeric(n)
  tcheg <- numeric(0)

  # Gera os intervalos de tempo entre um cliente e o próximo até o encerramento do tempo total de atendimento
  while (TRUE) {
    z <- rexp(1, rate)
    tempo_acum <- tempo_acum + z
    if (tempo_acum > tempo_total)
      break
    clientes_novos <- clientes_novos + 1
    tcheg <- c(tcheg, tcheg[clientes_novos])
    tcheg[clientes_novos] <- tempo_acum

    while ((min(tdisp) <= tempo_acum) && (atendidos < clientes_novos)) {
      atendidos <- atendidos + 1
      j <- which.min(tdisp)
      a <- rexp(1, mi)
      tdisp[j] <- max(tdisp[j], tcheg[atendidos]) + a
      tempo_max <- max(tempo_max, (tdisp[j] - tcheg[atendidos]))
    }
    tam_fila <- max(0, (clientes_novos - 1) - atendidos)
    prob <- tam_fila / (tam_fila + n)
    s <- rbinom(1, 1, prob)

    if (s == 1) {
      clientes_novos <- clientes_novos - 1
      naoatendidos <- naoatendidos + 1
    }
    tam_fila <- clientes_novos - atendidos
    prop_naoatendidos <- naoatendidos / (atendidos + naoatendidos + tam_fila)

  }
  return(list(x = atendidos, y = naoatendidos, r = tam_fila,
              w = prop_naoatendidos, tmax = tempo_max))
}

simula_atendimentos <- function(nb, c) {
  vetor_medias_x <- numeric(0)
  vetor_medias_y <- numeric(0)
  vetor_medias_w <- numeric(0)
  vetor_medias_tmax <- numeric(0)
  x_elem <- numeric(0)
  y_elem <- numeric(0)
  w_elem <- numeric(0)
  tmax_elem <- numeric(0)
  tam <- c(nb)

  while (TRUE) {
    for (i in 1:500) {
      resultado <- atendimento(tempo_total = 50, rate = 3, n = 5, mi = 0.5)
      x_elem <- c(x_elem, resultado$x)
      y_elem <- c(y_elem, resultado$y)
      w_elem <- c(w_elem, resultado$w)
      tmax_elem <- c(tmax_elem, resultado$tmax)
    }
    vetor_medias_x <- c(vetor_medias_x, mean(x_elem))
    vetor_medias_y <- c(vetor_medias_y, mean(y_elem))
    vetor_medias_w <- c(vetor_medias_w, mean(w_elem))
    vetor_medias_tmax <- c(vetor_medias_tmax, mean(tmax_elem))

    dp <- sd(w_elem)
    st_err <- dp / sqrt(length(w_elem))
    threshold <- 2 * 1.96 * st_err
    if (threshold < c)
      break
    tam <- c(tam, max(tam) + 500)
  }
  return(list(x_k = vetor_medias_x, y_k = vetor_medias_y,
              w_k = vetor_medias_w, tmax_k = vetor_medias_tmax, k = tam))
}

simulacao <- simula_atendimentos(nb = 500, c = 0.002)
print(simulacao$w_k)
print(simulacao$tmax_k)
print(simulacao$k)
