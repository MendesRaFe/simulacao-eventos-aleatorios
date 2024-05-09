## Balcão de Atendimento com Fila ##

# Função auxiliar para cálculo do erro padrão
erro_padrao <- function(dados) {
  dp <- sd(dados)
  st_err <- dp / sqrt(length(dados))
  return(st_err)
}

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

  # Gera intervalos de tempo entre atendimentos até seu encerramento
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

# Simula N atendentimentos por blocos de 500 e aplica o teor. do limite central
simula_atendimentos <- function(t, rate, n, mi, nb, c) {
  # Inicialização de variáveis e vetores de interesse
  inicio <- 0
  fim <- nb
  tam <- integer(0)
  x_elem <- numeric(nb)
  y_elem <- numeric(nb)
  w_elem <- numeric(nb)
  tmax_elem <- numeric(nb)
  vetor_w_err <- numeric(0)
  vetor_tmax_err <- numeric(0)
  vetor_medias_x <- numeric(0)
  vetor_medias_y <- numeric(0)
  vetor_medias_w <- numeric(0)
  vetor_medias_tmax <- numeric(0)

  while (TRUE) {
    # obtem os dados cumulados a cada nb simulações de atendimento
    for (i in (inicio + 1):fim) {
      resultado <- atendimento(t, rate, n, mi)
      x_elem[i] <- resultado$x
      y_elem[i] <- resultado$y
      w_elem[i] <- resultado$w
      tmax_elem[i] <- resultado$tmax
    }
    tam <- c(tam, fim) # atualiza o vetor de tamanho parcial

    # vetores resultantes com as médias efetuadas a cada 500 iterações
    vetor_medias_x <- c(vetor_medias_x, mean(x_elem))
    vetor_medias_y <- c(vetor_medias_y, mean(y_elem))
    vetor_medias_w <- c(vetor_medias_w, mean(w_elem))
    vetor_medias_tmax <- c(vetor_medias_tmax, mean(tmax_elem))

    # erros padrão de tmax e de w a cada 500 iterações
    tmax_err <- erro_padrao(tmax_elem)
    vetor_tmax_err <- c(vetor_tmax_err, tmax_err)
    w_err <- erro_padrao(w_elem)
    vetor_w_err <- c(vetor_w_err, w_err)

    # faz a convergência em relação ao erro padrão de w
    threshold <- 2 * 1.96 * w_err
    if (threshold < c)
      break

    # aloca mais posições aos vetores de elementos
    x_elem <- c(x_elem, numeric(nb))
    y_elem <- c(y_elem, numeric(nb))
    w_elem <- c(w_elem, numeric(nb))
    tmax_elem <- c(tmax_elem, numeric(nb))
    inicio <- inicio + nb
    fim <- fim + nb
  }
  return(list(x_k = vetor_medias_x, y_k = vetor_medias_y, w_k = vetor_medias_w,
              tmax_k = vetor_medias_tmax, k = tam,
              tmax_ep = vetor_tmax_err, w_ep = vetor_w_err))
}

# Grava o resultado do experimento na variável simulacao
simulacao <- simula_atendimentos(t = 50, rate = 3, n = 5, mi = 0.5,
                                 nb = 500, c = 0.002)

# Gera dataframe com a variável de interesse referente a W
df1 <- cbind.data.frame(simulacao$k, simulacao$w_k, simulacao$w_ep)
names(df1)[names(df1) == "simulacao$w_k"] <- "W"
names(df1)[names(df1) == "simulacao$k"] <- "N"
names(df1)[names(df1) == "simulacao$w_ep"] <- "erro_padrao"

# Adiciona ao df1 as colunas que definem o intervalo de confiança de 95%
df1$lim_inf <- df1$W - 1.96 * df1$erro_padrao
df1$lim_sup <- df1$W + 1.96 * df1$erro_padrao

# Gera o gráfico de linha com as médias parciais de W e o intervalo de confiança
plot(df1$N, df1$W, type = "l", col = "red", xlab = "N", ylab = "W",
     ylim = range(c(df1$W, df1$lim_inf, df1$lim_sup)))
lines(df1$N, df1$lim_inf, lty = "dotted")
lines(df1$N, df1$lim_sup, lty = "dotted")

# Gera dataframe com a variável de interesse referente a média de tempo máximo
df2 <- cbind.data.frame(simulacao$k, simulacao$tmax_k, simulacao$tmax_ep)
names(df2)[names(df2) == "simulacao$tmax_k"] <- "Tmax"
names(df2)[names(df2) == "simulacao$k"] <- "N"
names(df2)[names(df2) == "simulacao$tmax_ep"] <- "erro_padrao"

# Adiciona ao df2 as colunas que definem o intervalo de confiança de 95%
df2$lim_inf <- df2$Tmax - 1.96 * df2$erro_padrao
df2$lim_sup <- df2$Tmax + 1.96 * df2$erro_padrao

# Gera o gráfico de linha com médias parciais de Tmax e o intervalo de confiança
plot(df2$N, df2$Tmax, type = "l", col = "red", xlab = "N", ylab = "Tmax",
     ylim = range(c(df2$Tmax, df2$lim_inf, df2$lim_sup)))
lines(df2$N, df2$lim_inf, lty = "dotted")
lines(df2$N, df2$lim_sup, lty = "dotted")
