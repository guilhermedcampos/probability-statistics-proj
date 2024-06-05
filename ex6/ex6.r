set.seed(1973)

# Parâmetros
n <- 40
a <- 4
lambda <- 1/a
num_amostras <- 1000
threshold <- 126

# Inicializar vetor para armazenar as somas Y
valores_Y <- numeric(num_amostras)

# Gerar 1000 amostras de dimensão n=40 da distribuição exponencial com valor esperado a=4
for (i in 1:num_amostras) {
  amostra <- rexp(n, rate = lambda)
  valores_Y[i] <- sum(amostra)
}

# Calcular a proporção de valores de Y maiores que 126
proporcao_simulada <- mean(valores_Y > threshold)

# Abordagem exata usando a distribuição gama
forma <- n  # Parâmetro de forma (k)
rate <- 1/4  # Parâmetro de escala (θ), inverso do valor esperado

# Calcular P(Y > 126) usando a CDF complementar da distribuição gama
proporcao_exata <- pgamma(threshold, forma, rate, lower.tail = FALSE)

diff <- round(abs(proporcao_exata - proporcao_simulada) *100, 5)
diff

