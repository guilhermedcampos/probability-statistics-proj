set.seed(1950)  # Fixando a semente

n <- 23  # Graus de liberdade
r <- 300  # Número de amostras
m <- 170  # Número de valores de T em cada amostra
p <- numeric(r)  # Vetor para armazenar as proporções

# Gerar as amostras e calcular T para cada uma
for (i in 1:r) {
  # Gerar m valores de T
  T_values <- replicate(m, {
    Z <- rnorm(n + 1)  # Gerar valores de Z
    sqrt(n) * Z[1] / sqrt(sum(Z[-1]^2))  # Calcular T
  })
  p[i] <- mean(T_values <= 1.5)  # Calcular proporção de T
}

# Calcular a média das proporções
p_aproximado <- mean(p)

# Calcular p utilizando a função pt() para distribuição t-Student
p_real <- pt(1.5, df = n)

# Calcular a diferença absoluta entre as aproximações
diferenca <- abs(p_aproximado - p_real) * 100

# Arredondar o resultado final a 5 casas decimais
resultado_final <- round(diferenca, 5)
resultado_final

