# Carregar o pacote necessário
library(stats4)

# Dados da amostra
dados <-  c(4.37,4.3,5.15,5.11,5.15,4.66,6.15,5.72,5.87,5.64,4.05)
a <- 4

# Definir a função de densidade
density_f <- function(x, theta) {
  a <- 4
  result <- numeric(length(x))  # Inicializar vetor para armazenar os resultados
  for (i in 1:length(x)) {
    if (x[i] < a) {
      result[i] <- 0
    } else {
      result[i] <- theta * a^theta * x[i]^(-theta - 1)
    }
  }
  return(result)
}

# Definir a função logarítmica da densidade
log_density_f <- function(theta) {
  -sum(log(density_f(dados, theta)))
}
# Obter a MLE de θ usando a função mle
mle_result <- mle(minuslogl = log_density_f, start = list(theta = 3.6))

# Extrair a estimativa de θ
theta_mle <- coef(mle_result)
cat("Estimativa MLE de θ:", theta_mle, "\n")

# Estimar o quantil de probabilidade p = 0.75
a <- 4
p <- 0.75
q_75 <- a / (1 - p)^(1 / theta_mle)
cat("Estimativa do quantil 0.75:", q_75, "\n")

# Valor verdadeiro do quantil para θ = 3.6
theta_verdadeiro <- 3.6
q_75_verdadeiro <- a / (1 - p)^(1 / theta_verdadeiro)
cat("Valor verdadeiro do quantil 0.75:", q_75_verdadeiro, "\n")

# Calcular o desvio absoluto
desvio_absoluto <- abs(q_75 - q_75_verdadeiro)
resultado_final <- round(desvio_absoluto, 4)
cat("Desvio absoluto multiplicado por 100 e arredondado a 4 casas decimais:", resultado_final, "\n")

