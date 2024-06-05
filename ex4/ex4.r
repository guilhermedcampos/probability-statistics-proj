set.seed(2255)

probabilidades <- c(1/55, 2/55, 3/55, 4/55, 5/55, 6/55, 7/55, 8/55, 9/55, 10/55)
valores <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
num_simulacoes <- 150

avisos_sem_desligamento <- 0 
sistemas_nao_desligados <- 0

for (i in 1:num_simulacoes) {
  sinais <- sample(valores, size = 9, replace = TRUE, prob = probabilidades)
  
  aviso_sonoro <- any(sinais == 2)
  
  sistema_desligado <- any(sinais == 1)
  
  if (!sistema_desligado) {
    sistemas_nao_desligados <- sistemas_nao_desligados + 1
  }
  
  if (aviso_sonoro & !sistema_desligado) {
    avisos_sem_desligamento <- avisos_sem_desligamento + 1
  }
}

proporcao <- avisos_sem_desligamento / sistemas_nao_desligados

proporcao_arredondada <- round(proporcao, 2)

proporcao_arredondada
