# Arquivo: 04-lista-resolucao.R
# Autor(a): Felipe Fonseca
# Data: 17/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# fixa a semente para reprodutibilidade
set.seed(456)

# número de repetições simuladas
n <- 100000

# simula as quedas separadamente; no modelo simulado, elas são independentes
simulacao <- tibble(
  queda_x = sample(
    c(TRUE, FALSE),
    size = n,
    replace = TRUE,
    prob = c(0.08, 0.92)
  ),
  queda_y = sample(
    c(TRUE, FALSE),
    size = n,
    replace = TRUE,
    prob = c(0.06, 0.94)
  )
)

# mean(TRUE/FALSE) calcula a proporção de TRUE
p_x <- mean(simulacao$queda_x)
p_y <- mean(simulacao$queda_y)

# & identifica as linhas em que as duas ações caíram ao mesmo tempo
p_xy <- mean(simulacao$queda_x & simulacao$queda_y)

# compara a interseção simulada com o produto das probabilidades
tibble(
  `P(X cai)` = p_x,
  `P(Y cai)` = p_y,
  `P(ambas caem)` = p_xy,
  `P(X cai) * P(Y cai)` = p_x * p_y
)