# Arquivo: 02-avaliacao-resolucao.R
# Autor(a): Felipe Fonseca
# Data: 11/06/2026
# Objetivo: Resolução da Avaliação 2 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(tidyverse) # dplyr, purrr::pmap_dbl() etc.
library(EnvStats)  # distribuição triangular: rtri()
library(tidyquant) # tq_get()



# Resolução da Questão 1 ----------------------------------------

## fixa a semente para reprodutibilidade
set.seed(2026)

## número de cenários a serem simulados
n_sim <- 20000

## duração do projeto, em anos (conhecida)
duracao <- 5

## (a.1) Simula os parâmetros incertos de cada cenário, em R$ mil
cenarios <- tibble(
  cenario = seq_len(n_sim),
  investimento = rtri(n_sim, min = 850, max = 1200, mode = 1000),
  receita_anual = rtri(n_sim, min = 230, max = 350, mode = 290),
  valor_residual = rtri(n_sim, min = 100, max = 200, mode = 150),
  taxa_desconto = rtri(n_sim, min = 0.11, max = 0.15, mode = 0.13)
)
?rtri

## (a.2) Função que calcula o VPL de UM cenário.
## Traduza a fórmula do enunciado
calcula_vpl <- function(investimento, receita_anual,
valor_residual, taxa_desconto, duracao = 5) {
    
    # cria o vetor 1,2,...,duracao
    anos <- seq_len(duracao)

    # valor presente das receitas (constantes) ao longo dos anos
    vp_receitas <- receita_anual * sum(1 / (1 + taxa_desconto) ^ anos)
    
    # valor presente do valor residual, recebido ao final
    vp_residual <- valor_residual / (1 + taxa_desconto) ^ duracao
    
    # VPL é a soma dos valores presentes menos o investimento
    vpl <- vp_receitas + vp_residual - investimento

} 

## (a.3) Aplique a função a todos os cenários com
## a função apropriada do pacote purrr e armazene em 'vpl'
simulacoes <- cenarios |>
  mutate(vpl = pmap_dbl(
    list(investimento = investimento, 
         receita_anual = receita_anual, 
         valor_residual = valor_residual, 
         taxa_desconto = taxa_desconto),
    calcula_vpl
  ))

## vetor de VPLs simulados (use-o nos itens b,c e d)
vpl_sim <- simulacoes$vpl
head(vpl_sim)

## (b) Probabilidade de o projeto destruir valor: (P(VPL < 0)).
prob_vpl_neg <- mean(vpl < 0)

# exibe a probabilidade em percentual
prob_vpl_neg * 100

## Interpretação (item b): escreva aqui sua resposta,
## como comentário







## (c) VPL determinístico (use as MODAS de cada parâmetro)
vpl_deterministico <- calcula_vpl(
  investimento = 1000,
  receita_anual = 290,
  valor_residual = 150,
  taxa_desconto = 0.13
)

# média dos VPLs simulados (compare com o VPL determinístico)
vpl_medio <- mean(vpl_sim)

# desvio-padrão dos VPLs simulados
vpl_sd <- sd(vpl_sim)

# exibe os valores para comparação
vpl_deterministico
vpl_medio
vpl_sd

## Interpretação (item c): escreva aqui sua resposta,
## como comentário







## (d) Histograma da distribuição simulada
hist(vpl_sim,
     breaks = 50,
     col = "lightblue",
     main = "Distribuição Simulada do VPL", 
     xlab = "VPL (R$ mil)"
)

# linha vertical em VPL = 0
abline(v = 0, col = "red", lwd = 2, lty =2)

# linha vertical no VPL médio simulado
abline(v = vpl_medio, col = "blue", lwd = 2)

# legenda das duas linhas
legend("topright", 
       legend = c("VPL = 0", "VPL médio simulado"),
       col = c("red", "blue"), 
       lwd = 2, lty = c(2, 1), bty = "n")


# Resolução da Questão 2 ----------------------------------------

## (a) Importe os preços (ajustados) e calcule os
## retornos log diários.
precos_vale3 <- "VALE3.SA" |> 
  tq_get(get = "stock.prices", 
         from = "2024-01-01",
         to = "2026-06-08") |>
  select(date, adjusted)
head(precos_vale3)

retornos_vale3 <- precos_vale3 |>
  # fórmula do retorno log diário (use o preço ajustado)
  mutate(ret = log(adjusted / lag(adjusted))) |>
  drop_na() # remove o primeiro valor, que é NA

## extrai a coluna ret como um vetor
ret_vale3 <- retornos_vale3 |> pull(ret)
head(ret_vale3)
view(ret_vale3)

## (b) Parâmetros do problema (veja o enunciado).
valor_carteira <- 25000 # R$ 25 mil
p <- 0.99 # nível de confiança

## (c) VaR histórico: quantil da cauda esquerda

## ordena os retornos do pior para o melhor
ret_ordenado <- sort(ret_vale3)

## posição do quantil de p
k <- ceiling((1 - p) * length(ret_ordenado))

## retorno no ponto de corte
retorno_var <- ret_ordenado[k]

## converte o retorno em perda positiva (%)
var_percentual <- -retorno_var * 100

## VaR em reais
var_monetario <- valor_carteira * (-retorno_var)

## exibe os valores do VaR
var_percentual
var_monetario

## (d) Expected Shortfall

## média dos retornos nas posições 1 até k
retorno_medio_cauda <- mean(ret_ordenado[1:k])

## ES em %, como perda positiva
es_percentual <- -retorno_medio_cauda * 100

## ES em reais
es_monetario <- valor_carteira * (-retorno_medio_cauda)

## exibe os valores do ES
es_percentual
es_monetario

## (e) Interpretação: escreva aqui sua resposta, como comentário.








# Resolução da Questão 3 ----------------------------------------

