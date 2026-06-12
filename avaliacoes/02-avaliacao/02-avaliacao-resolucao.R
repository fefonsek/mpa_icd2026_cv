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
prob_vpl_neg <- mean(vpl_sim < 0)

# exibe a probabilidade em percentual
prob_vpl_neg * 100

## Interpretação (item b): escreva aqui sua resposta,
## como comentário

## Com base em distribuição triangular, pode-se dizer que
## há uma probabilidade de 23,21% do retorno do projeto
## da Laticínios Serra Azul Ltda. dentro do período de 5 anos
## resultar em prejuízo.

## A probabilidade simulada de VPL < 0 é de aproximadamente 23.21%.

## Esse resultado indica que, sob as hipóteses adotadas na simulação, 
## há uma probabilidade relativamente baixa de a empresa apresentar 
## VPL inferior a 0. Em termos financeiros, isso sugere que, em 
## muitos cenários, a empresa não teria prejuízos em  uma nova linha
## de produção de queijos especiais.

## Essa conclusão depende diretamente das hipóteses do exercício. 
## A simulação não afirma que a empresa necessariamente terá retorno 
## positivo, nem que a receita líquida anual se manterá obrigatoriamente
## constante ao longo dos anos, mas indica que o risco de o VPL ficar
## abaixo do nível de referência relativamente baixo.


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

## O VPL determinístico é igual a 101.41, enquanto o VPL médio 
## simulado é igual a 85.96. Esses valores são distantes, 
## representando uma variabilidade alta de possibilidade de retorno.
## O VPL determinístico usa apenas os valores mais prováveis dentre
## os parâmetros analisados. Já o VPL médio simulado considera todos 
## os cenários gerados a partir das distribuições triangulares.

## A simulação também permite observar a variabilidade do VPL. 
## O desvio-padrão da distribuição dos índices simulados é igual a 
## 114.43, o que mostra que a variabilidade do índice em torno de 
## sua média é relativamente alta.

## Portanto, a principal contribuição da simulação não é apenas 
## calcular um valor médio, mas representar a incerteza em torno 
## do indicador e estimar a probabilidade de ocorrência de situações
## consideradas desfavoráveis.


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
  mutate(ret = log(adjusted / dplyr::lag(adjusted))) |>
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

# organiza os resultados em uma tibble

horizonte <- "1 dia"

resultado_exercicio_2 <- tibble(
  medida = c("VaR histórico", "Expected Shortfall histórico"),
  horizonte = horizonte,
  percentual = c(var_percentual, es_percentual),
  reais = c(var_monetario, es_monetario)
)

## exige os resultados
resultado_exercicio_2

## O VaR histórico para o horizonte de 1 dia, com p = 1%, é de 
## aproximadamente 3.85%, ou R$ 962,80. Esse valor é o limiar de 
## perda associado à cauda esquerda de 1% dos retornos diários 
## observados. Com base no histórico usado, esse limiar de perda 
## diária poderá ser ultrapassado em aproximadamente 1% dos dias. 
## De modo equivalente, em aproximadamente 99% dos dias, a perda 
## diária não deverá ultrapassar esse limiar. O VaR não é a perda 
## máxima possível; ele é o ponto de corte da cauda de perdas.

## O Expected Shortfall histórico para o horizonte de 1 dia, com 
## p = 1%, é de aproximadamente 4.74%, ou R$ 1185.11. Esse valor 
## corresponde à média das perdas localizadas na cauda além do VaR.
## Assim, enquanto o VaR indica o limiar de perda associado a 
## p = 1%, o ES resume a severidade média das perdas mais extremas.




# Resolução da Questão 3 ----------------------------------------

## (a) Importe os preços (ajustados), organize em formato largo
## e calcule os retornos SIMPLES diários de cada ação.
serie_precos <- c("ITUB4.SA", "VALE3.SA", "WEGE3.SA") |> 
  tq_get(get = "stock.prices", 
         from = "2024-01-01",
         to = "2026-06-08") |>
  select(symbol, date, adjusted) |>
  pivot_wider(names_from = symbol, 
              values_from = adjusted) |>
  rename(dia = date,
         itub4 = ITUB4.SA,
         vale3 = VALE3.SA,
         wege3 = WEGE3.SA)

## retorno simples = preçoo / preço anterior -1
retornos <- serie_precos |>
  mutate(
    ret_itub4 = itub4 / dplyr::lag(itub4) - 1,
    ret_vale3 = vale3 / dplyr::lag(vale3) - 1,
    ret_wege3 = wege3 / dplyr::lag(wege3) - 1
  ) |>
  drop_na()

## (b) Calcule o retorno diário da carteira.

## pesos da carteira, na ordem: itub4, vale3, wege3
pesos <- c(itub4 = 0.4, vale3 = 0.35, wege3 = 0.25)

retornos <- retornos |>
  mutate(
    ret_carteira = ret_itub4 * pesos["itub4"] + 
      ret_vale3 * pesos["vale3"] + 
      ret_wege3 * pesos["wege3"]
  )

## (c)-(d) Parâmetros e medidas de risco (VaR e ES)

## parâmetros do problema
valor_carteira <- 100000
p <- 0.01

## ordena os retornos do pior para o melhor
ret_ordenado <- sort(retornos$ret_carteira)

## posição do quantil de p
k <- ceiling(p * length(ret_ordenado))

## VaR em %, como perda positiva
var_percentual <- -ret_ordenado[k] * 100

## VaR em reais
var_monetario <- valor_carteira * (-ret_ordenado[k])

## ES em %, como perda positiva
es_percentual <- -mean(ret_ordenado[1:k]) * 100

## ES em reais
es_monetario <- valor_carteira * (-mean(ret_ordenado[1:k]))

## exibe os valores do VaR e do ES
var_percentual
var_monetario
es_percentual
es_monetario

## (e) Interpretação: escreva aqui sua resposta, como comentário.

## organiza os resultados em uma tibble

horizonte <- "1 dia"
  
resultado_exercicio_3 <- tibble(
  medida = c("VaR histórico", "Expected Shortfall histórico"),
  horizonte = horizonte,
  percentual = c(var_percentual, es_percentual),
  reais = c(var_monetario, es_monetario)
)

## exige os resultados
resultado_exercicio_3

## O VaR histórico diário da carteira, com p = 1%, é de 
## aproximadamente 2.48%, ou R$ 2482.86. Esse valor indica o limiar 
## de perda diária da carteira associado a p = 1%. Com base no histórico 
## usado, esse limiar poderá ser ultrapassado em aproximadamente 1% 
## dos dias. De modo equivalente, em aproximadamente 99% dos dias, 
## a perda diária da carteira não deverá ultrapassar esse valor.

## O Expected Shortfall histórico diário da carteira, com p = 1%, é de 
## aproximadamente 3.16%, ou R$ 3157.13. Esse valor corresponde à 
## média das perdas mais severas.

## Como o ES resume perdas ainda mais severas do que o ponto de 
## corte usado no VaR, ele tende a ser maior em magnitude.
