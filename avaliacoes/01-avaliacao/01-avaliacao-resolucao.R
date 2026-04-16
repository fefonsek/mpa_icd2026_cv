# Arquivo: 01-avaliacao-resolucao.R
# Autor(a): Felipe Fonseca
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários

library(tidyverse)
library(here)

# Resolução da Questão 1


# 1.a) --------------------------------------------------------

# importar a planilha com as agências
agencias_csv <- here("data/raw/agencias.csv")
agencia <- read.csv(agencias_csv)
glimpse(agencia)

# importar a planilha com os creditos trimestrais
credito_csv <- here("data/raw/credito_trimestral.csv")
credito <- read.csv(credito_csv)
glimpse(credito)

# importar a planilha com as inadimplencias
inadimplencia_csv <- here("data/raw/inadimplencia.csv")
inadimplencia <- read.csv(inadimplencia_csv)
glimpse(inadimplencia)


# 1.b) ---------------------------------------------------------

credito_long <- credito |> 
  pivot_longer(
    cols = c("T1", "T2", "T3", "T4"), # colunas que serão valores da nova coluna
    names_to = "trimestre", # nome da nova coluna
    values_to = "volume_credito" # nome de outra nova coluna
  )
credito_long


# 1.c) ---------------------------------------------------------

glimpse(agencia)
glimpse(inadimplencia)

dados_completos <- credito_long |>
  left_join(agencia, by = "codigo_agencia") |>
  left_join(inadimplencia, by = c("codigo_agencia", "trimestre"))
view(dados_completos)


# 1.d) ---------------------------------------------------------

# 1. criando variável de volume de crédito por cooperado em R$
credito_por_cooperado = dados_completos |> 
  mutate(credito_por_cooperado = volume_credito * 1000 / num_cooperados)
glimpse(credito_por_cooperado)

# 2. Classificando o  risco de inadimplência com base na coluna taxa_inadimplencia
risco <- credito_por_cooperado |> 
  mutate(risco = case_when(
    taxa_inadimplencia < 3.0 ~ "Baixo",
    taxa_inadimplencia >= 3.0 & taxa_inadimplencia < 4.5 ~ "Moderado",
    taxa_inadimplencia >= 4.5 ~ "Alto"
  ))
glimpse(risco)

dados_analise <- risco
view(dados_analise)


# 1.e) ---------------------------------------------------------

resultados_coopcredminas <- dados_analise |> 
  group_by(cidade) |> 
  summarise(volume_total = sum(volume_credito),
            media_inadimplencia = mean(taxa_inadimplencia, na.rm = T)) |> 
  arrange(desc(volume_total))
resultados_coopcredminas


# Resolução da Questão 2


# 2.a) ---------------------------------------------------------

calcular_prestacao <- function(valor,
                               taxa_anual,
                               num_meses){
  
  # Converter taxa anual para mensal
  taxa_mensal <- taxa_anual / 12
  
  # calculo do PMT
  PMT <- valor * ((taxa_mensal * ((1 + taxa_mensal)^num_meses))
                  /
                    ((1 + taxa_mensal)^num_meses - 1))
  
  return(PMT)
}

# Testando a função
# Valores do exercício
valor <- 120000
taxa_anual <- 0.12
num_meses <- 60

# Cálculo da prestação
resultado <- calcular_prestacao(valor, taxa_anual, num_meses)
resultado


# 2.b) ---------------------------------------------------------

capital_giro <- c(0.08, 0.10, 0.12, 0.14, 0.16)
financiamento <- 120000
num_meses <- 60

calculo <- map_dbl(
  capital_giro,
  ~ calcular_prestacao(
    financiamento,
    taxa_anual = .x,
    num_meses = 60
  )
)
calculo

resultado <- tibble(
  taxa = taxa_anual,
  capital_giro,
  prestacao_mensal = calculo
)
view(resultado)


# 2.c) ---------------------------------------------------------

resultado <- resultado |> 
  mutate(custo_total = prestacao_mensal * 60,
         juros_totais = custo_total - 120000,
         acessibilidade = case_when(
           prestacao_mensal < 2600 ~ "Acessível",
           prestacao_mensal >= 2600 & prestacao_mensal < 2800 ~ "Moderado",
           prestacao_mensal >= 2800 ~ "Elevado"
         ))
resultado
view(resultado)
