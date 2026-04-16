# Arquivo: 04-lista-resolucao.R
# Autor(a): Felipe Fonseca
# Data: 17/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------

# Função para calcular montante com juros compostos mensais
calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  
  # Converter taxa anual para mensal
  taxa_mensal <- taxa_anual / 12
  
  # Calcular montante
  montante <- capital * (1 + taxa_mensal)^meses
  
  return(montante)
}

# Valores do exercício
capital <- 5000
taxa_anual <- 0.10
meses <- 36

# Cálculo do montante
resultado <- calcular_montante_mensal(capital, taxa_anual, meses)
resultado

# Exercício 2 ------------------------------------------------

# Função que recebe retorno (em decimal) e classifque textualmente
avaliar_investimento <- function(retorno) {
  
  if (retorno > 0.15) {
    classificacao <- "Excelente"
    
  } else if (retorno > 0.05) {
    classificacao <- "Bom"
    
  } else if (retorno > 0) {
    classificacao <- "Fraco"
    
  } else {
    classificacao <- "Negativo"
  }
  
  return(classificacao)
}

# Valores solicitados
retornos <- c(0.20, 0.08, 0.02, -0.05)

# Aplicar a função
sapply(retornos, avaliar_investimento)

# Exercício 3 ------------------------------------------------

analisar_carteira <- function(dados) {
  
  dados |>
    mutate(
      # Retorno percentual
      retorno = (preco_atual / preco_compra - 1) * 100,
      
      # Valores financeiros
      valor_investido = preco_compra * quantidade,
      valor_atual     = preco_atual * quantidade,
      
      # Resultado financeiro
      resultado = valor_atual - valor_investido,
      
      # Situação do ativo
      situacao = ifelse(resultado > 0, "Ganho", "Perda")
    )
}

carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)
carteira

# Aplicando a função
resultado_carteira <- analisar_carteira(carteira)
resultado_carteira
view(resultado_carteira)

# Exercício 4 ------------------------------------------------

calcular_valor_futuro <- function(capital, taxa_anual, anos){
  
  valor_futuro <- capital * (1 + taxa_anual)^anos
  
  return(valor_futuro)
}

taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

vf_20_anos <- map_dbl(
  taxas_anuais,
  ~ calcular_valor_futuro(
    capital = 10000,
    taxa_anual = .x,
    anos = 20
  )
)
vf_20_anos

comparacao_cenarios <- tibble(
  taxa = taxas_anuais,
  taxa_percentual = taxas_anuais * 100,
  valor_futuro = vf_20_anos,
  ganho_liquido = valor_futuro - 10000
)
comparacao_cenarios

# Exercício 5 ------------------------------------------------

calcular_vpl <- function(investimento_inicial,
                         fluxos_caixa,
                         taxa,
                         valor_residual = 0){
  
  n <- length(fluxos_caixa)
  
  # valor presente dos fluxos
  vp_fluxos <- sum(
    fluxos_caixa / (1 + taxa)^(1:n)
  )
  
  # valor presente do residual
  vp_residual <- valor_residual / (1 + taxa)^n
  
  # VPL
  vpl <- investimento_inicial + vp_fluxos + vp_residual
  
  return(vpl)
}

investimento_inicial <- 300000
fluxos_caixa <- c(80000, 95000, 110000, 100000)
valor_residual <- 30000
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

vpl_projeto <- map_dbl(
  taxas_desconto,
  ~ calcular_vpl(
    investimento_inicial,
    fluxos_caixa,
    taxa = .x,
    valor_residual
  )
)
vpl_projeto

analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl = vpl_projeto,
  decisao = ifelse(vpl > 0, "Viável", "Inviável")
)
analise_projeto

# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------