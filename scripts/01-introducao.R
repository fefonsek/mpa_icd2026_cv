# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 01_introducao.R
# Autor(a): Felipe Fonseca
# Data: 26/03/2026
# Objetivos: Aprender RStudio, script R e alguns fundamentos da linguagem R

# Atalho para criar seções de código: CTRL + SHIFT + R


# Configuracoes globais  --------------------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
# LEMBRE-SE: Pacotes precisarm ser instalados antes de serem carregados
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, ggplot2, etc.
library(skimr) # para compreender os dados
library(janitor) # para limpar nomes de colunas


# R como uma grande calculadora -------------------------------------------

# Operacoes aritmeticas basicas

# adicao
15 + 7


# subtracao
20 - 6

# multiplicacao
8 * 9

## divisao
84 / 7

## potenciacao
2^5

# Predencia de operacoes matemáticas
# parenteses primeiro, depois potenciacao, multiplicacao e divisao,
# e por ultimo adicao e subtracao

# parentese primeiro
(15 + 7) * 2
84 / (7 + 5)


# Exemplos de Funções matemáticas  ----------------------------------------

# logaritmo natural
log(100)

# logaritmo base 10
log10(100)

# funcao exponencial (e elevado a x)
exp(1)

# valor absoluto
abs(-45)

# raiz quadrada
sqrt(225)

# arredondamento para 2 casas decimais
round(3.14159, digits = 2)


# Tipos atômicos e classes ------------------------------------------------

# Os tipos de dados definem como os dados
# são armazenados na memória.

# tipo double e classe numeric
a <- 3.14
a
# função que retorna o tipo do objeto
typeof(a)
# função que retorna a classe do objeto
class(a)

# character
b <- "João"
b

# logical
c <- TRUE
c

d <- FALSE
d

# NaN (Not a Number) representa um valor indefinido
e <- 0 / 0
e

# Inf (Infinity) representa um valor infinito
f <- 1 / 0
f

# coerção de logical para numeric
# TRUE = 1 e FALSE = 0
f <- as.numeric(c)
f


# Vetores numéricos -------------------------------------------------------

# Cria dois vetores numericos com dados de receita e custo diarios

receita_diaria <- c(9200, 8700, 10100, 9800, 11050)
print(receita_diaria)


custo_diario <- c(6400, 6000, 7200, 6800, 7600)
custo_diario


# Vetorizacao: operacoes elemento a elemento
lucro_diario <- receita_diaria - custo_diario
margem_diaria <- lucro_diario / receita_diaria


# Funções úteis para vetores numéricos  -----------------------------------

# logaritmo da receita diária
log(receita_diaria)

# receita total da semana
sum(receita_diaria)

# receita media da semana
mean(receita_diaria)


# tamanho do vetor de receita
# Nesse caso é o número de dias registrados
length(receita_diaria)

# receita minima da semana
min(receita_diaria)

# receita maxima da semana
max(receita_diaria)

# vendo a ajuda de uma funcao
?mean
?length


# Vetores de caracteres e lógicos -----------------------------------------

# vetores devem conter o mesmo tipo de dados, ou seja,
# todos os elementos devem ser do mesmo tipo

# vetor de caracteres
nome_empresa <- c("Loja A", "Loja B", "Loja C")
# exibe o vetor criado
nome_empresa

# vetor logico (booleano) indicando se a meta de vendas foi batida
meta_batida <- c(TRUE, FALSE, TRUE)
# exibe o vetor criado
meta_batida


# Fatores -----------------------------------------------------------------

# Fatores são usados para armazenar variáveis categóricas
# nominais ou ordinais.

# vetor de caracteres com meses do ano
meses <- c("Dez", "Abr", "Jan", "Mar")
meses

# um vetor de caracteres é ordenado por ordem alfabética
sort(meses)


# definindo os níveis dos meses em ordem cronológica
niveis_meses <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

# converte o vetor meses para fator, usando os niveis definidos
meses <- factor(meses, levels = niveis_meses)
meses

# ordena os meses
sort(meses)


# Importa arquivo de dados ------------------------------------------------

# define o caminho relativo para o arquivo csv
# usando a função here() do pacote here
caminho_csv <- here("data/raw/dados_vendas.csv")

# importa o arquivo csv com a função readr do pacote readr
# e armazena no objeto dados_vendas
dados_vendas <- read_csv(caminho_csv)


# Compreendendo os dados --------------------------------------------------

## Exibe visão geral dos dados
dplyr::glimpse(dados_vendas)

## Visualiza as primeiras linhas da tabela
head(dados_vendas)

## Visão detalhada dos dados
skimr::skim(dados_vendas)


# Preparação dos dados para análise ---------------------------------------

## limpa os nomes das colunas
dados_vendas <- dados_vendas |>
  janitor::clean_names()

## visao geral dos dados
glimpse(dados_vendas)

## converte as colunas de cidade, representante e produto para fatores
dados_vendas_limpos <- dados_vendas |>
  mutate(
    cidade = as.factor(cidade),
    representante = as.factor(representante),
    produto = as.factor(produto)
  )

## verifica a estrutura dos dados
glimpse(dados_vendas_limpos)


## Resumo estatístico do objeto
summary(dados_vendas)
summary(dados_vendas_limpos)


# Salva os dados limpos ---------------------------------------------------

# define o caminho relativo da pasta onde o arquivo limpo será salvo
caminho_csv_limpo <- here("data/clean/dados_vendas_limpos.rds")

# salva o objeto dados_vendas_limpos no formato rds
readr::write_rds(dados_vendas_limpos, caminho_csv_limpo)


## Quer perguntas de negócio você faria para esse conjunto de dados?


# Manipulação/análise com o pacote dplyr  ---------------------------------

# Exemplo 1
# Pergunta de negócio: quero apenas as vendas realizadas em Formiga

dados_vendas_limpos |>
  filter(cidade == "Formiga")


# Exemplo 2
# Pergunta de negócio: quero apenas as vendas realizadas em Formiga que
# geraram receita maior que 1000

dados_vendas_limpos |>
  filter(cidade == "Formiga" & receita > 1000)


# Exemplo 3
# Pergunta de negócio: quero apenas as colunas cidade e receita

dados_vendas_limpos |>
  select(-cidade,-receita)


# Exemplo 4
# Pergunta de negócio: quero saber a receita total por cidade

receita_por_cidade <- dados_vendas_limpos |>
  group_by(cidade) |>
  summarise(receita_total = sum(receita))

# exibe o objeto criado
receita_por_cidade


# Exemplo 5
# Pergunta de negócio: quero saber a receita total por produto

dados_vendas_limpos |>
  group_by(produto) |>
  summarise(receita_total = sum(receita))


# Exemplo 6
# Pergunta de negócio: quero saber a receita total por cidade em
# ordem decrescente e salvar o resultado em outro objeto

receita_por_cidade_produto <- dados_vendas_limpos |>
  group_by(cidade) |>
  summarise(receita_total = sum(receita)) |>
  arrange(desc(receita_total))

# exibe o objeto criado
receita_por_cidade_produto


# Exemplo 6
# Pergunta de negócio: quero saber a receita total por cidade e representante,
# em ordem decrescente de receita

dados_vendas_limpos |>
  group_by(cidade, representante) |>
  summarise(receita_total = sum(receita)) |>
  arrange(desc(receita_total))


# Exemplo 7
# Pergunta de negócio: Quero saber a receita total por cidade e produto
# em ordem decrescente

dados_vendas_limpos |>
  group_by(cidade, produto) |>
  summarise(receita_total = sum(receita)) |>
  arrange(desc(receita_total))

# exibe o objeto criado
receita_por_cidade_produto


# Resolucao dos exercicios  -----------------------------------------------

# define o caminho relativo para o arquivo rds
caminho_rds <- here("data/clean/dados_vendas_limpos.rds")

# importa o arquivo rds com a função read_rds do pacote readr
dados_vendas_limpos <- readr::read_rds(caminho_rds)

# exibe  o objeto
glimpse(dados_vendas_limpos)


# Exercício 1

custos_semanais <- c(5400, 6100, 5900, NA, 6300, 6000)
custos_semanais

## cálculo do custo total usando a função sum() removendo NA
custo_total <- sum(custos_semanais, na.rm = TRUE)
custo_total


# Exercício 2

## cálculo do custo médio usando a função mean() removendo NA
custo_medio <- mean(custos_semanais, na.rm = TRUE)
custo_medio


# Exercício 3

## cálculo do custo mínimo usando a função min()
custo_minimo <- min(custos_semanais, na.rm = TRUE)
custo_minimo

## cálculo do custo máximo usando a função max()
custo_maximo <- max(custos_semanais, na.rm = TRUE)
custo_maximo


# Exercício 4

## filtra os dados para obter apenas as vendas do Produto A
dados_vendas_limpos |>
  filter(produto == "Produto A")


# Exercício 5

## filtra os dados para obter apenas as vendas realizadas em Piumhi
## que tiveram mais de 10 unidades vendidas
dados_vendas_limpos |>
  filter(cidade == "Piumhi" & unidades > 10)


# Exercício 6

## cálculo do total de unidades vendidas por produto.
dados_vendas_limpos |>
  group_by(produto) |>
  summarise(unidades_total = sum(unidades))


# Exercício 7

# cálculo da receita média por cidade
dados_vendas_limpos |>
  group_by(cidade) |>
  summarise(receita_media = mean(receita))


# Exercício 8

# cálculo da receita total por representante
dados_vendas_limpos |>
  group_by(representante) |>
  summarise(receita_total = sum(receita))


# Exercício 9

# cálculo do menor preço unitário por produto
dados_vendas_limpos |>
  group_by(produto) |>
  summarise(preco_unitario_min = min(preco_unitario))



# ------------------------- FIM -------------------------------------------#
