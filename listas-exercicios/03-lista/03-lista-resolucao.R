# Arquivo: 03-lista-resolucao.R
# Autor: Felipe Fonseca
# Data: 01/04/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------


# importa os arquivos
# define o caminho relativo para o arquivo csv
# usando a função here() do pacote here
produtos_csv  <- here("data/raw/produtos.csv")
vendas_csv    <- here("data/raw/vendas.csv")
clientes_csv  <- here("data/raw/clientes.csv")

# importa o arquivo csv com a função readr do pacote readr
# e armazena no objeto dados_vendas
dados_produtos  <- read_csv(produtos_csv)
dados_vendas <- read_csv(vendas_csv)
dados_clientes <- read_csv(clientes_csv)

view(dados_produtos)
view(dados_vendas)
view(dados_clientes)

# analisa os objetos importados
glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

# combina vendas com produtos e clientes
relatorio_vendas <- dados_vendas %>%
  left_join(dados_produtos, by = "codigo_produto") %>%
  left_join(dados_clientes, by = "id_cliente")

# Seleção de variáveis
relatorio_vendas <- relatorio_vendas %>%
  select(
    id_venda,
    data_venda,
    nome_produto,
    categoria,
    quantidade,
    nome_cliente,
    cidade
  )

# exibe a estrutura do resultado
glimpse(relatorio_vendas)

# Filtra registros com valores ausentes
registros_na <- relatorio_vendas %>%
  filter(
    is.na(nome_produto) |
      is.na(nome_cliente)
  )
registros_na

# Opcional: comparação com full_join

relatorio_full <- dados_vendas %>%
  full_join(dados_produtos, by = "codigo_produto")

glimpse(relatorio_full)

# Exercício 2 ---------------------------------------------------------------

# importa os arquivos
governanca_csv <- here("data/raw/governanca.csv")
risco_csv      <- here("data/raw/risco.csv")
contabeis_csv  <- here("data/raw/contabeis.csv")

dados_governanca <- read_csv(governanca_csv)
dados_risco      <- read_csv(risco_csv)
dados_contabeis  <- read_csv(contabeis_csv)

# analisa os objetos importados
glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)

# combina governança, risco e dados contábeis

# 1º join: governança + risco
# Aqui usamos apenas codigo_negociacao, pois governança não varia no tempo
base_gr <- dados_governanca %>%
  left_join(dados_risco, by = "codigo_negociacao")

# 2º join: resultado + contábeis
# Aqui usamos codigo_negociacao + ano (chave composta)
analise_integrada <- base_gr %>%
  left_join(dados_contabeis, by = c("codigo_negociacao", "ano"))

# Seleção de variáveis 
analise_integrada <- analise_integrada %>%
  select(
    empresa,
    codigo_negociacao,
    ano,
    indice_governanca,
    tipo_controlador,
    comite_auditoria,
    retorno_anual,
    volatilidade,
    beta,
    roa,
    alavancagem,
    tamanho_ativo
  )

# exibe a estrutura do resultado
glimpse(analise_integrada)


# Comentários explicativos --------------------------------------------------
# Algumas empresas podem apresentar valores NA após os joins porque:
# - podem existir empresas na base de governança que não possuem dados
#   correspondentes nas bases de risco ou contábeis;
# - podem existir anos para os quais não há informações disponíveis
#   nas bases de risco ou contábeis;
# - inconsistências nos códigos de negociação entre as bases também
#   podem gerar ausência de correspondência.

# O uso de left_join() é adequado porque queremos preservar todas as empresas
# da base principal (dados_governanca), mesmo que não haja informações completas
# nas demais bases. Isso garante que nenhuma empresa seja excluída da análise.

# Exercício 3 ---------------------------------------------------------------


# importa os arquivos
acoes_csv   <- here("data/raw/acoes.csv")
eventos_csv <- here("data/raw/eventos_corporativos.csv")

dados_acoes <- read_csv(acoes_csv)
dados_eventos_corporativos <- read_csv(eventos_csv)

# analisa os objetos importados
glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)

# constrói a base do estudo de eventos
# Inner join: mantém apenas observações com correspondência nas duas bases
dados_estudo_eventos <- dados_acoes %>%
  inner_join(
    dados_eventos_corporativos,
    by = c("ticker", "data" = "data_anuncio")
  )

# Seleção de variáveis

dados_estudo_eventos <- dados_estudo_eventos %>%
  select(
    ticker,
    data,
    tipo_evento,
    valor,
    retorno_diario,
    volume
  )

# exibe o objeto final
glimpse(dados_estudo_eventos)


# Comentários explicativos --------------------------------------------------

# O objeto final possui menos linhas que dados_acoes porque o inner_join()
# mantém apenas as observações que possuem correspondência em ambas as tabelas.
# Ou seja, apenas datas em que há simultaneamente dados de ações e eventos corporativos.

# O inner_join() não mantém observações sem correspondência porque sua lógica
# é retornar apenas a interseção entre as tabelas. Assim:
# - observações de dados_acoes sem evento correspondente são descartadas;
# - eventos sem dados de mercado no mesmo dia também são descartados.

# Esse comportamento é adequado para estudos de eventos, pois garante que
# estamos analisando apenas períodos em que ocorreu um evento corporativo
# e para os quais há dados de mercado disponíveis.

# ------------------------- FIM ---------------------------------------------#
