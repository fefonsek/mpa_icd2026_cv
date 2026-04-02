# Arquivo: 02-dados-organizados-joins.R
# Autor: Felipe Fonseca
# Data: 26/03/2026
# Objetivo:
#  1. Aprender a função pivot_longer() do pacote tidyr
#  2. Aprender joins essenciais do pacote dplyr


# Configuracoes globais  --------------------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
# lembre-se: pacotes precisam ser instalados antes de serem carregados
library(tidyverse) # meta-pacote que inclui dplyr, tidyr, ggplot2 etc.


# Função pivot_longer() ----------------------------------------------------

# Exemplo 1

# Cria uma tibble no formato amplo (wide)
receitas <- tribble(
  ~produto, ~T1, ~T2, ~T3, ~T4,
  "Produto A", 50000, 55000, 60000, 65000,
  "Produto B", 30000, 32000, 35000, 37000,
  "Produto C", 20000, 22000, 25000, 27000
)

# exibe o objeto
receitas


# pipeline que transforma o formato amplo em longo
receitas_longo <- receitas |>
  pivot_longer(
    cols = c("T1", "T2", "T3", "T4"), # colunas que serão valores da nova coluna
    names_to = "trimestre", # nome da nova coluna
    values_to = "receita" # nome de outra nova coluna
  )

# exibe o objeto
receitas_longo


# Exemplo 2

# Cria uma tibble no formato amplo (wide)
desempenho <- tribble(
  ~empresa, ~receita_T1, ~receita_T2, ~despesa_T1, ~despesa_T2,
  "Empresa A", 150000, 175000, 120000, 130000,
  "Empresa B", 250000, 270000, 200000, 220000,
  "Empresa C", 100000, 115000, 80000, 95000
)

# exibe o objeto
desempenho


# pipeline que transforma o formato amplo em longo
desempenho_longo <- desempenho |>
  pivot_longer(
    cols = -empresa, # todas as colunas exceto "empresa"
    names_to = c("indicador", "trimestre"), # nomes das duas novas colunas
    names_sep = "_", # separador nos nomes das colunas
    values_to = "valor" # nome de nova coluna para receber os valores
  )

# exibe o objeto
desempenho_longo
view(desempenho_longo)

# Exemplo 3

# Cria uma tibble no formato amplo (wide)
receita_trimestral <- tribble(
  ~empresa, ~`2022 T1 Receita`, ~`2022 T2 Receita`, ~`2023 T1 Receita`, ~`2023 T2 Receita`,
  "ABC Ltda", 1200000, 1350000, 1500000, 1620000,
  "XYZ S.A.", 2500000, 2700000, 2900000, 3100000
)

# exibe o objeto
receita_trimestral


# pipeline que transforma o formato amplo em longo
receita_trimestral_longa <- receita_trimestral |>
  pivot_longer(
    cols = -empresa, # todas as colunas exceto "empresa"
    names_to = c("ano", "trimestre", NA), # a terceira parte ("Receita") é descartada
    names_sep = " ", # separador nos nomes das colunas
    values_to = "receita" # nome de nova coluna para receber os valores da receita
  )

# exibe o objeto
receita_trimestral_longa


# Joins do dplyr ----------------------------------------------------------

# Dados dos exemplos

## cria a tabela de produtos
produtos <- tribble(
  ~codigo_produto, ~nome_produto, ~preco_unitario, ~categoria,
  "P001", "Notebook Pro", 4500, "Eletrônicos",
  "P002", "Smartphone X", 2800, "Eletrônicos",
  "P003", "Monitor 24pol", 1200, "Informática",
  "P004", "Mouse Gamer", 250, "Informática",
  "P005", "Cadeira Ergonômica", 950, "Mobiliário"
)

## exibe a tabela
produtos


## cria a tabela de vendas
vendas <- tribble(
  ~id_venda, ~codigo_produto, ~id_cliente, ~data_venda,  ~quantidade,
  1,         "P001",          "C001",      "2025-04-15", 1,
  2,         "P002",          "C002",      "2025-04-16", 2,
  3,         "P003",          "C001",      "2025-04-18", 2,
  4,         "P002",          "C003",      "2025-04-20", 1,
  5,         "P006",          "C002",      "2025-04-22", 3,
  6,         "P004",          "C004",      "2025-04-23", 4
)

## exibe a tabela
vendas


## cria a tabela de clientes
clientes <- tribble(
  ~id_cliente, ~nome_cliente,     ~cidade,
  "C001",      "Empresa Alpha",   "São Paulo",
  "C002",      "Empresa Beta",    "Rio de Janeiro",
  "C003",      "João Silva",      "Belo Horizonte",
  "C005",      "Maria Oliveira",  "Recife"
)

## exibe a tabela
clientes

view(clientes)
view(produtos)
view(vendas)


# Sintaxe básica de joins -------------------------------------------------

# formato básico (simples)
# resultado <- x |>
#   tipo_join(y, by = "coluna_comum")

# exemplos com nossas tabelas
vendas_com_produtos <- vendas |>
  left_join(produtos, by = "codigo_produto")

# visualiza o resultado
View(vendas_com_produtos)

vendas_com_clientes <- vendas |>
  left_join(clientes, by = "id_cliente")

# visualiza o resultado
View(vendas_com_clientes)


# Exemplo: left_join()

## cria a tabela de empresas listadas
empresas_listadas <- tribble(
  ~codigo_cvm, ~empresa,            ~setor,              ~segmento_listagem,
  "11592",     "Petrobras",         "Petróleo e Gás",    "Nível 2",
  "19615",     "Vale",              "Mineração",         "Novo Mercado",
  "14311",     "Itaú Unibanco",     "Financeiro",        "Nível 1",
  "18112",     "Natura",            "Bens de Consumo",   "Novo Mercado",
  "22691",     "Magazine Luiza",    "Varejo",            "Novo Mercado"
)

## exibe o objeto
empresas_listadas


# cria a tabela de indicadores contábeis
indicadores_contabeis <- tribble(
  ~codigo_cvm, ~ano_fiscal, ~roa, ~roe, ~ebitda_margem, ~divida_liquida,
  "11592", 2023, 0.089, 0.235, 0.392, 315780000,
  "19615", 2023, 0.112, 0.268, 0.468, 185230000,
  "14311", 2023, 0.064, 0.195, 0.412, NA,
  "22691", 2023, 0.052, 0.148, 0.185, 12450000
)

# exibe o objeto
indicadores_contabeis


# pipeline que combina as duas tabelas com left_join()
analise_empresas <- empresas_listadas |>
  left_join(indicadores_contabeis, by = "codigo_cvm")

# exibe o objeto
analise_empresas



# Exemplo: inner_join()

# cria a tabela de títulos de dívida corporativa
titulos_divida <- tribble(
  ~isin, ~emissor, ~valor_emissao, ~yield_to_maturity, ~vencimento,
  "BRPETRDBS036", "Petrobras", 1000000000, 0.0785, "2030-03-15",
  "BRVALEDBF009", "Vale", 750000000, 0.0652, "2032-10-08",
  "BRITAUDB0025", "Itaú Unibanco", 500000000, 0.0723, "2028-05-22",
  "BRBTGPDB0017", "BTG Pactual", 650000000, 0.0798, "2029-08-30",
  "BRCVCODB0032", "Cielo", 350000000, 0.0815, "2027-11-12"
)

# exibe o objeto
titulos_divida


# cria a tabela de mudanças de rating
mudancas_rating <- tribble(
  ~isin,          ~data_evento, ~agencia,  ~rating_anterior, ~novo_rating, ~perspectiva,
  "BRPETRDBS036", "2023-05-10", "Moody's", "Ba2",            "Ba1",        "Positiva",
  "BRVALEDBF009", "2023-06-22", "S&P",     "BBB",            "BBB+",       "Estável",
  "BRVALEDBF009", "2023-08-15", "Fitch",   "BBB",            "BBB+",       "Estável",
  "BRITAUDB0025", "2023-07-08", "Moody's", "Ba1",            "Baa3",       "Estável",
  "BRECOPDB0016", "2023-09-14", "S&P",     "BB-",            "BB",         "Positiva"
)

# exibe o objeto
mudancas_rating


# pipeline que combina as duas tabelas com inner_join()
analise_rating_impacto <- titulos_divida |>
  inner_join(mudancas_rating, by = "isin")

# exibe o objeto
analise_rating_impacto



# Exemplo: full_join()

# pipeline que combina as duas tabelas
completo_vendas_produtos <- vendas |>
  full_join(produtos, by = "codigo_produto")

# exibe o objeto
completo_vendas_produtos


# ------------------------- FIM ---------------------------------------------#
