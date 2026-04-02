# Arquivo: 02-lista-resolucao.R
# Autor: Washington S. da Silva
# Data: 19/03/2026
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)
dplyr::glimpse(gapminder)




# Exercício 1 -------------------------------------------------------------

# Importa base
dados <- read_csv(here("data/raw/productionlog_sample.csv "))

# Visualiza estrutura
glimpse(dados)

# Estatísticas descritivas
summary(dados)



# Exercício 2 -------------------------------------------------------------

dados_expectativa <- gapminder  %>%
  select(country, year, lifeExp)
dados_expectativa 


# Exercício 3 -------------------------------------------------------------

variaveis_exceto_pib <- gapminder  %>%
  select(-pop, -gdpPercap)
variaveis_exceto_pib


# Exercício 4 -------------------------------------------------------------

variaveis_com_c <- gapminder  %>%
  select(starts_with("c"))
variaveis_com_c 


# Exercício 5 -------------------------------------------------------------

variaveis_country_pop <- gapminder  %>%
  select(country:pop)
variaveis_country_pop


# Exercício 6 -------------------------------------------------------------

comeca_termina_p <- gapminder  %>%
  select(contains("p"), ends_with("p"))
comeca_termina_p 


# Exercício 7 -------------------------------------------------------------

paises_america_2007 <- gapminder  %>%
  filter(continent == "Americas" & year == 2007)
paises_america_2007


# Exercício 8 -------------------------------------------------------------

dados_brasil <- gapminder  %>%
  filter(country == "Brazil")
dados_brasil


# Exercício 9 -------------------------------------------------------------

filtro <- gapminder  %>%
  filter(
    continent == "Asia" &
      pop > 50000000 &
      year == 2007
  )
filtro


# Exercício 10 -------------------------------------------------------------

filtro_novo <- gapminder  %>%
  filter(
    lifeExp > 75,
    gdpPercap < 10000,
    year == 2007
  )
filtro_novo


# Exercício 11 -------------------------------------------------------------

pop_em_milhoes <- gapminder  %>%
  mutate(pop_em_milhoes = pop / 1000000)
pop_em_milhoes


# Exercício 12 -------------------------------------------------------------

receita_total <- gapminder  %>%
  mutate(receita_total = gdpPercap * pop)
receita_total


# Exercício 13 -------------------------------------------------------------

economia_grande <- gapminder  %>%
  mutate(economia_grande = ifelse(pop > 50000000, "Sim", "Não"))
economia_grande


# Exercício 14 -------------------------------------------------------------

classificacao_vida <- gapminder  %>%
  filter(year == 2007)  %>%
  mutate(classificacao_vida = case_when(
    lifeExp < 60 ~ "Baixa",
    lifeExp >= 60 & lifeExp <= 75 ~ "Média",
    lifeExp > 75 ~ "Alta"
  ))
classificacao_vida


# Exercício 15 -------------------------------------------------------------

expectativa_por_continente <- gapminder  %>%
  group_by(continent)  %>%
  summarise(expectativa_media = mean(lifeExp))
expectativa_por_continente


# Exercício 16 -------------------------------------------------------------

filtro_agrupamento <- gapminder  %>%
  filter(year == 2007)  %>%
  group_by(continent)  %>%
  summarise(pop_total = sum(pop))
filtro_agrupamento


# Exercício 17 -------------------------------------------------------------

filiais_pib <- gapminder  %>%
  group_by(continent) %>% 
  summarise(
    numero_filiais = n(),
    pib_medio = mean(gdpPercap, na.rm = TRUE),
    melhor_filial = max(gdpPercap, na.rm = TRUE)
  )
filiais_pib


# Exercício 18 -------------------------------------------------------------

evolucao_america <-  gapminder %>% 
  filter(continent == "Americas") %>% 
  group_by(year) %>% 
  summarise(expectativa_media = mean(lifeExp, na.rm = T))
evolucao_america


# Exercício 19 -------------------------------------------------------------

paises_ordenados <- gapminder %>% 
  filter(year == 2007) %>% 
  arrange(desc(lifeExp))
paises_ordenados 


# Exercício 20 -------------------------------------------------------------

gapminder |>
  filter(year == 2007) %>%
  arrange(gdpPercap) %>%
  slice_head(n = 5)


# Exercício 21 -------------------------------------------------------------

paises_america_pop <- gapminder |>
  filter(continent == "Americas", year == 2007) |>
  arrange(desc(pop))
paises_america_pop


# Exercício 22 -------------------------------------------------------------

ranking_continentes <- gapminder |>
  filter(year == 2007) |>
  group_by(continent) |>
  summarise(expectativa_media = mean(lifeExp, na.rm = TRUE)) |>
  arrange(desc(expectativa_media))
ranking_continentes



# ------------------------- FIM ---------------------------------------------#