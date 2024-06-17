#------------- ORGANIZANDO A BASE DE DADOS -------------------------------

library(remotes) #p baixar pacote de github
library(microdatasus) #p ler a base de dados do SIM
library(dplyr) #p filtrar e selecionar as variaveis que vamos usar
library(stringr) #p filtrar os CID's que contem F
library(ggplot2) #p montar os graficos

#baixar e processar a base de dados

remotes::install_github("rfsaldanha/microdatasus")
dados <- fetch_datasus(year_start = 2018, year_end = 2022, uf = "ES", information_system = "SIM-DO")
dados <- process_sim(dados)

# selecionar variaveis que vamos usar
dados2 <- dados %>%
  select(CAUSABAS, CODMUNOCOR, CODMUNRES, DTNASC, DTOBITO, IDADE, RACACOR, SEXO, ESTCIV, ESC)

# filtrar de acordo com o CID F10 a F19
dados_filtrados <- dados2[grepl("F1", dados2$CAUSABAS, ignore.case = TRUE),]
                          


#------------CONTRUINDO GRAFICOS ----------------------------------------

# montar grafico de barras baseado no GENERO
grafico_genero <- ggplot(resumo_dados, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Gênero",
       x = "Sexo",
       y = "Quantidade")

# grafico de barras baseado na ESCOLARIDADE
resumo_dados$ESC <- factor(resumo_dados$ESC, 
      levels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais", NA)) #ordem das barras

grafico_escolaridade <- ggplot(resumo_dados, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Escolaridade",
       x = "Escolaridade",
       y = "Quantidade")

# grafico de barras baseado no ESTADO CIVIL
grafico_estado_civil<- ggplot(resumo_dados, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Estado Civil",
       x = "Estado Civil",
       y = "Quantidade")

# grafico de barras baseado na COR/RACA
grafico_raca <- ggplot(resumo_dados, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Cor / Raça",
       x = "Raça",
       y = "Quantidade")

# grafico de barras baseado no NUMERO DE MORTES POR ANO
dados_filtrados$ANO_OBITO <- format(as.Date(dados_filtrados$DTOBITO, "%Y-%m-%d"), "%Y")#extraindo ano da data

grafico_mortes_por_ano <- ggplot(dados_filtrados, aes(x = ANO_OBITO, y = ..count.., fill = ANO_OBITO)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Quantidade de mortes por substância psicoativa por ano",
       x = "Ano",
       y = "Quantidade")

# grafico de barras baseado na IDADE
dados_filtrados$IDADE <- as.numeric(as.character(dados_filtrados$IDADE))#converter coluina da IDADE para numerico

sum(is.na(dados_filtrados$IDADE))

dados_filtrados$IDADE_ANOS <- dados_filtrados$IDADE / 12 #converter idade em meses para idade em anos
      #na conversão 3 valores foram modificados para NA porque foram considerados nao numericos

dados_filtrados$IDADE_GRUPO <- cut(dados_filtrados$IDADE_ANOS, breaks = 
      nclass.Sturges(dados_filtrados$IDADE_ANOS)) #separar classes - Regra de Sturges

grafico_idade <- ggplot(dados_filtrados, aes(x = IDADE_GRUPO, y = ..count.., fill = IDADE_GRUPO)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribuição de Mortes por substância psicoativa - Idade",
       x = "Grupo Etário",
       y = "Quantidade")


#------------BAIXANDO OS GRAFICOS ---------------------------------------

ggsave("grafico_genero.png", grafico_genero, width = 10, height = 6, dpi = 300)
ggsave("grafico_idade.png", grafico_idade, width = 10, height = 6, dpi = 300)
ggsave("grafico_raca.png", grafico_raca, width = 10, height = 6, dpi = 300)
ggsave("grafico_mortes_por_ano.png", grafico_mortes_por_ano, width = 10, height = 6, dpi = 300)
ggsave("grafico_escolaridade.png", grafico_escolaridade, width = 10, height = 6, dpi = 300)
ggsave("grafico_estado_civil.png", grafico_estado_civil, width = 10, height = 6, dpi = 300)

