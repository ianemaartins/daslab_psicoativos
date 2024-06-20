#------------- ORGANIZANDO A BASE DE DADOS -------------------------------

library(remotes) #p baixar pacote de github
library(microdatasus) #p ler a base de dados do SIM
library(dplyr) #p filtrar e selecionar as variaveis que vamos usar
library(stringr) #p filtrar os CID's que contem F
library(ggplot2) #p montar os graficos
library(lubridate) # p ajustar os dados que envolvem data

#baixar e processar a base de dados
remotes::install_github("rfsaldanha/microdatasus")
dados <- fetch_datasus(year_start = 2018, year_end = 2022, uf = "ES", information_system = "SIM-DO")
dados <- process_sim(dados)

# selecionar variaveis que vamos usar
dados2 <- dados %>%
  select(CAUSABAS, CODMUNOCOR, CODMUNRES, DTNASC, DTOBITO, IDADE, RACACOR, SEXO, ESTCIV, ESC, OCUP)

#criar outra coluna de idade (nascimento - morte) pois a que temos dificulta os calculos
dados2$DTNASC <- ymd(dados2$DTNASC)  
dados2$DTOBITO <- ymd(dados2$DTOBITO)
dados2$IDADE2 <- floor(interval(start = dados2$DTNASC, end = dados2$DTOBITO) / years(1)) #arrendondado p baixo - anos completos


# filtrar de acordo com o CID F10 a F19
dados_filtrados <- dados2[grepl("F1", dados2$CAUSABAS, ignore.case = TRUE),]


#------------- RESUMINDO OS DADOS TOTAIS (DADOS2)--------------------------

# Contar frequência de cada categoria - VARIAVEIS CATEGORICAS

frequencias_genero <- dados2 %>%
  group_by(SEXO) %>%
  summarise(Count = n(), .groups = "drop")

frequencias_raca <- dados2 %>%
  group_by(RACACOR) %>%
  summarise(Count = n(), .groups = "drop")

frequencias_escolaridade <- dados2 %>%
  group_by(ESC) %>%
  summarise(Count = n(), .groups = "drop")

frequencias_estado_civil <- dados2 %>%
  group_by(ESTCIV) %>%
  summarise(Count = n(), .groups = "drop")



# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequencias_idade <- dados2 %>%
  group_by(IDADE2) %>%
  summarise(Count = n(), .groups = "drop")

# agrupamento por faixas etarias

dados2$FAIXAETA <- cut(dados2$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0,12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequencias_faixa_etaria <- dados2 %>%
  group_by(FAIXAETA) %>%
  summarise(Count = n(), .groups = "drop")


#------------- RESUMINDO OS DADOS FILTRADOS PELA CID (DADOS_FILTRADOS)--------------------------

# Contar frequencia de cada categoria - VARIAVEIS CATEGORICAS

frequenciasf_genero <- dados_filtrados %>%
  group_by(SEXO) %>%
  summarise(Count = n(), .groups = "drop")

frequenciasf_raca <- dados_filtrados %>%
  group_by(RACACOR) %>%
  summarise(Count = n(), .groups = "drop")

frequenciasf_escolaridade <- dados_filtrados %>%
  group_by(ESC) %>%
  summarise(Count = n(), .groups = "drop")

frequenciasf_estado_civil <- dados_filtrados %>%
  group_by(ESTCIV) %>%
  summarise(Count = n(), .groups = "drop")



# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequenciasf_idade <- dados_filtrados %>%
  group_by(IDADE2) %>%
  summarise(Count = n(), .groups = "drop")

# agrupamento por faixas etarias

dados_filtrados$FAIXAETA <- cut(dados_filtrados$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0,12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequenciasf_faixa_etaria <- dados_filtrados %>%
  group_by(FAIXAETA) %>%
  summarise(Count = n(), .groups = "drop")





#------------CONTRUINDO GRAFICOS DE BARRAS DOS DADOS FILTRADOS ----------------------------------------

# montar grafico de barras baseado no GENERO
graficof_genero <- ggplot(resumo_dados, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Gênero",
       x = "Sexo",
       y = "Quantidade")

print(graficof_genero)

# grafico de barras baseado na ESCOLARIDADE
resumo_dados$ESC <- factor(resumo_dados$ESC, 
      levels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais", NA)) #ordem das barras

graficof_escolaridade <- ggplot(resumo_dados, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Escolaridade",
       x = "Escolaridade",
       y = "Quantidade")

# grafico de barras baseado no ESTADO CIVIL
graficof_estado_civil<- ggplot(resumo_dados, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Estado Civil",
       x = "Estado Civil",
       y = "Quantidade")

# grafico de barras baseado na COR/RACA
graficof_raca <- ggplot(resumo_dados, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de mortes por uso de substância psicoativa - Cor / Raça",
       x = "Raça",
       y = "Quantidade")


# grafico de barras baseado no NUMERO DE MORTES POR ANO
dados_filtrados$ANO_OBITO <- format(as.Date(dados_filtrados$DTOBITO, "%Y-%m-%d"), "%Y")#extraindo ano da data

graficof_mortes_por_ano <- ggplot(dados_filtrados, aes(x = ANO_OBITO, y = ..count.., fill = ANO_OBITO)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Quantidade de mortes por substância psicoativa por ano",
       x = "Ano",
       y = "Quantidade")

# grafico de barras baseado na IDADE

dados_filtrados$DTNASC <- as.Date(dados_filtrados$DTNASC, format = "%Y-%m-%d") #converter o formato da coluna
dados_filtrados$DTOBITO <- as.Date(dados_filtrados$DTOBITO, format = "%Y-%m-%d")

dados_filtrados$IDADE_CALCULADA <- interval(
  start = dados_filtrados$DTNASC, end = dados_filtrados$DTOBITO) / years(1) #calcular idade no momento do falecimento

dados_filtrados$IDADE_GRUPO <- cut(
  dados_filtrados$IDADE_CALCULADA, breaks = nclass.Sturges(
    dados_filtrados$IDADE_CALCULADA), right = FALSE) #calcular as classes - regra de Sturges

graficof_idade <- ggplot(dados_filtrados, aes(x = IDADE_GRUPO, y = ..count.., fill = IDADE_GRUPO)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribuição de Mortes por substância psicoativa - Idade",
       x = "Grupo Etário",
       y = "Quantidade")



