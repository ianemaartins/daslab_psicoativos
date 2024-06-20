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

#criar coluna da data do obito
dados2$DTOBITO <- ymd(dados2$DTOBITO)
dados2$ANOOBITO <- year(dados2$DTOBITO)

# filtrar de acordo com o CID F10 a F19
dados_filtrados <- dados2[grepl("F1", dados2$CAUSABAS, ignore.case = TRUE),]



#------------- RESUMINDO OS DADOS TOTAIS (DADOS2)--------------------------

# Contar frequência de cada categoria - VARIAVEIS CATEGORICAS

frequencias_genero <- dados2 %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequencias_raca <- dados2 %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequencias_escolaridade <- dados2 %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequencias_estado_civil <- dados2 %>%
  group_by(ESTCIV) %>%
  summarise(Quantidade = n(), .groups = "drop")

# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequencias_idade <- dados2 %>%
  group_by(IDADE2) %>%
  summarise(Quantidade = n(), .groups = "drop")

# agrupamento por faixas etarias

dados2$FAIXAETA <- cut(dados2$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0,12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequencias_faixa_etaria <- dados2 %>%
  group_by(FAIXAETA) %>%
  summarise(Quantidade = n(), .groups = "drop")

#agrupamento pelo ano do obito

frequencias_ano_obito <- dados2 %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop")



#------------- RESUMINDO OS DADOS FILTRADOS PELA CID (DADOS_FILTRADOS)--------------------------

# Contar frequencia de cada categoria - VARIAVEIS CATEGORICAS

frequenciasf_genero <- dados_filtrados %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequenciasf_raca <- dados_filtrados %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequenciasf_escolaridade <- dados_filtrados %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop")

frequenciasf_estado_civil <- dados_filtrados %>%
  group_by(ESTCIV) %>%
  summarise(Quantidade = n(), .groups = "drop")

# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequenciasf_idade <- dados_filtrados %>%
  group_by(IDADE2) %>%
  summarise(Quantidade = n(), .groups = "drop")

# agrupamento por faixas etarias

dados_filtrados$FAIXAETA <- cut(dados_filtrados$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0,12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequenciasf_faixa_etaria <- dados_filtrados %>%
  group_by(FAIXAETA) %>%
  summarise(Quantidade = n(), .groups = "drop")

#agrupamento pelo ano do obito

frequenciasf_ano_obito <- dados_filtrados %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop")



#------------CONTRUINDO GRAFICOS DE BARRAS DOS DADOS TOTAIS ----------------------------------------

# montar grafico de barras baseado no GENERO

grafico_genero <- ggplot(frequencias_genero, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gênero das pessoas falecidades no ES de 2018 a 2022",
       x = "Gênero",
       y = "Quantidade")

print(grafico_genero)

# grafico de barras baseado na ESCOLARIDADE

grafico_escolaridade <- ggplot(frequencias_escolaridade, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Escolaridade das pessoas falecidades no ES de 2018 a 2022",
       x = "Escolaridade",
       y = "Quantidade")

print(grafico_escolaridade)

# grafico de barras baseado no ESTADO CIVIL

grafico_estado_civil<- ggplot(frequencias_estado_civil, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Estado Civil das pessoas falecidades no ES de 2018 a 2022",
       x = "Estado Civil",
       y = "Quantidade")

print(grafico_estado_civil)

# grafico de barras baseado na COR/RACA

grafico_raca <- ggplot(frequencias_raca, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Raça das pessoas falecidades no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(grafico_raca)

# grafico de barras baseado na IDADE

grafico_idade <- ggplot(frequencias_faixa_etaria, aes(x = FAIXAETA, y = Quantidade, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Faixa Etária das pessoas falecidades no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(grafico_idade)

# grafico de barras baseado no NUMERO DE MORTES POR ANO

grafico_mortes_por_ano <- ggplot(frequencias_ano_obito, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(grafico_mortes_por_ano)



#------------CONTRUINDO GRAFICOS DE BARRAS DOS DADOS FILTRADOS ----------------------------------------

# montar grafico de barras baseado no GENERO

graficof_genero <- ggplot(frequenciasf_genero, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gênero das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Gênero",
       y = "Quantidade")

print(graficof_genero)

# grafico de barras baseado na ESCOLARIDADE

graficof_escolaridade <- ggplot(frequenciasf_escolaridade, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Escolaridade das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Escolaridade",
       y = "Quantidade")

print(graficof_escolaridade)

# grafico de barras baseado no ESTADO CIVIL

graficof_estado_civil<- ggplot(frequenciasf_estado_civil, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Estado Civil das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Estado Civil",
       y = "Quantidade")

print(graficof_estado_civil)

# grafico de barras baseado na COR/RACA

graficof_raca <- ggplot(frequenciasf_raca, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Raça das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(graficof_raca)

# grafico de barras baseado na IDADE

graficof_idade <- ggplot(frequenciasf_faixa_etaria, aes(x = FAIXAETA, y = Quantidade, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Faixa Etária das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(graficof_idade)

# grafico de barras baseado no NUMERO DE MORTES POR ANO

graficof_mortes_por_ano <- ggplot(frequenciasf_ano_obito, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por psicoativos no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(graficof_mortes_por_ano)




