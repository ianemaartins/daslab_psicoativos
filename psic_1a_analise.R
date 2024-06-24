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

#criar coluna do ano em que o obito ocorreu
dados2$DTOBITO <- ymd(dados2$DTOBITO)
dados2$ANOOBITO <- year(dados2$DTOBITO)

# filtrar de acordo com o CID F10 a F19
dados_filtrados <- dados2[grepl("F1", dados2$CAUSABAS, ignore.case = TRUE),]


#------------------------ DEFININDO CORES ---------------

cores_sexo <- c("Masculino" = "#418e8e", "Feminino" = "#eb613b", "Ignorado" = "#443a37")
cores_raca <- c("Branca" = "#418e8e", "Preta" = "#eb613b", "Amarela" = "#c8cd3b", "Parda" = "#ae8fba", "Indígena" = "#660022", "Ignorado" = "#443a37")
cores_escolaridade <- c("Nenhuma" = "#418e8e", "1 a 3 anos" = "#eb613b", "4 a 7 anos" = "#c8cd3b", "8 a 11 anos" = "#ae8fba", "12 anos ou mais" = "#660022", "NA" = "#443a37")
cores_estado_civil <- c("Solteiro" = "#418e8e", "Casado" = "#eb613b", "Viúvo" = "#c8cd3b", "Separado judicialmente" = "#ae8fba", "União consensual" = "#660022", "Ignorado" = "#443a37")
cores_faixa_etaria <- c("0-12 anos" = "#418e8e", "13-17 anos" = "#eb613b", "18-30 anos" = "#c8cd3b", "31-60 anos" = "#ae8fba", "60+ anos" = "#660022", "NA" = "#443a37")


#------------- RESUMINDO OS DADOS TOTAIS (DADOS2)--------------------------

# Contar frequência de cada categoria - VARIAVEIS CATEGORICAS

frequencias_genero <- dados2 %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)
  
frequencias_raca <- dados2 %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

  frequencias_raca <- frequencias_raca %>% #reordenar variaveis qualitativas nominais
    mutate(RACACOR = reorder(RACACOR, -Quantidade)) %>%
    arrange(desc(Quantidade))

dados2$ESC <- factor(dados2$ESC, levels = c(
  "Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais", NA)) #ordenar as variaveis
frequencias_escolaridade <- dados2 %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

frequencias_estado_civil <- dados2 %>%
  group_by(ESTCIV) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

  frequencias_estado_civil <- frequencias_estado_civil %>% #reordenar variaveis qualitativas nominais
    mutate(ESTCIV = reorder(ESTCIV, -Quantidade)) %>%
    arrange(desc(Quantidade))

# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequencias_idade <- dados2 %>%
  group_by(IDADE2) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

# agrupamento por faixas etarias

dados2$FAIXAETA <- cut(dados2$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequencias_faixa_etaria <- dados2 %>%
  group_by(FAIXAETA) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

#agrupamento pelo ano do obito

frequencias_ano_obito <- dados2 %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)


#------------- RESUMINDO OS DADOS FILTRADOS PELA CID (DADOS_FILTRADOS)--------------------------

# Contar frequencia de cada categoria - VARIAVEIS CATEGORICAS

frequenciasf_genero <- dados_filtrados %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

frequenciasf_raca <- dados_filtrados %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

  frequenciasf_raca <- frequenciasf_raca %>% #reordenar variaveis qualitativas nominais
    mutate(RACACOR = reorder(RACACOR, -Quantidade)) %>%
    arrange(desc(Quantidade))

dados_filtrados$ESC <- factor(dados_filtrados$ESC, levels = c(
  "Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais", NA)) #ordenar as variaveis
frequenciasf_escolaridade <- dados_filtrados %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

frequenciasf_estado_civil <- dados_filtrados %>%
  group_by(ESTCIV) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

  frequenciasf_estado_civil <- frequenciasf_estado_civil %>% #reordenar variaveis qualitativas nominais
    mutate(ESTCIV = reorder(ESTCIV, -Quantidade)) %>%
    arrange(desc(Quantidade))

# Resumo da variavel idade2 - VARIAVEL NUMERICA

frequenciasf_idade <- dados_filtrados %>%
  group_by(IDADE2) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

# agrupamento por faixas etarias

dados_filtrados$FAIXAETA <- cut(dados_filtrados$IDADE2,
  breaks = c(-Inf, 12, 17, 30, 60, Inf),
  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
  right = TRUE) #inclusivo a direita (x-y]

frequenciasf_faixa_etaria <- dados_filtrados %>%
  group_by(FAIXAETA) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

#agrupamento pelo ano do obito

frequenciasf_ano_obito <- dados_filtrados %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)


#------------CONTRUINDO GRAFICOS DE BARRAS DOS DADOS TOTAIS ----------------------------------------

# montar grafico de barras baseado no GENERO

grafico_genero <- ggplot(frequencias_genero, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_sexo) +
  labs(title = "Gênero das pessoas falecidades no ES de 2018 a 2022",
       x = "Gênero",
       y = "Quantidade")

print(grafico_genero)

# grafico de barras baseado na ESCOLARIDADE

grafico_escolaridade <- ggplot(frequencias_escolaridade, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_escolaridade) +
  labs(title = "Escolaridade das pessoas falecidades no ES de 2018 a 2022",
       x = "Escolaridade",
       y = "Quantidade")

print(grafico_escolaridade)

# grafico de barras baseado no ESTADO CIVIL

grafico_estado_civil<- ggplot(frequencias_estado_civil, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_estado_civil) +
  labs(title = "Estado Civil das pessoas falecidades no ES de 2018 a 2022",
       x = "Estado Civil",
       y = "Quantidade")

print(grafico_estado_civil)

# grafico de barras baseado na COR/RACA

grafico_raca <- ggplot(frequencias_raca, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_raca) +
  labs(title = "Raça das pessoas falecidades no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(grafico_raca)

# grafico de barras baseado na IDADE

grafico_idade <- ggplot(frequencias_faixa_etaria, aes(x = FAIXAETA, y = Quantidade, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_faixa_etaria) +
  labs(title = "Faixa Etária das pessoas falecidades no ES de 2018 a 2022",
       x = "Faixa Etária",
       y = "Quantidade")

print(grafico_idade)

# grafico de barras baseado no NUMERO DE MORTES POR ANO

grafico_mortes_por_ano <- ggplot(frequencias_ano_obito, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes no ES de 2018 a 2022",
       x = "Ano",
       y = "Óbitos")

#print(grafico_mortes_por_ano)


#------------CONTRUINDO GRAFICOS DE BARRAS DOS DADOS FILTRADOS ----------------------------------------

# montar grafico de barras baseado no GENERO

graficof_genero <- ggplot(frequenciasf_genero, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_sexo) +
  labs(title = "Gênero das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Gênero",
       y = "Quantidade")

print(graficof_genero)

# grafico de barras baseado na ESCOLARIDADE

graficof_escolaridade <- ggplot(frequenciasf_escolaridade, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_escolaridade) +
  labs(title = "Escolaridade das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Escolaridade",
       y = "Quantidade")

print(graficof_escolaridade)

# grafico de barras baseado no ESTADO CIVIL

graficof_estado_civil<- ggplot(frequenciasf_estado_civil, aes(x = ESTCIV, y = Quantidade, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_estado_civil) +
  labs(title = "Estado Civil das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Estado Civil",
       y = "Quantidade")

print(graficof_estado_civil)

# grafico de barras baseado na COR/RACA

graficof_raca <- ggplot(frequenciasf_raca, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_raca) +
  labs(title = "Raça das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Raça",
       y = "Quantidade")

print(graficof_raca)

# grafico de barras baseado na IDADE

graficof_idade <- ggplot(frequenciasf_faixa_etaria, aes(x = FAIXAETA, y = Quantidade, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_faixa_etaria) +
  labs(title = "Faixa Etária das pessoas falecidades por psicoativos no ES de 2018 a 2022",
       x = "Faixa Etária",
       y = "Quantidade")

print(graficof_idade)

# grafico de barras baseado no NUMERO DE MORTES POR ANO

graficof_mortes_por_ano <- ggplot(frequenciasf_ano_obito, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por psicoativos no ES de 2018 a 2022",
       x = "Ano",
       y = "Óbitos")

#print(graficof_mortes_por_ano)


#------------------- SALVANDO GRAFICOS ---------------------------

ggsave("graficof_genero.png", graficof_genero, width = 10, height = 6, dpi = 300)
ggsave("graficof_idade.png", graficof_idade, width = 10, height = 6, dpi = 300)
ggsave("graficof_raca.png", graficof_raca, width = 10, height = 6, dpi = 300)
ggsave("graficof_mortes_por_ano.png", graficof_mortes_por_ano, width = 10, height = 6, dpi = 300)
ggsave("graficof_escolaridade.png", graficof_escolaridade, width = 10, height = 6, dpi = 300)
ggsave("graficof_estado_civil.png", graficof_estado_civil, width = 10, height = 6, dpi = 300)
ggsave("grafico_genero.png", grafico_genero, width = 10, height = 6, dpi = 300)
ggsave("grafico_idade.png", grafico_idade, width = 10, height = 6, dpi = 300)
ggsave("grafico_raca.png", grafico_raca, width = 10, height = 6, dpi = 300)
ggsave("grafico_mortes_por_ano.png", grafico_mortes_por_ano, width = 10, height = 6, dpi = 300)
ggsave("grafico_escolaridade.png", grafico_escolaridade, width = 10, height = 6, dpi = 300)
ggsave("grafico_estado_civil.png", grafico_estado_civil, width = 10, height = 6, dpi = 300)


#--------------RELAÇÃO ENTRE OS DADOS TOTAIS E OS DADOS POR PSICOATIVOS --------------

#juntando os dados totais x e os dados filtrados segundo as CIDs y de cada variavel

# MORTES POR ANO
uniao_ano_obito <- merge(frequencias_ano_obito, frequenciasf_ano_obito, by = "ANOOBITO") #juntando os dados

uniao_ano_obito <- uniao_ano_obito %>% #mortes por psicoativos a cada 1000 mortes totais
  mutate(Mortes_por_1000 = (Quantidade.y / Quantidade.x) * 1000)

names(uniao_ano_obito) <- c("Ano", "Mortes Totais", "% MT", "Mortes por Psicoativos", "% MP", "MP/1000 MT") #mudar nome das colunas

# GENERO
uniao_genero <- merge(frequencias_genero, frequenciasf_genero, by = "SEXO") #juntando os dados

uniao_genero <- uniao_genero %>% #mortes por psicoativos a cada 1000 mortes totais
  mutate(Mortes_por_1000 = (Quantidade.y / Quantidade.x) * 1000)

names(uniao_genero) <- c("Genero", "Mortes Totais", "% MT", "Mortes por Psicoativos", "% MP", "MP/1000 MT")

#ESCOLARIDADE
uniao_escolaridade <- merge(frequencias_escolaridade, frequenciasf_escolaridade, by = "ESC") #juntando os dados
names(uniao_escolaridade) <- c("escolaridade", "quantidade_total", "porcentagem_total", "quantidade_psicoativos", "porcentagem_psicoativos")

#ESTADO CIVIL
uniao_estado_civil <- merge(frequencias_estado_civil, frequenciasf_estado_civil, by = "ESTCIV") #juntando os dados
names(uniao_estado_civil) <- c("estado_civil", "quantidade_total", "porcentagem_total", "quantidade_psicoativos", "porcentagem_psicoativos")

#RACA
uniao_raca <- merge(frequencias_raca, frequenciasf_raca, by = "RACACOR") #juntando os dados
names(uniao_raca) <- c("raca_cor", "quantidade_total", "porcentagem_total", "quantidade_psicoativos", "porcentagem_psicoativos")

#FAIXA ETARIA
uniao_faixa_etaria <- merge(frequencias_faixa_etaria, frequenciasf_faixa_etaria, by = "FAIXAETA") #juntando os dados
names(uniao_faixa_etaria) <- c("faixa_etaria", "quantidade_total", "porcentagem_total", "quantidade_psicoativos", "porcentagem_psicoativos")

#IDADE

# Calcular a média
media_idade_total <- mean(dados2$IDADE2, na.rm = TRUE)
media_idade_psicoativos <- mean(dados_filtrados$IDADE2, na.rm = TRUE)

# Calcular a mediana
mediana_idade_total <- median(dados2$IDADE2, na.rm = TRUE)
mediana_idade_psicoativos <- median(dados_filtrados$IDADE2, na.rm = TRUE)

#boxplot
boxplot_total <- boxplot(dados2$IDADE2, 
                         main="Idade de falecimento total", 
                         ylab="Valores",
                         col = "#418e8e")
boxplot_psic <- boxplot(dados_filtrados$IDADE2, 
                        main = "Idade de falecimento por psicoativos", 
                        ylab = "Valores", 
                        col = "#eb613b")



