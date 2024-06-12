

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sp)
library(EnvRtype)


# Data Treatment ----------------------------------------------------------


# loading principal data file
main_data <- read_excel("Input/Dados-ProdutividadeemLatossoloePlintossolo-2018a2023-v1.xlsx")
# correcting col names
colnames(main_data) <- gsub(pattern = '\\s', replacement = '\\_', x = names(main_data))

soybean_data <- main_data |>
  # removing accents
  # removendo acentos
  mutate_at(vars(Local, Textura_do_solo, Cultivar), 
            ~stringi::stri_trans_general(.,"Latin-ASCII")) |>
  # transforming categorical variables
  # transformando variáveis categóricas
  mutate_at(vars(Local,Solo,Cultivar,Textura_do_solo), as.factor) |>
  # transforming the Date covariate
  # transformando a covariável de Data
  mutate_at(vars(Plantio), as.Date)

# correcting coordinates
soybean_data <- soybean_data |>
  separate(Lat._e_Long., into = c("Latit", "Longit"), sep = ";") |>
  mutate(Latit = case_when(
    Local == "Pium" ~ "10°12' 58'' S",
    Local == "Paraiso do Tocantins" ~ "10°11' 16.9'' S",
    Local == "Pedro Afonso" ~ "08°58' 03'' S",
    Local == "Aparecida do Rio Negro" ~ "09°57' 07'' S",
    Local == "Lagoa da Confusao" ~ "10°47' 37'' S",
    TRUE ~ as.character(Latit)
  ),
  Longit = case_when(
    Local == "Pium" ~ "49°15' 1.4'' W",
    Local == "Paraiso do Tocantins" ~ "48°40' 54.6'' W",
    Local == "Goiania" ~ "49°30' 11'' W",
    Local == "Pedro Afonso" ~ "48°10' 29'' W",
    Local == "Aparecida do Rio Negro" ~ "47°58' 19'' W",
    Local == "Lagoa da Confusao" ~ "49°37' 25'' W",
    TRUE ~ as.character(Longit)
  )) |>
  mutate(Latitude = char2dms(from = Latit,chd = "°", chm = "'", chs = "''") 
         |> as.numeric()) |>
  mutate(Longitude = char2dms(from = Longit,chd = "°", chm = "'", chs = "''")
         |> as.numeric())

# correcting planting dates
# corrigindo as datas de plantio
soybean_data <- soybean_data |>
  mutate(Plantio = case_when(
    Plantio == "2022-01-17" ~ as.Date("2022-11-17"),
    Plantio == "2021-11-23" ~ as.Date("2022-11-23"),
    TRUE ~ as.Date(Plantio)),
    Colheita = Plantio+Ciclo)

# creating the harvest cycle categorical covariate
# criando a covariável categórica de ciclo de colheita
soybean_data <- soybean_data |>
  mutate(Cycle3 = cut(x = Ciclo, breaks = c(-Inf, 105,115,Inf),
                      labels = c("Precoce","Medio","Tardio"))) |>
  mutate(Cycle4 = cut(x = Ciclo, breaks = c(-Inf, 100, 110, 120, Inf), 
                      labels = c("Super Precoce","Precoce","Medio","Tardio"))) |>
  # creating an identifier
  mutate(ID = as.numeric(as.factor(paste(Local, Ano, Ciclo, sep = "_"))))

# creating oceanic season variable
soybean_data <- soybean_data |>
  mutate(Temp = case_when(
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 3 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7,-0.5),
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 4 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7,-0.5,-0.2),
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 2 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7),
    year(Plantio) == 2017 & month(Plantio) == 12 & year(Colheita) == 2018 & month(Colheita) == 3 ~ mean(-0.8,-1.0,-0.9,-0.9,-0.7,-0.5),
    year(Plantio) == 2017 & month(Plantio) == 12 & year(Colheita) == 2018 & month(Colheita) == 4 ~ mean(-0.8,-1.0,-0.9,-0.9,-0.7,-0.5,-0.2),
    year(Plantio) == 2019 & month(Plantio) == 11 & year(Colheita) == 2020 & month(Colheita) == 2 ~ mean(0.3,0.5,0.5,0.5,0.5,0.4),
    year(Plantio) == 2019 & month(Plantio) == 11 & year(Colheita) == 2020 & month(Colheita) == 3 ~ mean(0.3,0.5,0.5,0.5,0.5,0.4,0.2),
    year(Plantio) == 2020 & month(Plantio) == 11 & year(Colheita) == 2021 & month(Colheita) == 2 ~ mean(-1.2,-1.3,-1.2,-1.0,-0.9,-0.8),
    year(Plantio) == 2020 & month(Plantio) == 11 & year(Colheita) == 2021 & month(Colheita) == 3 ~ mean(-1.2,-1.3,-1.2,-1.0,-0.9,-0.8,-0.7),
    year(Plantio) == 2021 & month(Plantio) == 10 & year(Colheita) == 2022 & month(Colheita) == 2 ~ mean(-0.7,-0.8,-1.0,-1.0,-1.0,-0.9,-1.0),
    year(Plantio) == 2021 & month(Plantio) == 11 & year(Colheita) == 2022 & month(Colheita) == 3 ~ mean(-0.8,-1.0,-1.0,-1.0,-0.9,-1.0,-1.1),
    year(Plantio) == 2022 & month(Plantio) == 11 & year(Colheita) == 2023 & month(Colheita) == 2 ~ mean(-1.0,-0.9,-0.8,-0.7,-0.4,-0.1),
    year(Plantio) == 2022 & month(Plantio) == 11 & year(Colheita) == 2023 & month(Colheita) == 3 ~ mean(-1.0,-0.9,-0.8,-0.7,-0.4,-0.1,0.2),
  )) |>
  mutate(Caracteristica = cut(Temp, breaks = c(-2, -0.5, 0.5), labels = c("LaNina", "Neutral"))) |>
  mutate(Clima = as.factor(cut(Temp, breaks = c(-1.5,-1.0,-0.5,0.5), 
                               labels = c("ModerateLaNina","WeakLaNina","Neutral"))))





# Nasa Power Data Extraction ----------------------------------------------

# information data frame for extracting NASA POWER
# data frame de informações para extração do NASA POWER
dados <- soybean_data |>
  dplyr::select(ID, Ciclo, Latitude, Longitude, Plantio, Colheita) |>
  distinct(ID, .keep_all = TRUE)

dados <- arrange(dados, ID)

# variáveis coletadas do Nasa Power API
# ALLSKY_SFC_LW_DWN --> Fluxo radiativo infravermelho térmico descendente (onda longa)
# ALLSKY_SFC_SW_DWN --> Incidente de insolação total do céu em uma superfície horizontal
# PRECTOT --> Precipitação acumulada
# RH2M --> Umidade relativa a 2 metros
# T2M --> Temperatura a 2 metros
# T2MDEW --> Ponto de orvalho/gelo a 2 metros
# T2M_MAX --> Temperatura máxima a 2 metros
# T2M_MIN --> Temperatura Mínima a 2 Metros
# WS2M --> Velocidade do vento a 2 metros

# function to extract data
# função pra extrair os dados
get_nasap_data <- function(id) {
  
  ID=dados$ID[id]
  lati = dados$Latitude[id]
  longi = dados$Longitude[id]
  dtplant = dados$Plantio[id]
  dtcolh = dados$Colheita[id]
  
  output <- get_weather(env.id = as.character(ID), 
                        lat = lati, lon = longi, start.day = dtplant,
                        end.day = dtcolh, country = NULL, 
                        variables.names = c("T2M","T2M_MAX","T2M_MIN","T2MDEW",
                                            "WS2M","RH2M","PRECTOT",
                                            "ALLSKY_SFC_LW_DWN","ALLSKY_SFC_SW_DWN"))
  output
}

#applying the function that collects API data to the id vector
# aplicando a função que coleta dos dados da API no vetor de ids
nasaPowerData <- map_df(dados$ID, get_nasap_data)

# calculate and create new climate variables
# calcula e cria novas variáveis de clima
df.clim <- processWTH(env.data = nasaPowerData,
                      Tbase1 = 12,
                      Tbase2 = 24,
                      Topt1 = 33,
                      Topt2 = 37)

# defines stage range and stage names of the EC matrix
# define intervalo de estágio e nomes de estágio da matriz EC
(var.i = names(df.clim)[c(9:14,16,17,21:28)])
stages = c('0.45',"45.70","70.")
interval <- c(0, 45, 70) # do not include the last date

# builds a matrix of climate covariates with q dimensions of environments
# constrói uma matriz de covariáveis climáticas com q dimensões de ambientes
EC1 <- W_matrix(env.data = df.clim,
                env.id = 'env',
                var.id = var.i,
                statistic = 'mean',
                scale = F,
                center = F,
                by.interval = T,
                sd.tol = 5,
                time.window = interval,
                names.window = stages)
EC1 <- as.data.frame(EC1)
EC1$ID <- 1:79

# matriz EC das covariáveis acumuladas: PRECTOT e ALLSKY_SFC_LW_DWN
(var.i = names(df.clim)[c(15,16)])
EC2 <- W_matrix(env.data = df.clim, env.id = 'env', var.id = var.i,
                statistic = 'sum', scale = F, center = F, by.interval = T,
                sd.tol = 5, time.window = interval, names.window = stages)
EC2 <- as.data.frame(EC2)
EC2$ID <- 1:79

# fazendo a junção do conjunto de dados original com os dados obtidos da API
soybean_data <- left_join(x = soybean_data, y = EC1, by = "ID")
soybean_data <- left_join(x = soybean_data, y = EC2, by = "ID")

# escrevendo o conjunto de dados final
write.csv(x = soybean_data, file = "Input/soybean_data-v2.csv", row.names = FALSE)



# Clustering Covariables --------------------------------------------------

# clearing memory
rm(list = ls())

# loading data version 2
soybean_data <- read.csv(file = "Input/soybean_data-v2.csv")


### CLUSTER KGHA ####

dados2 <- soybean_data |>
  dplyr::select(kgha)

# Seed definition to ensure reproducibility
# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Application of the sinuette method
# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - kgha")

# Application of the elbow method
# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss") +
  labs(title = "Wss method - kgha")

ggpubr::ggarrange(g1,g2)

# clustering using kmeans
set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(kghaCluster = factor(x = kmcluster$cluster, levels = c("2","3","1"),
                              labels = c("Low","Middle","High")))

# making prediction to delimit the limits of each level of the clusters
# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(kgha = seq(min(soybean_data$kgha),max(soybean_data$kgha),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$kgha),max(soybean_data$kgha),1)))
# checking cluster boundaries
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER RH2M_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(RH2M_mean_70.)

# # Seed definition to ensure reproducibility # Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - RH2M-70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - RH2M-70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RH2M_phase3 = factor(x = kmcluster$cluster, levels = c("1","3","2"),
                              labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(RH2M_mean_70. = seq(min(soybean_data$RH2M_mean_70.),max(soybean_data$RH2M_mean_70.),0.0001),
                       cluster = predict(kmcluster, seq(min(soybean_data$RH2M_mean_70.),max(soybean_data$RH2M_mean_70.),0.0001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER RH2M_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(RH2M_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - RH2M_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - RH2M_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RH2M_phase2 = factor(x = kmcluster$cluster, levels = c("1","3","2"),
                              labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(RH2M_mean_45.70 = seq(min(soybean_data$RH2M_mean_45.70),max(soybean_data$RH2M_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$RH2M_mean_45.70),max(soybean_data$RH2M_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER RH2M_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(RH2M_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - RH2M_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - RH2M_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RH2M_phase1 = factor(x = kmcluster$cluster, levels = c("2","1","3"),
                              labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(RH2M_mean_0.45 = seq(min(soybean_data$RH2M_mean_0.45),max(soybean_data$RH2M_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$RH2M_mean_0.45),max(soybean_data$RH2M_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER T2M_MAX_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MAX_mean_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, fdm2id::KMEANS, method = "silhouette") +
  labs(title = "Silhouette method - T2M_MAX-70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, fdm2id::KMEANS, method = "wss", k.max = 10) +
  labs(title = "Wss method - T2M_MAX-70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MAX_phase3 = factor(x = kmcluster$cluster, levels = c("1","2","3"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MAX_mean_70. = seq(min(soybean_data$T2M_MAX_mean_70.),max(soybean_data$T2M_MAX_mean_70.),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MAX_mean_70.),max(soybean_data$T2M_MAX_mean_70.),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER T2M_MAX_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MAX_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_MAX_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_MAX_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MAX_phase2 = factor(x = kmcluster$cluster, levels = c("1","2"),
                                 labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MAX_mean_45.70 = seq(min(soybean_data$T2M_MAX_mean_45.70),max(soybean_data$T2M_MAX_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MAX_mean_45.70),max(soybean_data$T2M_MAX_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_MAX_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MAX_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_MAX_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_MAX_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MAX_phase1 = factor(x = kmcluster$cluster, levels = c("1","2"),
                                 labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MAX_mean_0.45 = seq(min(soybean_data$T2M_MAX_mean_0.45),max(soybean_data$T2M_MAX_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MAX_mean_0.45),max(soybean_data$T2M_MAX_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(T2M_mean_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - T2M-70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - T2M-70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_phase3 = factor(x = kmcluster$cluster, levels = c("2","1","3"),
                             labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_mean_70. = seq(min(soybean_data$T2M_mean_70.),max(soybean_data$T2M_mean_70.),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_mean_70.),max(soybean_data$T2M_mean_70.),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_phase2 = factor(x = kmcluster$cluster, levels = c("1","2"),
                             labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_mean_45.70 = seq(min(soybean_data$T2M_mean_45.70),max(soybean_data$T2M_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_mean_45.70),max(soybean_data$T2M_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_phase1 = factor(x = kmcluster$cluster, levels = c("1","2"),
                             labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_mean_0.45 = seq(min(soybean_data$T2M_mean_0.45),max(soybean_data$T2M_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_mean_0.45),max(soybean_data$T2M_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_MIN_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MIN_mean_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - T2M_MIN_70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - T2M_MIN_70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MIN_phase3 = factor(x = kmcluster$cluster, levels = c("2","1","3"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MIN_mean_70. = seq(min(soybean_data$T2M_MIN_mean_70.),max(soybean_data$T2M_MIN_mean_70.),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MIN_mean_70.),max(soybean_data$T2M_MIN_mean_70.),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_MIN_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MIN_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_MIN_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_MIN_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MIN_phase2 = factor(x = kmcluster$cluster, levels = c("1","2"),
                                 labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MIN_mean_45.70 = seq(min(soybean_data$T2M_MIN_mean_45.70),max(soybean_data$T2M_MIN_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MIN_mean_45.70),max(soybean_data$T2M_MIN_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2M_MIN_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(T2M_MIN_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2M_MIN_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2M_MIN_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2M_MIN_phase1 = factor(x = kmcluster$cluster, levels = c("1","3","2"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2M_MIN_mean_0.45 = seq(min(soybean_data$T2M_MIN_mean_0.45),max(soybean_data$T2M_MIN_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2M_MIN_mean_0.45),max(soybean_data$T2M_MIN_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2MDEW_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(T2MDEW_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2MDEW_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2MDEW_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2MDEW_phase2 = factor(x = kmcluster$cluster, levels = c("1","2","3"),
                                labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2MDEW_mean_45.70 = seq(min(soybean_data$T2MDEW_mean_45.70),max(soybean_data$T2MDEW_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2MDEW_mean_45.70),max(soybean_data$T2MDEW_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2MDEW_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(T2MDEW_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - T2MDEW_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - T2MDEW_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2MDEW_phase1 = factor(x = kmcluster$cluster, levels = c("1","2"),
                                labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2MDEW_mean_0.45 = seq(min(soybean_data$T2MDEW_mean_0.45),max(soybean_data$T2MDEW_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2MDEW_mean_0.45),max(soybean_data$T2MDEW_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



### CLUSTER T2MDEW_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(T2MDEW_mean_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - T2MDEW-70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - T2MDEW-70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(T2MDEW_phase3 = factor(x = kmcluster$cluster, levels = c("3","1","2"),
                                labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(T2MDEW_mean_70. = seq(min(soybean_data$T2MDEW_mean_70.),max(soybean_data$T2MDEW_mean_70.),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$T2MDEW_mean_70.),max(soybean_data$T2MDEW_mean_70.),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER WS2M_mean_70. ####

dados2 <- soybean_data |>
  dplyr::select(WS2M_mean_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - WS2M_70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - WS2M_70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(WS2M_phase3 = factor(x = kmcluster$cluster, levels = c("3","1","2"),
                              labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(WS2M_mean_70. = seq(min(soybean_data$WS2M_mean_70.),max(soybean_data$WS2M_mean_70.),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$WS2M_mean_70.),max(soybean_data$WS2M_mean_70.),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER WS2M_mean_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(WS2M_mean_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - WS2M_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - WS2M_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 2)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(WS2M_phase2 = factor(x = kmcluster$cluster, levels = c("2","1"),
                              labels = c("Low","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(WS2M_mean_45.70 = seq(min(soybean_data$WS2M_mean_45.70),max(soybean_data$WS2M_mean_45.70),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$WS2M_mean_45.70),max(soybean_data$WS2M_mean_45.70),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER WS2M_mean_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(WS2M_mean_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - WS2M_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - WS2M_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(WS2M_phase1 = factor(x = kmcluster$cluster, levels = c("3","2","1"),
                              labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(WS2M_mean_0.45 = seq(min(soybean_data$WS2M_mean_0.45),max(soybean_data$WS2M_mean_0.45),0.001),
                       cluster = predict(kmcluster, seq(min(soybean_data$WS2M_mean_0.45),max(soybean_data$WS2M_mean_0.45),0.001)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER ALLSKY_SFC_LW_DWN_sum_70. ####

dados2 <- soybean_data |>
  dplyr::select(ALLSKY_SFC_LW_DWN_sum_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - \nALLSKY_SFC_LW_DWN_phase3")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - \nALLSKY_SFC_LW_DWN_phase3")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RADIATION_phase3 = factor(x = kmcluster$cluster, levels = c("3","2","1"),
                                   labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(ALLSKY_SFC_LW_DWN_sum_70. = seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_70.),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_70.),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_70.),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_70.),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER ALLSKY_SFC_LW_DWN_sum_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(ALLSKY_SFC_LW_DWN_sum_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - \nALLSKY_SFC_LW_DWN_phase2")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - \nALLSKY_SFC_LW_DWN_phase2")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RADIATION_phase2 = factor(x = kmcluster$cluster, levels = c("3","1","2"),
                                   labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(ALLSKY_SFC_LW_DWN_sum_45.70 = seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_45.70),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_45.70),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_45.70),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_45.70),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER ALLSKY_SFC_LW_DWN_sum_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(ALLSKY_SFC_LW_DWN_sum_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - \nALLSKY_SFC_LW_DWN_phase1")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - \nALLSKY_SFC_LW_DWN_phase1")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(RADIATION_phase1 = factor(x = kmcluster$cluster, levels = c("3","1","2"),
                                   labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(ALLSKY_SFC_LW_DWN_sum_0.45 = seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_0.45),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_0.45),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$ALLSKY_SFC_LW_DWN_sum_0.45),max(soybean_data$ALLSKY_SFC_LW_DWN_sum_0.45),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER PRECTOT_sum_70. ####

dados2 <- soybean_data |>
  dplyr::select(PRECTOT_sum_70.)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette") +
  labs(title = "Silhouette method - PRECTOT_70+")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 10) +
  labs(title = "Wss method - PRECTOT_70+")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(PRECTOT_phase3 = factor(x = kmcluster$cluster, levels = c("3","2","1"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(PRECTOT_sum_70. = seq(min(soybean_data$PRECTOT_sum_70.),max(soybean_data$PRECTOT_sum_70.),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$PRECTOT_sum_70.),max(soybean_data$PRECTOT_sum_70.),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER PRECTOT_sum_45.70 ####

dados2 <- soybean_data |>
  dplyr::select(PRECTOT_sum_45.70)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - PRECTOT_45-70")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - PRECTOT_45-70")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(PRECTOT_phase2 = factor(x = kmcluster$cluster, levels = c("2","1","3"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(PRECTOT_sum_45.70 = seq(min(soybean_data$PRECTOT_sum_45.70),max(soybean_data$PRECTOT_sum_45.70),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$PRECTOT_sum_45.70),max(soybean_data$PRECTOT_sum_45.70),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))


### CLUSTER PRECTOT_sum_0.45 ####

dados2 <- soybean_data |>
  dplyr::select(PRECTOT_sum_0.45)

# Definição da semente para garantir reprodutibilidade
set.seed(123)

# Aplicação do método de sinueta
g1 <- factoextra::fviz_nbclust(dados2, kmeans, method = "silhouette", k.max = 9) +
  labs(title = "Silhouette method - PRECTOT_0-45")

# Aplicação do método de cotevelo
g2 <- factoextra::fviz_nbclust(dados2, kmeans, method = "wss", k.max = 9) +
  labs(title = "Wss method - PRECTOT_0-45")

ggpubr::ggarrange(g1,g2)

set.seed(123)
kmcluster <- fdm2id::KMEANS(d = dados2, k = 3)
kmcluster$centers
soybean_data <- soybean_data |> 
  mutate(PRECTOT_phase1 = factor(x = kmcluster$cluster, levels = c("2","1","3"),
                                 labels = c("Low","Middle","High")))

# fazendo predição para delimitar os limites de cada nível dos clusters
predicao <- data.frame(PRECTOT_sum_0.45 = seq(min(soybean_data$PRECTOT_sum_0.45),max(soybean_data$PRECTOT_sum_0.45),1),
                       cluster = predict(kmcluster, seq(min(soybean_data$PRECTOT_sum_0.45),max(soybean_data$PRECTOT_sum_0.45),1)))
# verificando os limites dos clusters
predicao |> group_by(cluster) |> summarise_all(.funs = c(min, max, mean, length))



write.csv(x = soybean_data, file = "Input/soybean_data-v3.csv", row.names = FALSE)
