

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
write.csv(x = soybean_data, file = "soybean_data-v2.csv")
