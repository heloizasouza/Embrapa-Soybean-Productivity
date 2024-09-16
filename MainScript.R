
rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rpart.plot)
library(caret)

# Data Loading ------------------------------------------------------------

soybean_data <- read.csv(file = "Input/soybean_data-v3.csv")

# transforming text variables into categorical ones, with levels in Portuguese
# transformando variáveis texto em categóricas, com níveis em português
dados2 <- soybean_data |>
  select(kgha,kghaCluster,Solo,Cycle4,RH2M_mean_45.70, T2M_MAX_mean_0.45,
         PRECTOT_sum_70., RH2M_mean_0.45, WS2M_mean_45.70, WS2M_mean_70.,
         T2M_MIN_mean_70., ALLSKY_SFC_LW_DWN_sum_70., ALLSKY_SFC_LW_DWN_sum_45.70,
           RH2M_phase2,T2M_MAX_phase1,PRECTOT_phase3,RH2M_phase1,
         PRECTOT_sum_0.45, PRECTOT_sum_45.70, T2MDEW_mean_70.,
           WS2M_phase2,WS2M_phase3,T2M_MIN_phase3,RADIATION_phase2,
           PRECTOT_phase2,PRECTOT_phase1,RADIATION_phase3,T2MDEW_phase3) |>
  mutate(kghaCluster = factor(x = kghaCluster,labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         Solo = as.factor(Solo),
         Ciclo = factor(Cycle4, levels=c("Super Precoce","Precoce","Medio","Tardio")),
         RH2M_fase2 = factor(x = RH2M_phase2, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         T2M_MIN_fase3 = factor(x = T2M_MIN_phase3, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         PRECTOT_fase3 = factor(x = PRECTOT_phase3,labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         PRECTOT_fase1 = factor(x = PRECTOT_phase1,labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         RADIATION_fase3 = factor(x = RADIATION_phase3, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         T2M_MAX_fase1 = factor(x = T2M_MAX_phase1, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         RH2M_fase1 = factor(x = RH2M_phase1, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         WS2M_fase2 = factor(x = WS2M_phase2, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         WS2M_fase3 = factor(x = WS2M_phase3, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         RADIATION_fase2 = factor(x = RADIATION_phase2, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         PRECTOT_fase2 = factor(x = PRECTOT_phase2, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         T2MDEW_fase3 = factor(x = T2MDEW_phase3, labels = c("Baixo", "Médio", "Alto"),levels = c("Low","Middle","High")),
         )

# transforming text variables into categorical ones, with levels in English
# transformando variáveis texto em categóricas, com níveis em inglês
soybean_data <- soybean_data |>
  mutate(kghaCluster = factor(x = kghaCluster,levels = c("Low","Middle","High")),
         RH2M_phase3 = factor(x = RH2M_phase3,levels = c("Low","Middle","High")),
         RH2M_phase2 = factor(x = RH2M_phase2,levels = c("Low","Middle","High")),
         RH2M_phase1 = factor(x = RH2M_phase1,levels = c("Low","Middle","High")),
         T2M_MAX_phase3 = factor(x = T2M_MAX_phase3,levels = c("Low","Middle","High")),
         T2M_phase3 = factor(x = T2M_phase3,levels = c("Low","Middle","High")),
         T2M_MIN_phase3 = factor(x = T2M_MIN_phase3,levels = c("Low","Middle","High")),
         T2M_MIN_phase1 = factor(x = T2M_MIN_phase1,levels = c("Low","Middle","High")),
         T2MDEW_phase2 = factor(x = T2MDEW_phase2,levels = c("Low","Middle","High")),
         T2MDEW_phase3 = factor(x = T2MDEW_phase3,levels = c("Low","Middle","High")),
         WS2M_phase3 = factor(x = WS2M_phase3,levels = c("Low","Middle","High")),
         WS2M_phase1 = factor(x = WS2M_phase1,levels = c("Low","Middle","High")),
         RADIATION_phase3 = factor(x = RADIATION_phase3,levels = c("Low","Middle","High")),
         RADIATION_phase2 = factor(x = RADIATION_phase2,levels = c("Low","Middle","High")),
         RADIATION_phase1 = factor(x = RADIATION_phase1,levels = c("Low","Middle","High")),
         PRECTOT_phase3 = factor(x = PRECTOT_phase3,levels = c("Low","Middle","High")),
         PRECTOT_phase2 = factor(x = PRECTOT_phase2,levels = c("Low","Middle","High")),
         PRECTOT_phase1 = factor(x = PRECTOT_phase1,levels = c("Low","Middle","High")),
         T2M_MAX_phase2 = factor(x = T2M_MAX_phase2,levels = c("Low","High")),
         T2M_MAX_phase1 = factor(x = T2M_MAX_phase1,levels = c("Low","High")),
         T2M_phase2 = factor(x = T2M_phase2,levels = c("Low","High")),
         T2M_phase1 = factor(x = T2M_phase1,levels = c("Low","High")),
         T2M_MIN_phase2 = factor(x = T2M_MIN_phase2,levels = c("Low","High")),
         T2MDEW_phase1 = factor(x = T2MDEW_phase1,levels = c("Low","High")),
         WS2M_phase2 = factor(x = WS2M_phase2,levels = c("Low","High")),
         Solo = as.factor(Solo),
         Soil = factor(Solo, levels = c("Latossolo", "Plintossolo"), labels = c("Oxisols", "Plinthosol")),
         Cultivar = as.factor(Cultivar), 
         Caracteristica = as.factor(Caracteristica),
         Cycle3 = factor(x = Cycle3, levels = c("Precoce","Medio","Tardio"), labels = c("Early", "Medium", "Late")),
         Cycle4 = factor(x=Cycle4, levels=c("Super Precoce","Precoce","Medio","Tardio"), labels = c("Super Early", "Early", "Medium", "Late")),
         Clima = factor(x=Clima, levels=c("ModerateLaNina","WeakLaNina","Neutral")) )

# creating data frame with only categorical covariates
dados_soja <- soybean_data |>
  dplyr::select(ID, Local, kgha, kghaCluster, Soil, Cycle3, Cycle4, Caracteristica, Clima, Cultivar,
                RH2M_phase3, RH2M_phase2, RH2M_phase1, T2M_MAX_phase3, T2M_MAX_phase2, T2M_MAX_phase1,
                T2M_phase3, T2M_phase2, T2M_phase1, T2M_MIN_phase3, T2M_MIN_phase2, T2M_MIN_phase1,
                T2MDEW_phase2, T2MDEW_phase1, T2MDEW_phase3, WS2M_phase3, WS2M_phase2, WS2M_phase1,
                RADIATION_phase3, RADIATION_phase2, RADIATION_phase1,
                PRECTOT_phase3, PRECTOT_phase2, PRECTOT_phase1)


# spliting the dataset into training and testing
# separando o conjunto de dados em treino e teste
set.seed(123)
trainId <- sample(x = 1:nrow(dados_soja), size = nrow(dados_soja)*0.7)

train.dt <- dados_soja |>
  slice(trainId) 

test.dt <- dados_soja |>
  slice(-trainId) 

attach(dados_soja)


# Descriptive Analysis ----------------------------------------------------

attach(soybean_data)

# Density Graph by soil type
# Densidade por tipo de SOLO
sample_size = soybean_data %>% group_by(Soil) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Soil, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Soil)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Soybean productivity density by Soil") +
  xlab("") + ylab("")


# average profile graph of the Soil types
# perfil médio dos tipos Solos
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Soil), fun = "mean", geom = "line") + 
  theme_light() +
  labs(x = "Year", y = "Soybean Productivity (kg/ha)")


# average profile graph of the Harvest Cycle
# perfil médio do Ciclo de Colheita
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Cycle4), fun = "mean", geom = "line") + 
  theme_light() +
  labs(x = "Year", y = "Soybean Productivity (kg/ha)", colour = "Harvest cycle")


# box-plot in panels by Soil and Cycle
# gráfico em painéis por Solo e Ciclo
ggplot(soybean_data, aes(x = Caracteristica, y = kgha)) + 
  geom_boxplot() + facet_grid(Soil~Cycle4) +
  theme_light() +
  labs(x = "Climatic Feature", y = "Soybean Productivity (kg/ha)")

# box plot dos Locais de plantio
ggplot(soybean_data, aes(x = Local, y = kgha, fill = Local)) +
  geom_boxplot() + 
  geom_violin(trim = T, alpha = 0.5) + 
  theme_light() + labs(x="", y = "Produtividade de soja (kg/ha)") +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 315, vjust=0 , hjust=0.1),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 11),
        legend.position = "top")

# gráfico em painéis por Solo e Ciclo Vegetativo
ggplot(dados2, aes(x = Solo, y = kgha, fill = Ciclo)) + 
  geom_boxplot() + 
  facet_wrap(vars(Ciclo)) + 
  geom_violin(trim = T, alpha = 0.5) + 
  theme_light() + 
  labs(x="", y = "Produtividade de soja (kg/ha)", fill = "Ciclo vegetativo") +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "top")

ggplot(dados2, aes(x = kghaCluster, y = kgha, fill = kghaCluster)) +
  geom_boxplot() +
  theme_light() + 
  labs(x="", y = "Produtividade de soja (kg/ha)", fill = "Cluster") +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top")

ggplot(dados2, aes(x = RH2M_fase2, y = RH2M_mean_45.70, fill = RH2M_fase2)) +
  geom_boxplot() +
  theme_light() + 
  labs(x="", y = "Produtividade de soja (kg/ha)", fill = "Cluster") +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top")


# coefficient of variation per experiment
# coeficiente de variacao por experimento
cv = soybean_data |> group_by(ID) |> summarise(goeveg::cv(kgha))


# Decision Tree Model -----------------------------------------------------

set.seed(2233)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 78,52%
model <- rpart(formula = kghaCluster ~ Soil + Cycle4 +
                   RH2M_phase2+T2M_MAX_phase1+PRECTOT_phase3+RH2M_phase1+
                   WS2M_phase2+WS2M_phase3+T2M_MIN_phase3+RADIATION_phase2+
                   PRECTOT_phase2+PRECTOT_phase1+RADIATION_phase3+T2MDEW_phase3,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = model, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions <- predict(object = model, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM <- confusionMatrix(data = predictions, reference = test.dt$kghaCluster);CM


# Average Productivity ----------------------------------------------------


# tabelas auxiliares de resultados dos indicadores
tab.cenarios <- data.frame(Scenario = 1:11,
                           kgha_Cluster = c(rep("Low",4),rep("Middle",5),rep("High",2)),
                           Mean_Yield = rep(NA, 11),
                           cv = rep(NA, 11),
                           n = rep(NA, 11),
                           GPR = rep(NA,11),
                           RPR = rep(NA,11))

tab.clusters <- data.frame(kgha_Cluster = c("Low","Middle","High"),
                      Mean_Yield = rep(NA, 3),
                      cv = rep(NA, 3),
                      n = rep(NA, 3),
                      GPR = rep(NA,3),
                      RPR = rep(NA,3))


# average productivity for each branch of the tree
# produtividade média por caminho da árvore

# Average Productivity for the High branch (23%) - SCENARIO 11
tab.cenarios[11,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Low") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the High branch (2%) - SCENARIO 10
tab.cenarios[10,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |> 
  filter(Cycle4 == "Medium") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# data frames auxiliares de cada cenário para alimentar o 
# cálculo dos resultados das as tabelas de indicadores
# SCENARIO 11
df1 = soybean_data |>
  filter(RH2M_phase2 == "Low") |>
  select(kgha)
# SCENARIO 10
df2 = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |> 
  filter(Cycle4 == "Medium") |>
  select(kgha) 

df.aux <- rbind(df1, df2)

tab.clusters[3,2:4] = df.aux |>
  summarise_all(.funs = c(mean, goeveg::cv, length))



# Average Productivity for the Middle branch (3%) - SCENARIO 9 
tab.cenarios[9,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |>
  filter(Cycle4 == "Late") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Middle branch (32%) - SCENARIO 8
tab.cenarios[8,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Middle branch (1%) - SCENARIO 7
tab.cenarios[7,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Soil == "Oxisols") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Middle branch (12%) - SCENARIO 6
tab.cenarios[6,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Low" | T2M_MIN_phase3 == "High") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Middle branch (3%) - SCENARIO 5
tab.cenarios[5,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Early") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# data frames auxiliares de cada cenário para alimentar o 
# cálculo dos resultados das as tabelas de indicadores
# SCENARIO 9
df1 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |>
  filter(Cycle4 == "Late") |>
  select(kgha)
# SCENARIO 8
df2 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  select(kgha)

df.aux <- rbind(df1, df2)

# SCENARIO 7
df1 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Soil == "Oxisols") |>
  select(kgha)
# SCENARIO 6
df2 = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Low" | T2M_MIN_phase3 == "High") |>
  select(kgha)

df.aux <- rbind(df.aux, df1, df2)

# SCENARIO 5
df1 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Early") |>
  select(kgha)

df.aux <- rbind(df.aux, df1)

tab.clusters[2,2:4] = df.aux |>
  summarise_all(.funs = c(mean, goeveg::cv, length))



# Average Productivity for the Low branch (6%) - SCENARIO 4
tab.cenarios[4,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Super Early" | Cycle4 == "Medium") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Low branch (4%) - SCENARIO 3
tab.cenarios[3,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "High") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Low branch (2%) - SCENARIO 2
tab.cenarios[2,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Soil == "Plinthosol") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# Average Productivity for the Low branch (12%) - SCENARIO 1
tab.cenarios[1,3:5] = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Plinthosol") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, goeveg::cv, length))


# data frames auxiliares de cada cenário para alimentar o 
# cálculo dos resultados das as tabelas de indicadores
# SCENARIO 4
df1 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Super Early" | Cycle4 == "Medium") |>
  select(kgha)
# SCENARIO 3
df2 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Oxisols") |>
  filter(RADIATION_phase3 == "High") |>
  select(kgha)

df.aux <- rbind(df1, df2)

# SCENARIO 2
df1 <- soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Soil == "Plinthosol") |>
  select(kgha)
# SCENARIO 1
df2 = soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Soil == "Plinthosol") |>
  select(kgha)

df.aux <- rbind(df.aux, df1, df2)

tab.clusters[1,2:4] = df.aux |>
  summarise_all(.funs = c(mean, goeveg::cv, length))



# calculando os indicadores por cluster
tab.clusters$GPR = ((tab.clusters$Mean_Yield/min(tab.clusters$Mean_Yield))-1)*100
tab.clusters$RPR = ((tab.clusters$cv/min(tab.clusters$cv))-1)*100

# calculando os indicadores por cenários da árvore de decisão
tab.cenarios$GPR = ((tab.cenarios$Mean_Yield/min(tab.cenarios$Mean_Yield))-1)*100
tab.cenarios$RPR = ((tab.cenarios$cv/min(tab.cenarios$cv))-1)*100


# exportanto em arquivo Excel as tabelas de resultados
openxlsx::write.xlsx(list("Clusters" = tab.clusters,"Cenarios" = tab.cenarios),
                     file = "./Indicadores.xlsx")
