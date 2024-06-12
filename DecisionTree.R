
rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rpart.plot)
library(caret)


# Data Loading ------------------------------------------------------------

soybean_data <- read.csv(file = "Input/soybean_data-v3.csv")

# transforming character variables into categorical ones
# transformando variáveis texto em categóricas
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
         Solo = as.factor(Solo), Cultivar = as.factor(Cultivar), 
         Caracteristica = as.factor(Caracteristica), 
         Cycle3 = factor(x = Cycle3, levels = c("Precoce","Medio","Tardio")),
         Cycle4 = factor(x=Cycle4, levels=c("Super Precoce","Precoce","Medio","Tardio")),
         Clima = factor(x=Clima, levels=c("ModerateLaNina","WeakLaNina","Neutral")) )

# creating data frame with only categorical covariates
dados_soja <- soybean_data |>
  dplyr::select(ID, kgha, kghaCluster, Solo, Cycle3, Cycle4, Caracteristica, Clima, Cultivar,
                RH2M_phase3, RH2M_phase2, RH2M_phase1, T2M_MAX_phase3, T2M_MAX_phase2, T2M_MAX_phase1,
                T2M_phase3, T2M_phase2, T2M_phase1, T2M_MIN_phase3, T2M_MIN_phase2, T2M_MIN_phase1,
                T2MDEW_phase2, T2MDEW_phase1, T2MDEW_phase3, WS2M_phase3, WS2M_phase2, WS2M_phase1,
                RADIATION_phase3, RADIATION_phase2, RADIATION_phase1,
                PRECTOT_phase3, PRECTOT_phase2, PRECTOT_phase1)
#write.csv(x = dados_soja, file = "dados_soja.csv")


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

##### Densidade por tipo de SOLO
sample_size = soybean_data %>% group_by(Solo) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Solo") +
  xlab("")


##### Densidade por CLIMA
sample_size = soybean_data %>% group_by(Clima) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Clima, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Clima)) +
  geom_violin(width=1.4) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Característica Climática") +
  xlab("")


##### Densidade por CICLO
sample_size = soybean_data %>% group_by(Ciclo4) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Ciclo4)) +
  geom_violin(width=1.4) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Característica Climática") +
  xlab("")


##### Densidade por CICLO e CLIMA
sample_size = soybean_data %>% group_by(Ciclo4, Clima) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", Clima, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano e Solo") +
  xlab("")


##### perfil médio dos tipos Solos
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Solo), fun = "mean", geom = "line") + 
  theme_light()


##### perfil médio do Ciclo de Colheita
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Ciclo4), fun = "mean", geom = "line") + 
  theme_light()


# boxplot dos 95 níveis de Cultivar
ggplot(soybean_data, aes(x = Cultivar, y = kgha)) + 
  geom_boxplot() + theme_light() + theme(axis.text.x = element_blank())


# gráfico em painéis por Solo e Ciclo
ggplot(soybean_data, aes(x = Caracteristica, y = kgha)) + 
  geom_boxplot() + facet_grid(Solo~Ciclo4) +
  theme_light()


# coeficiente de variacao por experimento
cv = soybean_data |> group_by(id) |> summarise(cv(kgha))






# Decision Tree Model -----------------------------------------------------



#### set.seed(2024) -- Acurácia de 77,85% -- TEM SOLO ----

set.seed(2024)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 74,5%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   RH2M_phase2+T2M_MAX_phase2+T2MDEW_phase2+WS2M_phase2+
                   PRECTOT_phase3+T2M_phase1+PRECTOT_phase1+T2M_MIN_phase2+
                   WS2M_phase3+RADIATION_phase2+T2MDEW_phase1+T2M_MIN_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 77,85%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   RH2M_phase2+T2M_MAX_phase2+T2MDEW_phase2+WS2M_phase2+
                   PRECTOT_phase3+T2M_phase1+PRECTOT_phase1+T2M_MIN_phase2+
                   WS2M_phase3+RADIATION_phase2+T2MDEW_phase1+T2M_MIN_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2


5
#### set.seed(123) -- Acurácia de 76,51% -- TEM SOLO ----

set.seed(123)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 76,51%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   T2MDEW_phase3+RADIATION_phase3+T2MDEW_phase1+RH2M_phase1+
                   T2M_MIN_phase3+WS2M_phase1+T2M_MIN_phase2+T2M_MAX_phase2+
                   T2M_MAX_phase3+PRECTOT_phase3+T2M_MAX_phase1+T2M_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 76,51%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   T2MDEW_phase3+RADIATION_phase3+T2MDEW_phase1+RH2M_phase1+
                   T2M_MIN_phase3+WS2M_phase1+T2M_MIN_phase2+T2M_MAX_phase2+
                   T2M_MAX_phase3+PRECTOT_phase3+T2M_MAX_phase1+T2M_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2



#### set.seed(22) -- Acurácia de 78,52% -- TEM SOLO ----

set.seed(22)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 74,83%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   T2M_MAX_phase1+T2M_phase1+T2M_MIN_phase3+T2M_MIN_phase1+
                   WS2M_phase2+T2M_MAX_phase2+T2M_MIN_phase2+RH2M_phase1+
                   WS2M_phase3+T2M_MAX_phase3+PRECTOT_phase3+T2M_phase2,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 78,52%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   T2M_MAX_phase1+T2M_phase1+T2M_MIN_phase3+T2M_MIN_phase1+
                   WS2M_phase2+T2M_MAX_phase2+T2M_MIN_phase2+RH2M_phase1+
                   WS2M_phase3+T2M_MAX_phase3+PRECTOT_phase3+T2M_phase2,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2



#### set.seed(2020) -- Acurácia de 78,52% -- TEM SOLO ----

set.seed(2020)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 75,84%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   T2M_MIN_phase1+PRECTOT_phase2+PRECTOT_phase3+RH2M_phase3+
                   WS2M_phase2+T2M_MAX_phase3+T2M_MIN_phase3+T2M_MAX_phase1+
                   RADIATION_phase1+T2MDEW_phase2+T2M_phase2+T2MDEW_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 78,52%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   T2M_MIN_phase1+PRECTOT_phase2+PRECTOT_phase3+RH2M_phase3+
                   WS2M_phase2+T2M_MAX_phase3+T2M_MIN_phase3+T2M_MAX_phase1+
                   RADIATION_phase1+T2MDEW_phase2+T2M_phase2+T2MDEW_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2



#### set.seed(725) -- Acurácia de 78,52% -- TEM SOLO ----

set.seed(725)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 75,17%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   RH2M_phase1+T2M_phase1+RH2M_phase2+PRECTOT_phase2+
                   RADIATION_phase3+WS2M_phase3+T2M_MIN_phase1+T2M_MAX_phase2+
                   PRECTOT_phase1+T2M_MIN_phase2+WS2M_phase2+T2M_phase3,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 78,52%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   RH2M_phase1+T2M_phase1+RH2M_phase2+PRECTOT_phase2+
                   RADIATION_phase3+WS2M_phase3+T2M_MIN_phase1+T2M_MAX_phase2+
                   PRECTOT_phase1+T2M_MIN_phase2+WS2M_phase2+T2M_phase3,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2



#### set.seed(13) -- Acurácia de 77,85% -- TEM SOLO ----

set.seed(13)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 77,18%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   PRECTOT_phase1+RH2M_phase1+T2M_MAX_phase2+T2M_MIN_phase3+
                   T2MDEW_phase2+T2M_MAX_phase1+WS2M_phase3+T2M_MAX_phase3+
                   WS2M_phase1+RADIATION_phase2+T2M_MIN_phase1+T2M_phase2,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 77,85%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   PRECTOT_phase1+RH2M_phase1+T2M_MAX_phase2+T2M_MIN_phase3+
                   T2MDEW_phase2+T2M_MAX_phase1+WS2M_phase3+T2M_MAX_phase3+
                   WS2M_phase1+RADIATION_phase2+T2M_MIN_phase1+T2M_phase2,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2




#### set.seed(2233) -- Acurácia de 78,52% -- TEM SOLO ----

set.seed(2233)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 74,16%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   RH2M_phase2+T2M_MAX_phase1+PRECTOT_phase3+RH2M_phase1+
                   WS2M_phase2+WS2M_phase3+T2M_MIN_phase3+RADIATION_phase2+
                   PRECTOT_phase2+PRECTOT_phase1+RADIATION_phase3+T2MDEW_phase3,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 78,52%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   RH2M_phase2+T2M_MAX_phase1+PRECTOT_phase3+RH2M_phase1+
                   WS2M_phase2+WS2M_phase3+T2M_MIN_phase3+RADIATION_phase2+
                   PRECTOT_phase2+PRECTOT_phase1+RADIATION_phase3+T2MDEW_phase3,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2



#### set.seed(597) -- Acurácia de 78,52% -- TEM SOLO ----

set.seed(597)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 74,83%
mod1.dt <- rpart(formula = kghaCluster ~ Solo + Cycle3 +
                   T2M_MIN_phase3+PRECTOT_phase3+PRECTOT_phase2+T2M_MIN_phase2+
                   T2MDEW_phase2+T2MDEW_phase3+WS2M_phase2+RADIATION_phase1+
                   T2M_phase1+T2M_phase2+T2M_MAX_phase3+T2M_MAX_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod1.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt1 <- predict(object = mod1.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt1 <- confusionMatrix(data = predictions.dt1, reference = test.dt$kghaCluster);CM.dt1

# accuracy 78,52%
mod2.dt <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
                   T2M_MIN_phase3+PRECTOT_phase3+PRECTOT_phase2+T2M_MIN_phase2+
                   T2MDEW_phase2+T2MDEW_phase3+WS2M_phase2+RADIATION_phase1+
                   T2M_phase1+T2M_phase2+T2M_MAX_phase3+T2M_MAX_phase1,
                 data = train.dt, method = "class")
# decision tree plot
# gráfico da árvore de decisão
rpart.plot(x = mod2.dt, type = 5)
# decision tree prediction
# predição da árvore de decisão
predictions.dt2 <- predict(object = mod2.dt, newdata = test.dt, type = "class")
# confusion matrix
# matriz de confusão
CM.dt2 <- confusionMatrix(data = predictions.dt2, reference = test.dt$kghaCluster);CM.dt2

