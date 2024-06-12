
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

# average profile graph of the Soil types
# perfil médio dos tipos Solos
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Solo), fun = "mean", geom = "line") + 
  theme_light()

# average profile graph of the Harvest Cycle
# perfil médio do Ciclo de Colheita
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Cycle4), fun = "mean", geom = "line") + 
  theme_light()

# box-plot in panels by Soil and Cycle
# gráfico em painéis por Solo e Ciclo
ggplot(soybean_data, aes(x = Caracteristica, y = kgha)) + 
  geom_boxplot() + facet_grid(Solo~Cycle4) +
  theme_light()

# coefficient of variation per experiment
# coeficiente de variacao por experimento
cv = soybean_data |> group_by(ID) |> summarise(goeveg::cv(kgha))


# Decision Tree Model -----------------------------------------------------

set.seed(2233)
colnames(dados_soja)[sample(10:33,12)]

# accuracy 78,52%
model <- rpart(formula = kghaCluster ~ Solo + Cycle4 +
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


# production cost indicator
indicator <- function(x) (4641.71/mean(x))


# average productivity for each branch of the tree
# produtividade média por caminho da árvore

# Average Productivity for the High branch (23%)
soybean_data |>
  filter(RH2M_phase2 == "Low") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Middle branch (32%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the High branch (2%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |>
  filter(Cycle4 == "Medio") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Middle branch (3%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "High") |>
  filter(Cycle4 == "Tardio") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Middle branch (1%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Solo == "Latossolo") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Low branch (2%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Low" | PRECTOT_phase1 == "High") |>
  filter(RADIATION_phase3 == "High") |>
  filter(PRECTOT_phase3 == "Middle") |>
  filter(Solo == "Plintossolo") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Middle branch (12%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Solo == "Latossolo") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Low" | T2M_MIN_phase3 == "High") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Middle branch (3%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Solo == "Latossolo") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Precoce") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Low branch (6%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Solo == "Latossolo") |>
  filter(RADIATION_phase3 == "Low" | RADIATION_phase3 == "Middle") |>
  filter(T2M_MIN_phase3 == "Middle") |>
  filter(Cycle4 == "Super Precoce" | Cycle4 == "Medio") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Low branch (4%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Solo == "Latossolo") |>
  filter(RADIATION_phase3 == "High") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))

# Average Productivity for the Low branch (12%)
soybean_data |>
  filter(RH2M_phase2 == "Middle" | RH2M_phase2 == "High") |>
  filter(PRECTOT_phase1 == "Middle") |>
  filter(Solo == "Plintossolo") |>
  select(kgha) |>
  summarise_all(.funs = c(mean, indicator))
