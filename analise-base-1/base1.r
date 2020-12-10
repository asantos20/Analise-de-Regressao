setwd("~/Análise de Regressão") # Setando diretório

library(readxl) ## Biblioteca para ler arquivos .xlsx
library(ggplot2) ## gráficos
library(tidyverse) ## caso necessite trabalhar a base de dados
library(ggpubr) ## plotar o modelo de regressão e temas
options(scipen = 999) ## Retirar a notação científica no R
#################################################################################################

## A tabela a seguir mostra a relação entre 
#a quilometragem de um carro usado e seu respectivo
#preço de venda.

dados = read_excel("Tabelas.xlsx", col_names = T, sheet = "F") ## importando os dados

View(dados) ## visualizar a base de dados
names(dados) ## variáveis

########################################################################

x = dados$`Quilomentagem (1.000 km)` ## objeto X
y = dados$`Preço de venda (Dezena de Euros)` ## objeto Y

summary(x)
########################################################################
## Gráfico para análise a priori
dados %>%
  ggplot(aes(x,y)) +
  geom_point() +
  labs(x = 'Quilometragem (1000 Km)', y = 'Preço de venda (Dezenas de Euros)')

########################################################################

cor.test(y, x) ## teste de correlação para provar se há e se é significativa.

result = lm(y ~ x) ## modelo linear

summary(result) ## sumário do modelo

anova(result) ## Análise de variância

confint.lm(result) ## intervalo de confiança com 95% para B0 e B1.


##################################################################################################

## Gráfico final com modelo e reta traçados
dados %>%
  ggplot(aes(x,y)) +
  geom_point() +
  labs(x = 'Quilometragem (1000 Km)', y = 'Preço de venda (Dezenas de euros)') + 
  geom_smooth(method = "lm", se = F) +
  stat_regline_equation(aes(x, y), label.x.npc = 0.75) ## label.x.npc (posição da formula em X)