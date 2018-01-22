###############################################################################
############################ Análise TCC ######################################
###############################################################################

library(tidyverse)
library(magrittr)
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)


############################ Análise Exploratória #############################

vitamin.df <- read_csv2("Dados/vitamina.csv", col_names = T)

head(vitamin.df)

## Calculando Kaplan

vitamin.df %<>%
  select(-obs) %>%
  mutate(x0 = as.factor(x0),
         x2 = as.factor(x2),
         x3 = as.factor(x3))

vitamin.surv <- Surv(vitamin.df$tempo, vitamin.df$cens)

vitamin.km <- survfit(formula = vitamin.surv ~ 1)

summary(vitamin.km)
vitamin.kmt <- tidy(vitamin.km)

## Calculando o risco acumulado

vitamin.kmt %<>%
  mutate(risk.cum = -log(estimate)) 
      
autoplot(vitamin.km)

plot(vitamin.km)
plot(x = vitamin.kmt$time, y = vitamin.kmt$risk.cum, type = "s")





## verossimilhan?a da log logistica

p_t <- function(t, mu, beta){

  val = 1/(1 + (t/mu)^beta) - 1/(1 + ((t+1)/mu)^beta)
  return(val)
}




