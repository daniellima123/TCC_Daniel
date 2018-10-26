library(tidyverse)
library(survival)
library(data.table)
source('programacao/leitura.R')
d <- read_vitamin('Dados/vitamina.csv')
d <- read_quimica('Dados/Banco de dados - QuimIn.csv')
setDT(d)
# Vitamina
intervals <- c(4, 14, 18, 23, 29, 37, 48, 61, 73, 90, 108, 126, 185)

# Quimica
intervals <- seq(0, 12)
#intervals <- seq(0, 12, 2)
int_df <- data.frame(inf = intervals[-length(intervals)],
                     sup = intervals[-1])

km <- survfit(Surv(d$tempo, d$censura) ~ 1)
plot(km, conf.int = F)
lines(sort(d$tempo), sobrev(sort(d$tempo), mu_semestre, beta_semestre),
      col = 'red')
lines(sort(d$Tempo), sobrev(sort(d$Tempo), mu_ano, beta_ano),
      col = 'blue')
legend(1, 0.4, legend = c('Semestral', 'Anual'),
       col = c('red', 'blue'), lty = 1)
