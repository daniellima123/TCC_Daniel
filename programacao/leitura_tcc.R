library(tidyverse)
library(survminer)
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

km <- survfit(Surv(d$tempo, d$censura) ~ idade, data = d)
km <- survfit(Surv(d$tempo, d$censura) ~ tratamento, data = d)
ggsurvplot(km, conf.int = F, data = d, legend = c(0.85, 0.85), ggtheme = theme_bw(),
           palette = c('black', 'red'), legend.title = 'Tratamento', legend.labs = c('Placebo',
                                                                               'Vitamina A'))
ggsurvplot(km, conf.int = F, data = d, legend = c(0.85, 0.85), ggtheme = theme_bw(),
           palette = c('black', 'red'), legend.title = 'Idade', legend.labs = c('< 24 meses',
                                                                               '\u2265 24 meses'))+
  xlab('Tempo')+ylab('S(t)')
lines(sort(d$tempo), sobrev(sort(d$tempo), mu_semestre, beta_semestre),
      col = 'red')
lines(sort(d$Tempo), sobrev(sort(d$Tempo), mu_ano, beta_ano),
      col = 'blue')
legend(1, 0.4, legend = c('Semestral', 'Anual'),
       col = c('red', 'blue'), lty = 1)
ggsurvplot(km, conf.int = F, data = d, fun = 'cumhaz', legend = 'none', color = 'red', ggtheme = theme_bw())+
  xlab('Tempo')+ylab('H(t)')


qplot(d$Tempo) +
  scale_x_continuous(labels = 0:12, breaks = 0:12) +
  scale_color_manual(values = c('black'))+
  theme_bw()+xlab('Tempo')+ylab('Frequeência')

