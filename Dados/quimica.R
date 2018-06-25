library(data.table)
library(tidyverse)
library(magrittr)
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)


vari <- c("Matrícula", "Entrada", "Ulti_Per", "Tempo", "Status", "Sexo", "Turno", "Curso",
          "Escola", "Ingresso", "Idade", "Idade2", "Raca")

d <- fread("Downloads/Banco de dados - QuimIn.csv")

d <- d[, vari, with=F]
d <- as_tibble(d)

d.surv <- Surv(d$Tempo, d$Status)

d.km <- survfit(formula = d.surv ~ 1)

ggsurvplot(fit = d.km, data = d, legend = "none",
                 ggtheme = theme_bw(), conf.int = F)+
    xlab("Tempo")+ylab("S(t) Estimada")


d.km1 <- survfit(formula = d.surv ~ Sexo, data = d)

ggsurvplot(fit = d.km1, data = d, legend = "right",
           legend.title = "Tratamento", legend.labs = c("Masculino",
                                                        "Feminino"),
           ggtheme = theme_bw(), conf.int = F, pval = T, log.rank.weights = "n")+
  xlab("Tempo")+ylab("S(t) Estimada")


d.km2 <- survfit(formula = d.surv ~ Ingresso, data = d)

ggsurvplot(fit = d.km2, data = d, legend = "right",
           legend.title = "Tratamento", legend.labs = c("Vestibular",
                                                        "ENEM"),
           ggtheme = theme_bw(), conf.int = F, pval = T, log.rank.weights = "n")+
  xlab("Tempo")+ylab("S(t) Estimada")



d.km3 <- survfit(formula = d.surv ~ Turno, data = d)

ggsurvplot(fit = d.km3, data = d, legend = "right",
           legend.title = "Tratamento", legend.labs = c("Diurno",
                                                        "Noturno"),
           ggtheme = theme_bw(), conf.int = F, pval = T, log.rank.weights = "n")+
  xlab("Tempo")+ylab("S(t) Estimada")




d.km4 <- survfit(formula = d.surv ~ Raca, data = d)

ggsurvplot(fit = d.km4, data = d, legend = "right",
           legend.title = "Tratamento", 
           ggtheme = theme_bw(), conf.int = F, pval = T, log.rank.weights = "n")+
  xlab("Tempo")+ylab("S(t) Estimada")
d.km5 <- survfit(formula = d.surv ~ Escola, data = d)

ggsurvplot(fit = d.km5, data = d, legend = "right",
           legend.title = "Tratamento", 
           ggtheme = theme_bw(), conf.int = F, pval = T, log.rank.weights = "n")+
  xlab("Tempo")+ylab("S(t) Estimada")
