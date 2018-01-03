###############################################################################
###############################################################################
################ Análise Descritiva de Ambos os trabalhos #####################
###############################################################################
###############################################################################

### Leitura dos dados

vitamin.df <- read_csv2("Dados/vitamina.csv", col_names = T)

vitamin.df %<>%
  select(-obs) %>%
  mutate(x0 = as.factor(x0),
         x2 = as.factor(x2),
         x3 = as.factor(x3))

vitamin.surv <- Surv(vitamin.df$tempo, vitamin.df$cens)

### Utilizando apenas a variável tempo


vitamin.km <- survfit(formula = vitamin.surv ~ 1)

vitamin.kmt <- tidy(vitamin.km)

ggsurvplot(fit = vitamin.km, data = vitamin.df,
           legend = "none",
           ggtheme = theme_bw(), conf.int = F)+
  xlab("Tempo")+ylab("S(t) Estimada")

##### 
ggsurvplot(fit = vitamin.km, fun = "cumhaz",
           data = vitamin.df, conf.int = F)

### 

