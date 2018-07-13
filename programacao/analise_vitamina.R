## Importa as funções
source("programacao/funcoes.R")
source("programacao/leitura.R")

## Lê os arquivos
df <- read_vitamin("Dados/vitamina.csv")
opt <- otimizador_max_veros(5, df, "tempo", "censura")
teste_hipo(opt)
func(df, opt, "tempo", "censura")

## Função para verificar o ajuste do modelo


betas <- opt$par[-1]
model_vars <- df[, -which(names(df) %in% c(tempo, cens))]
x_b <- as.matrix(model_vars)%*%betas

mu <- exp(x_b)
e <- H_t(tempo = df[[tempo]], beta = opt$par[1], mu = mu)

s <- Surv(e, df[[cens]])
km <- surv_fit(s ~ 1, df)
km1 <- survfit(s ~ 1, df)
a <- ggsurvplot(km, data = df, conf.int = F)
a + 
  geom_line(aes(x = a$plot$data$time,
                exp(-a$plot$data$time)))

