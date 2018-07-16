## Importa as funções
source("programacao/funcoes.R")
source("programacao/leitura.R")

## Variáveis
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Turno', 'Idade')
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Idade')
vars <- c('Tempo', 'Status', 'x0', 'Sexo')
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Escola')
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Escola', 'Ingresso')# chisq < 0.05
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Ingresso')
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Ingresso', 'Origem')# chisq < 0.05
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Escola', 'Origem')# chisq ~ 0
vars <- c('Tempo', 'Status', 'x0', 'Sexo', 'Escola', 'Origem')# chisq ~ 0

## Lê os arquivos
df <- read_quimica("Dados/Banco de dados - QuimIn.csv", vars = vars)
opt <- otimizador_max_veros(length(vars) - 1, df, "Tempo", "Status")
teste_hipo(opt)



betas <- opt$par[-1]
model_vars <- df[, -which(names(df) %in% c('Tempo', 'Status'))]
x_b <- as.matrix(model_vars)%*%betas

mu <- exp(x_b)
e <- H_t(tempo = df[['Tempo']], beta = opt$par[1], mu = mu)

s <- Surv(e, df[['Status']])
km <- surv_fit(s ~ 1, df)
a <- ggsurvplot(km, data = df, conf.int = F)
a + 
  geom_line(aes(x = a$plot$data$time,
                exp(-a$plot$data$time)))
