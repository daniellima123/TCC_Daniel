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
model_vars <- df[, c('x0', 'Sexo', 'Turno', 'Ingresso', 'Idade')]
model_vars <- d[, c('x0', 'idade', 'tratamento')]
x_b <- as.matrix(model_vars)%*%betas

mu <- exp(x_b)
e <- H_t(tempo = d[['tempo']], beta = opt$par[1], mu = mu)

s <- Surv(e, d[['censura']])
km <- survfit(s ~ 1, d)
a <- ggsurvplot(km,conf.int = F, col = 'red', legend='none')
a$plot + 
  geom_line(aes(x = a$plot$data$time,
                exp(-a$plot$data$time)))+xlab('Resíduo')+ylab('S(t)')+
  theme_bw()
