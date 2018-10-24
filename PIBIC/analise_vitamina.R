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



# O trabalho abordará dois bancos de dados, sendo que o primeiro é o banco de dados
# utilizado por Barreto et al. (1994). Este banco de dados estuda pouco mais de mil e duzentas
# crianças e estuda o tempo entre a suplementação de vitamina A ou placebo até o primeiro caso de
# diarréia. O segundo banco de dados, é um banco cedido pela Universidade Estadual da Paraíba
# (UEPB). A variável de estudo nesse caso é o tempo que um estudante de química leva para
# evadir do curso.



# A função de risco acumulado é uma função que não possui uma interpretação simples,
# porém possui importância dentro do campo da análise de sobrevivência. Esta função, denotada
# como H(t), pode ser definida como o logaritmo da função de sobrevivência multiplicado por
# menos um, ou seja:
#   
#   \begin{equation} \label{eq:riskcum}
# H(t) = -\log(S(t))
# \end{equation}




# O modelo apenas com o intercepto e uma variável apresentou os seguintes resultados:
#   
#   ```{r, results='asis'}
# opt_q_Sexo <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Sexo')], tempo = 'Tempo', censura = 'Status')
# testq1 <- teste_hipo(opt_q_Sexo)
# opt_q_t <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Turno')], tempo = 'Tempo', censura = 'Status')
# testq2 <- teste_hipo(opt_q_t)
# opt_q_e <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Escola')], tempo = 'Tempo', censura = 'Status')
# testq3 <- teste_hipo(opt_q_e)
# opt_q_in <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq4 <- teste_hipo(opt_q_in)
# opt_q_id <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Idade')], tempo = 'Tempo', censura = 'Status')
# testq5 <- teste_hipo(opt_q_id)
# opt_q_o <- otimizador_max_veros(3, a[, c('Tempo', 'Status', 'x0', 'Origem')], tempo = 'Tempo', censura = 'Status')
# testq6 <- teste_hipo(opt_q_o)
# para <- c(opt_q_Sexo$par[3], opt_q_t$par[3], opt_q_e$par[3], opt_q_in$par[3],
#           opt_q_id$par[3], opt_q_o$par[3])
# test <- c(testq1[2], testq2[2], testq3[2], testq4[2], testq5[2], testq6[2])
# xt <- xtable(data.frame('Parâmetro' = para, 'P-valor' = round(test, 4)),
#              caption = 'Parâmetros do modelo apenas com o intercepto', label = 'tab_int', align = 'lcc')
# rownames(xt) <- c('Sexo', 'Turno', 'Escola', 'Ingresso', 'Idade', 'Origem')
# print.xtable(xt, include.rownames = T, include.colnames = T, sanitize.rownames.function = identity,
#              comment = F, table.placement = 'H')
# ```
# 
# Segundo a Tabela \ref{tab_int}, as variáveis sexo e forma de ingresso são as mais propícias a entrar no modelo, porém a variável de ingresso é mais significativa e por isso é a variável adicionada. Após isso, repetindo o processo para uma segunda variável, tem-se:
#   
#   ```{r, results='asis'}
# opt_q_Sexo <- otimizador_max_veros(4, a[, c('Tempo', 'Status', 'x0', 'Sexo', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq1 <- teste_hipo(opt_q_Sexo)
# opt_q_t <- otimizador_max_veros(4, a[, c('Tempo', 'Status', 'x0', 'Turno', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq2 <- teste_hipo(opt_q_t)
# opt_q_e <- otimizador_max_veros(4, a[, c('Tempo', 'Status', 'x0', 'Escola', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq3 <- teste_hipo(opt_q_e)
# opt_q_id <- otimizador_max_veros(4, a[, c('Tempo', 'Status', 'x0', 'Idade', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq4 <- teste_hipo(opt_q_id)
# opt_q_o <- otimizador_max_veros(4, a[, c('Tempo', 'Status', 'x0', 'Origem', 'Ingresso')], tempo = 'Tempo', censura = 'Status')
# testq5 <- teste_hipo(opt_q_o)
# para <- c(opt_q_Sexo$par[3], opt_q_t$par[3], opt_q_e$par[3],
#           opt_q_id$par[3], opt_q_o$par[3])
# test <- c(testq1[2], testq2[2], testq3[2], testq4[2], testq5[2])
# xt1 <- xtable(data.frame('Parâmetro' = para, 'P-valor' = round(test, 4)),
#               caption = 'Parâmetros do modelo apenas com o intercepto', label = 'tab_int1', align = 'lcc')
# rownames(xt1) <- c('Sexo', 'Turno', 'Escola', 'Idade', 'Origem')
# print.xtable(xt1, include.rownames = T, include.colnames = T, sanitize.rownames.function = identity,
#              comment = F, table.placement = 'H')
# ```
# 
# Pela Tabela \ref{tab_int1}, nota-se que a variável sexo é a mais significativa e por isso deve ser incluída no modelo. Repetindo o processo para as variáveis restantes, encontra-se o seguinte:
#   
#   ```{r, results='asis'}
# opt_q_t <- otimizador_max_veros(5, a[, c('Tempo', 'Status', 'x0', 'Turno', 'Ingresso', 'Sexo')], tempo = 'Tempo', censura = 'Status')
# testq2 <- teste_hipo(opt_q_t)
# opt_q_e <- otimizador_max_veros(5, a[, c('Tempo', 'Status', 'x0', 'Escola', 'Ingresso', 'Sexo')], tempo = 'Tempo', censura = 'Status')
# testq3 <- teste_hipo(opt_q_e)
# opt_q_id <- otimizador_max_veros(5, a[, c('Tempo', 'Status', 'x0', 'Idade', 'Ingresso', 'Sexo')], tempo = 'Tempo', censura = 'Status')
# testq4 <- teste_hipo(opt_q_id)
# opt_q_o <- otimizador_max_veros(5, a[, c('Tempo', 'Status', 'x0', 'Origem', 'Ingresso', 'Sexo')], tempo = 'Tempo', censura = 'Status')
# testq5 <- teste_hipo(opt_q_o)
# para <- c(opt_q_t$par[3], opt_q_e$par[3],
#           opt_q_id$par[3], opt_q_o$par[3])
# test <- c(testq2[2], testq3[2], testq4[2], testq5[2])
# xt1 <- xtable(data.frame('Parâmetro' = para, 'P-valor' = round(test, 4)),
#               caption = 'Parâmetros do modelo apenas com o intercepto', label = 'tab_int2', align = 'lcc')
# rownames(xt1) <- c('Turno', 'Escola', 'Idade', 'Origem')
# print.xtable(xt1, include.rownames = T, include.colnames = T, sanitize.rownames.function = identity,
#              comment = F, table.placement = 'H')
# ```
# 
# Como nenhuma das variáveis se mostra significativa, não é incluído mais nenhum fator no modelo, chegando assim a um possível modelo final. Após a seleção do modelo, é necessário medir a qualidade do ajuste com o critério gráfico de Cox-Snell.

