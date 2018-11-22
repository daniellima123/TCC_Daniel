library(pacman)
p_load(survival)
p_load(survminer)

## Função densidade de probabilidade
dens<-function(tempo,mu,beta){					
  (beta*(tempo/mu)^(beta-1)) / (  mu * (1+(tempo/mu)^beta)^2  )
}
## Função Distribuição de probabilidade
distr <- function(tempo, mu, beta){
  sobrev(tempo, mu, beta) - sobrev(tempo + 1, mu, beta)
}
## Função de Sobrevivência
sobrev<-function(tempo,mu,beta){					# sub-funções que realizarão o cálculo
  
  (1+((tempo+1)/mu)^beta)^-1
  
}						# da função de sobrevivência
## Função risco acumulada
H_t <- function(tempo, mu, beta){
  -log(sobrev(tempo, mu, beta))
}
## Máxima verossimilhança da log-logística
l_t <- function(tempo, cens, parameter){
  
  mu = parameter[1]
  
  beta = parameter[2]
  
  L1 = log(distr(tempo, mu, beta))
  L2 = log(sobrev(tempo, mu, beta))
  
  if(mu > 0 && beta > 0){
    val = -1 * sum(cens*L1 + (1-cens)*L2)
  }
  else val = -Inf
  
  return(val)
}

## Função de verossimilhança para a estimação da regressão
l_reg <- function(par, data, tempo, cens){
  
  beta <- par[1]
  
  mu <- exp(as.matrix(data)%*%par[-1])
  print(mu)
  L1 = log(distr(tempo, mu, beta))
  L2 = log(sobrev(tempo, mu, beta))
  
  if(mu > 0 && beta > 0){
    val = -1 * sum(cens*L1 + (1-cens)*L2)
  }
  else val = -Inf
  
  return(val)
  
}

## Função para otimizar a função de maxima verossimilhança da regressão
otimizador_max_veros <- function(n, data, tempo, censura){
  opt <- NULL
  attempt <- 1
  data_entry <- data[, which(!(names(data) %in% c(tempo, censura)))]  
  while(is.null(opt) && attempt <= 100){
    parametros <<- sample(n)
    try(
      
      opt <- optim(par = parametros, fn = l_reg,
                   tempo = data[[tempo]], cens = data[[censura]], data = data_entry,
                   hessian=T, control = list(maxit = 10000))
    )
    attempt <- attempt + 1
  }
  opt
}
otimizador_max_veros(5, d, 'tempo', 'censura')
## Função para realizar os testes de hipótese nos parâmetros da regressão

teste_hipo <- function(opt){
  matriz <- opt$hessian[-1,-1]
  mat_inv <- solve(matriz)  
  vari <- diag(mat_inv)
  par <- opt$par[-1]
  
  p_val <- ifelse(par > 0, 2*pnorm(par/sqrt(vari), lower.tail = F),
                  2*pnorm(par/sqrt(vari), lower.tail = T))
  p_val
}

### RODAR O OPTIM PARA TODAS AS VARIÁVEIS
### RODAR O TESTE DE HIPOTESES
### PEGAR A VARIÁVEL MENOS SIGNIFICANTE
### TESTAR SE FOI BOM TIRAR OU NÃO
### REPETIR PARA O MODELO ESCOLHIDO
expl <- c('x0', 'Sexo', 'Turno', 'Escola', 'Ingresso', 'Idade', 'Origem')
par <- c(1.65, 2.08, 3, 0.27, -0.84, -0.5, -0.3, -0.5)
non_sig <- 5
#model_vars <- expl

  
#init_param <- par
back <- function(df, tempo, censura, sobrev, para = NULL, signi_level =0.1, vars = NULL){
  if(is.null(vars)){
    model_vars <- names(df[, -which(names(df) %in% c(tempo, censura))])
    init_param <- c(1.65, 2.08, 3, 0.27, -0.84, -0.5, -0.3, -0.5)
  } else {
    model_vars <- vars
    init_param <- para
  }
  opt <- optim(par = init_param, l_reg1, df = df, interval = interval, s = sobrev,
               tempo = tempo, censura = censura, expl = model_vars, hessian = T, control = list(maxit = 10000))
  tes <- teste_hipo(opt)
  non_sig <- which.max(tes) + 1
  opt_sem <- optim(par = init_param[-non_sig], l_reg1, df = df, interval = interval, s = sobrev,
                   tempo = tempo, censura = censura, expl = model_vars[-(non_sig-1)], hessian = T,
                   control = list(maxit = 10000))
  print(model_vars[(non_sig-1)])
  p_val <- pchisq(-2*(opt$value - opt_sem$value), 1, lower.tail = F)
  print(round(p_val, 5))
  if(p_val < signi_level) {
    print(model_vars)
    return(opt)
  }
  else{
    model_vars <- model_vars[-(non_sig-1)]
    init_param <- init_param[-non_sig]
    return(back(df = df, tempo = tempo, censura = censura, sobrev = sobrev,
                para = init_param, vars = model_vars))
  }
}
Sys.time()
opt <- back(df = d, tempo = 'Tempo', censura = 'Status', sobrev = sobrev, signi_level = 0.1)
Sys.time()
## 10% modelo = intercepto, sexo, turno, ingresso, idade
## 5% modelo = intercepto, sexo, turno, ingresso

opt <- back(df = d, tempo = 'tempo', censura = 'censura', sobrev = sobrev,
            vars = c('x0', 'idade', 'tratamento', 'sexo'), para = c(1, 2, 3, 4, 5),
            signi_level = 0.1)
