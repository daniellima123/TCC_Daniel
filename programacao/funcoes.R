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



