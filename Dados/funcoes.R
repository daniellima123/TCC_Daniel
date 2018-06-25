dens<-function(tempo,mu,beta){					# sub-funções que realizarão o cálculo
  (beta*(tempo/mu)^(beta-1)) / (  mu* (1+(tempo/mu)^beta)^2  )
}		# da função densidade

distr <- function(tempo, mu, beta){
  sobrev(tempo, mu, beta) - sobrev(tempo + 1, mu, beta)
}

sobrev<-function(tempo,mu,beta){					# sub-funções que realizarão o cálculo

  (1+((tempo+1)/mu)^beta)^-1

}						# da função de sobrevivência

H_t <- function(tempo, mu, beta){
  -log(sobrev(tempo, mu, beta))
}

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

l_reg <- function(tempo, cens, parameter){
  
  
  
  beta = parameter[1]
  
  beta_0 = parameter[2]
  beta_1 = parameter[3]
  beta_2 = parameter[4]
  beta_3 = parameter[5]
  
  mu = exp(beta_0 + beta_1*as.numeric(vitamin.df$x3) + beta_2*vitamin.df$x2 + beta_3*vitamin.df$idade_1)
  
  L1 = log(distr(tempo, mu, beta))
  L2 = log(sobrev(tempo, mu, beta))

  if(mu > 0 && beta > 0){
    val = -1 * sum(cens*L1 + (1-cens)*L2)
  }
  else val = -Inf
  
  return(val)
}

opt <- optim(par = c(2, 0.5, 1, 3), fn = l_reg,
        t = vitamin.df$tempo, cens = vitamin.df$cens, hessian=T)
mat
teste_hipo <- function(opt){
  matriz <- opt$hessian[-1,-1]
  mat_inv <- solve(matriz)  
  vari <- diag(mat_inv)
  par <- opt$par[-1]

  p_val <- ifelse(par > 0, 2*pnorm(par/sqrt(vari), lower.tail = F),
                           2*pnorm(par/sqrt(vari), lower.tail = T))
  p_val
}

otimizador_max_veros <- function(n, data, tempo, censura){
  opt <- NULL
  attempt <- 1
  
  while(is.null(opt) && attempt <= 1000){
    parametros <<- sample(n)
    try(
      
      opt <- optim(par = parametros, fn = l_reg,
                   tempo = data[[tempo]], cens = data[[censura]], hessian=T,
                   control = list(maxit = 10000))
    )
  }
  opt
}

opt <- otimizador_max_veros(5, vitamin.df, "tempo", "cens")

params <- opt$par[-1]
beta <- opt$par[1]
model_vars <- vitamin.df[, -c(1, 2, 4, 7)]

x_b <- as.matrix(model_vars)%*%params
mu <- exp(x_b)
e <- H_t(tempo = vitamin.df$tempo, beta = beta, mu = mu)

su <- Surv(e, vitamin.df$cens)
su.km <- survfit(su ~ 1, vitamin.df)


unique_e <- sort(e)
S_unique <- S_exp(unique_e, 1)
a <- ggsurvplot(su.km, conf.int = F)
a + 
  geom_line(aes(x = a$plot$data$time,
                exp(-a$plot$data$time)))

