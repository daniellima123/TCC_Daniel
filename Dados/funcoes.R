dens<-function(t,mu,beta){					# sub-funções que realizarão o cálculo
  (beta*(t/mu)^(beta-1)) / (  mu* (1+(t/mu)^beta)^2  )
}		# da função densidade

dist <- function(t, mu, beta){
  sobrev(t, mu, beta) - sobrev(t + 1, mu, beta)
}

sobrev<-function(t,mu,beta){					# sub-funções que realizarão o cálculo

  (1+(t/mu)^beta)^-1

}						# da função de sobrevivência


l_t <- function(t, cens, parameter){
  
  mu = parameter[1]
  
  beta = parameter[2]
  
  L1 = log(dist(t, mu, beta))
  L2 = log(sobrev(t, mu, beta))
  
  if(mu > 0 && beta > 0){
    val = -1 * sum(cens*L1 + (1-cens)*L2)
  }
  else val = -Inf
  
  return(val)
}


