# Método congruencial para geração de variáveis uniformes

cong_unif <- function(n = 5000){
  a <- 1226874159
  m <- 2^31 - 1
  y <- as.numeric(Sys.time())
  x <- c()
  for(i in 1:n){
    y <- (a*y) %% m
    x[i] <- y/m
  }
  x
}

norm_pol <- function(n = 5000){
  unif1 <- cong_unif(n)
  unif2 <- cong_unif(n)
  
  r_2 <- -2*log(unif1)
  angle <- 2*pi*unif2
  
  x <- sqrt(r_2)*cos(angle)
  
  x
}


