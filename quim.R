library(pacman)
p_load(survival)
p_load(survminer)
p_load(foreach)
p_load(doMC)
# sub-funções que realizarão o cálculo
sobrev<-function(tempo,mu,beta){
  
  (1+((tempo)/mu)^beta)^-1
  
}

categ_veross <- function(s, tempo, tempo_1, parameter, censura, lim_inf, lim_sup){
  if((tempo > lim_inf && tempo < lim_sup) && censura == 1){
    val <- log(1-(s(log(tempo_1), parameter[1], parameter[2])/s(log(tempo), parameter[1], parameter[2])))
  } else if((tempo > lim_inf && tempo < lim_sup) && censura == 0){
    val <- 0.5*log(s(log(tempo_1), parameter[1], parameter[2])/s(log(tempo), parameter[1], parameter[2])) 
    
  } else {
    val <- log(s(log(tempo_1), parameter[1], parameter[2])/s(log(tempo), parameter[1], parameter[2]))
  }
  return(val)
}

s_ext <- function(t, mu, sigma){
  exp(-exp( (t - mu) / sigma) )
}



# Verossimilhança
l_group <- function(par, df, interval, s, censura, tempo){
  val <- c()
  for(i in seq_len(nrow(interval))){
    for(j in seq_len(nrow(df)-1)){
      if(df[[tempo]][j] >= interval$inf[i]){
        if(df[[tempo]][j] < interval$sup[i] && df[[censura]][j] == 1){
          val <- c(val,
                   log(1-(s(interval$sup[i], par[1], par[2])/s(interval$inf[i], par[1], par[2]))))
        } else if(df[[tempo]][j] < interval$sup[i] && df[[censura]][j] == 0){
          val <- c(val,
                   0.5*log(s(interval$sup[i], par[1], par[2])/s(interval$inf[i], par[1], par[2]))) 
        } else if(df[[tempo]][j] > interval$sup[i]){
          val <- c(val,
                   log(s(interval$sup[i], par[1], par[2])/s(interval$inf[i], par[1], par[2])))
        }
      }
      
    }
  }
  if(par[1] > 0 && par[2] > 0){
    print(-1 * sum(val))
    return(-1 * sum(val))
  } else return(-Inf)
}


l_reg <- function(par, df, interval, s, censura, tempo){
  alpha <- exp(as.matrix(df[,c('x0', 'Sexo', 'Turno', 'Escola', 'Ingresso', 'Idade', 'Origem')])%*%par[-1])
  gam <- par[1]
  val <- c()
  for(i in seq_len(nrow(interval))){
    #print(any(is.na(val)))
    for(j in seq_len(nrow(df)-1)){
      if(df[[tempo]][j] >= interval$inf[i]){
        if(df[[tempo]][j] < interval$sup[i] && df[[censura]][j] == 1){
          val <- c(val,
                   log(1-(s(interval$sup[i], alpha[j], gam)/s(interval$inf[i], alpha[j], gam))))
        } else if(df[[tempo]][j] < interval$sup[i] && df[[censura]][j] == 0){
          val <- c(val,
                   0.5*log(s(interval$sup[i], alpha[j], gam)/s(interval$inf[i], alpha[j], gam))) 
        } else if(df[[tempo]][j] > interval$sup[i]){
          val <- c(val,
                   log(s(interval$sup[i], alpha[j], gam)/s(interval$inf[i], alpha[j], gam)))
        }
      }
      
    }
  }
  if(alpha > 0 && gam > 0){
    #print(-1 * sum(val))
    return(-1 * sum(val))
  } else print(gam); return(-Inf);
}

# Origem é o mais não significativo.
# Verificando se a retirada é significativa.
#
l_reg1 <- function(par, df, interval, s, censura, tempo, expl){
  alpha <- exp(as.matrix(df[,expl])%*%par[-1])
  gam <- par[1]
  data_mod <- data.frame(tempo = df[[tempo]], cens = d[[censura]], alpha = alpha)
  va <- sapply(1:nrow(int_df), function(x){
    v <- ifelse(data_mod$tempo < int_df$inf[x], 0,
                ifelse(data_mod$tempo < int_df$sup[x] & data_mod$cens == 1,
                       log(1-(s(int_df$sup[x], data_mod$alpha, gam)/s(int_df$inf[x], data_mod$alpha, gam))),
                       ifelse(data_mod$tempo < int_df$sup[x] & data_mod$cens == 0,
                              0.5*log(s(int_df$sup[x], data_mod$alpha, gam)/s(int_df$inf[x], data_mod$alpha, gam)),
                              log(s(int_df$sup[x], data_mod$alpha, gam)/s(int_df$inf[x], data_mod$alpha, gam)))
                )
    )
    sum(v)
  }
  )
  if(alpha > 0 && gam > 0){
    #print(-1 * sum(val))
    return(-1 * sum(va))
  } else print(gam); return(-Inf);
  
}

# 2.0085408  2.0550870  0.3617486 -1.0714221 -0.1203370 -0.1152171 14:06
Sys.time()
opt <- optim(par = c(1.65, 2.08, 3, 0.27, -0.84, -0.5, -0.3), l_reg, df = d, interval = int_df, s = sobrev,
             tempo = 'Tempo', censura = 'Status', hessian = T, control = list(maxit = 10000))
Sys.time()
opt_1 <- optim(par = c(1.65, 2.08, 3, 0.27, -0.84, -0.5, -0.3), l_reg1, df = d, interval = int_df, s = sobrev,
             tempo = 'Tempo', censura = 'Status', hessian = T, control = list(maxit = 10000))
Sys.time()
teste_hipo(opt_1)

val_1 <- opt_1$value
val_2 <- opt_2$value



mu_semestre <- opt$par[1]
beta_semestre <- opt$par[2]
mu_ano <- opt$par[1]
beta_ano <- opt$par[2]

