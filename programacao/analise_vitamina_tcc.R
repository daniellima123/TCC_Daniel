library(pacman)
p_load(survival)
p_load(survminer)

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


l_reg <- function(par, df, interval, s, censura, tempo, expl){
  alpha <- exp(as.matrix(df[,expl])%*%par[-1])
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

l_reg1 <- function(par, df, interval, s, censura, tempo, expl){
  alpha <- exp(as.matrix(df[,expl])%*%par[-1])
  gam <- par[1]
  data_mod <- data.frame(tempo = df[[tempo]], cens = df[[censura]], alpha = alpha)
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

opt_qu <- optim(par = c(1, 2, 1, -1, -1, 0), df=df, interval=int_df, s = sobrev, tempo = 'Tempo',
                censura='Status', expl = c('x0', 'Sexo', 'Turno', 'Ingresso', 'Idade'), fn = l_reg1)

opt_quim_vazio <- optim(par = c(1, 2), l_group, df = df, interval = int_df, s = sobrev, tempo = 'Tempo',
                        censura = 'Status', hessian = T)


opt_vazio <- optim(par = c(1, 2), l_group, df = d, interval = int_df, s = sobrev, tempo = 'tempo',
                   censura = 'censura', hessian = T)

opt_1 <- optim(par = c(1.65, 2.08, 0.27, -0.84), l_reg, df = d, interval = int_df, s = sobrev,
             tempo = 'tempo', censura = 'censura', hessian = T)
Sys.time()
opt <- optim(par = c(1.65, 2.08, 0.27, -0.84), l_reg1, df = d, interval = int_df, s = sobrev,
             tempo = 'tempo', censura = 'censura', hessian = T, expl = c('x0', 'idade', 'tratamento'))
Sys.time()
mu_semestre <- opt$par[1]
beta_semestre <- opt$par[2]
mu_ano <- opt$par[1]
beta_ano <- opt$par[2]

alpha <- opt_vazio$par[1]
gamma <- opt_vazio$par[2]
km <- surv_fit(Surv(d$tempo, d$censura) ~ 1, data = d)
a <- ggsurvplot(km, conf.int = F, color = 'red', legend = c(0.85, 0.85),
                ggtheme = theme_bw(), legend.labs = c('Kaplan-Meier'))
ggsurvplot(km, data = d, conf.int = F, palette = 'red', legend = c(0.85, 0.85),
           ggtheme = theme_bw(), legend.labs = c('Kaplan-Meier'), legend.title = 'Curvas')
a$plot+
  geom_line(aes(x = a$plot$data$time, sobrev(a$plot$data$time, alpha, gamma)))+
  xlab('Tempo')+ylab('S(t)')+
  annotate("rect", xmin = 150, xmax = 190, ymin = 0.75, ymax = 0.95, fill = 'white')+
  annotate('segment', x = 150, xend = 158, y = 0.93, yend = 0.93, color = 'red', size = 1)+
  annotate('text', x = 179, y = 0.93, label = 'Kaplan-Meier', size=3.3)+
  annotate('text', x = 184, y = 0.88, label = 'Função Estimada', size=3.3)+
  annotate('segment', x = 150, xend = 158, y = 0.88, yend = 0.88, color = 'black', size = 1)

qplot(d$tempo, geom='histogram', binwidth=1)+theme_bw()+xlab('Tempo')+ylab('Frequência')
ggsurv