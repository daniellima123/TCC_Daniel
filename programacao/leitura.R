##### Funções de leitura
## Vitamina
read_vitamin <- function(path){
  
  df <- read.csv(path, sep = ';')
  df <- df[, -ncol(df)]
  names(df) <- c("tempo", "censura", "x0", "idade", "tratamento", "sexo")
  df[,"idade"] <- ifelse(df$idade < 24, 1, 0)
  df
}

## Quimica
read_quimica <- function(path, vars = NULL){
  df <- read.csv(path)
  df[, "x0"] <- 1
  if(is.null(vars)){
    vari <- c("Tempo", "Status", "x0", "Sexo", "Turno",
            "Escola", "Ingresso", "Idade", "Origem")
  } else {
    vari <- vars
  }
  df <- df[, vari]
  if('Idade' %in% vari)
    df[,'Idade'] <- ifelse(df$Idade < median(df$Idade, na.rm = T), 1, 0)
  
  df[complete.cases(df),]
}
