#parametro  tuning_rf_mtry
tuning_rf_mtry <- function(df, y, ntree = 200){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del número de predictores evaluados (mtry)
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = número de árboles creados en el modelo randomForest
  max_predictores <- ncol(df) - 1
  print(max_predictores)
  n_predictores   <- rep(NA, max_predictores)
  oob_err_rate    <- rep(NA, max_predictores)
  print(c("hasta ", max_predictores) )
  for (i in 1:max_predictores) {
    gc()
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
    print(c("mtry= ", i))
  }
  results <- data_frame(n_predictores, oob_err_rate)
  return(results)
}

#Gráfico tuning_rf_mtry

grafico_mtry<-function(hiperparametro_mtry){
  ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
    scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
    geom_line() +
    geom_point() +
    geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
               color = "red") +
    labs(title = "Evolución del out-of-bag-error vs mtry",
         x = "nº predictores empleados") +
    theme_bw()
  
  
}

#funcion para graficar tuning_rf_mtry

#Parametro nodesize: número mínimo de observaciones que deben tener los nodos terminales  
#Nota para clasificación se utiliza por definición nodesize = 1 
tuning_rf_nodesize <- function(df, y, mtry, size = NULL, ntree = 200){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del tamaño mínimo de los nodos terminales (nodesize).
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_err_rate <- rep(NA, length(size))
  print(c("hasta ",seq_along(size)))
  for (i in seq_along(size)) {
    gc()
    set.seed(321)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = mtry, ntree = ntree,
                              nodesize = i)
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
    print(c("nodesize ",i))
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

#Grafico parámetro nodesize
grafico_nodesize<-function(hiperparametro_nodesize){
  ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
    scale_x_continuous(breaks = hiperparametro_nodesize$size) +
    geom_line() +
    geom_point() +
    geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
               color = "red") +
    labs(title = "Evolución del out-of-bag-error vs nodesize",
         x = "nº observaciones en nodos terminales") +
    theme_bw()
  
}

