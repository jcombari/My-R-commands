#Limpieza 
rm(list=ls())
.rs.restartR() #restart
#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR","zoo", "ROCR","randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()
#Para lo siguiente se requiere tener la BBDD de testing
#load(file="pre_balanceo2.Rdata")
load(file="Balanceo_down.Rdata")
load(file="balanceo_down_extras.Rdata")

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

#Balanceo
eliminar<-c("min_MADURACION_CUOTA_objetivo")
training<-training[ , !(names(training) %in% eliminar)]
training2<-training2[ , !(names(training2) %in% eliminar)]
#down_training<-downSample(x=training, y=training$TARGET_objetivo)
#down_training2<-downSample(x=training2, y=training2$TARGET2_objetivo)
#save(down_training, down_training2, file="balanceo_down.Rdata")
load(file="balanceo_down.Rdata")

eliminar<-c("Class")

down_training<-down_training[ , !(names(down_training) %in% eliminar)]

#https://rpubs.com/Joaquin_AR/255596

#Modelamiento RandomForest
#La función randomForest() emplea como valores por defecto nodesize = 1 
#en problemas de clasificación, nodesize = 5 en regresión y  ntree = 500, 
#Ambos hiperparámetros se optimizan empleando el out of bag error.

source("E:/Jennyfer_C/04_CHURN/CODIGO/08_Optimización_hiperparámetros.R")

hiperparametro_mtry <-  tuning_rf_mtry(df = down_training, y = "TARGET_objetivo")

#Ver y analizarla siguiente tabla 
hiperparametro_mtry %>% arrange(oob_err_rate)
grafico_mtry(hiperparametro_mtry)

#Se debe seleccionar el valor del mtry y reemplazarlo en la función tuning_rf_nodesize
#Hiperparametro nodesize

hiperparametro_nodesize <-  tuning_rf_nodesize(df = down_training, y = "TARGET_objetivo",
                                               size = c(1:20), mtry=16)

hiperparametro_nodesize %>% arrange(oob_err_rate)

save(hiperparametro_mtry, hiperparametro_nodesize, file="hiperparametro_RF.Rdata")

grafico_nodesize(hiperparametro_nodesize)

#Se debe selccionar el valor del nodesize y reemplazarlo abajo para obneter el ntree optimo

modelo_randomforest <- randomForest(TARGET_objetivo ~ ., data = down_training, mtry = 16, ntree = 500,
                                    importance = TRUE, nodesize = 1)

oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                           arboles = seq_along(modelo_randomforest$err.rate[, 1]))

save(hiperparametro_mtry, hiperparametro_nodesize, oob_err_rate, file="hiperparametro_RF.Rdata")

#Se debe analizar el siguiente gráfico
ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

#Modelo definitivo colocar los valores mtry, nodesize, ntree anteriores

set.seed(1234)
down_training$Class

modelo_randomforest <- randomForest(TARGET_objetivo ~ ., data = down_training, mtry = 10, ntree = 50,
                                    importance = TRUE, nodesize = 1,
                                    norm.votes = TRUE )

save(modelo_randomforest,file="Random_forest_model_down.Rdata")

#extraemos el último valor OOB (error de clasificación)
tail(modelo_randomforest$err.rate[, 1], 1)

#Identificación de los predictores más influyentes
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data=importancia_pred, aes(x=reorder(variable, MeanDecreaseAccuracy),
                                        y = MeanDecreaseAccuracy,
                                        fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, MeanDecreaseGini),
                                          y = MeanDecreaseGini,
                                          fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

#Backtesting

varImpPlot(modelo_randomforest, sort=T, n.var = 10, main ='Top 10 - variables - bosque aleatorio - churn')
write.table(importance(modelo_randomforest),file=paste("SALIDA/Random_forest_importance_down_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

plot(modelo_randomforest$err.rate[,1], log = "y") 
modelo_randomforest$err.rate[200,1]

print(modelo_randomforest)

# Evaluation metrics testing
modelo.pred_testing      <- predict(modelo_randomforest, newdata = testing, type = "class") 
modelo.prob_testing <-predict(modelo_randomforest,testing, type="prob")[,2]
write.table(modelo.pred_testing ,file=paste("SALIDA/Random_forest_pred_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_testing        <- confusionMatrix(data = modelo.pred_testing, testing$TARGET_objetivo)  
modelo.F1_testing          <- modelo.result_testing  $byClass['F1']
save(modelo.pred_testing   ,modelo.result_testing   , modelo.F1_testing    , file="Random_forest_results_down_testing.Rdata")

print("Matriz de confusion")
print(modelo.result_testing$table)

# Evaluation metrics training
modelo.pred_training      <- predict(modelo_randomforest, newdata = training, type = "class") 
modelo.prob_training <-predict(modelo_randomforest,training, type="prob")[,2]
write.table(modelo.pred_training ,file=paste("SALIDA/Random_forest_pred_training",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_training      <- confusionMatrix(data = modelo.pred_training, training$TARGET_objetivo)  
modelo.F1_training        <- modelo.result_training$byClass['F1']
print("Matriz de confusion")
print(modelo.result_training$table)
save(modelo.pred_training  ,modelo.result_training  , modelo.F1_training    , file="Random_forest_results_down_training.Rdata")

#Analisis de puntos de corte
modelo.prob_training_tram <-  cut(modelo.prob_training , breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
freq(modelo.prob_training_tram ,usena="ifany")
BBDD_training<-as.data.frame(cbind( modelo.pred_training , modelo.prob_training, training$TARGET_objetivo ,training$MAX_SAL_CAPITA_objetivo, training$min_MADURACION_SALDO_objetivo))
names(BBDD_training)<-c("modelo.pred_training", "modelo.prob_training","TARGET_objetivo","MAX_SAL_CAPITA_objetivo","min_MADURACION_SALDO_objetivo")

decil_prob<-decil(BBDD_training$modelo.prob_training)
BBDD_training$modelo_prob_training_tram<-  cut(BBDD_training$modelo.prob_training, breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
decil_saldo<-decil(training$MAX_SAL_CAPITA_objetivo)
BBDD_training$MAX_SAL_CAPITA_objetivo_tram<- cut(BBDD_training$MAX_SAL_CAPITA_objetivo, breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
decil_maduracion<-decil(BBDD_training$min_MADURACION_SALDO_objetivo)
BBDD_training$min_MADURACION_SALDO_objetivo_tram<- cut(BBDD_training$min_MADURACION_SALDO_objetivo, breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

#clientes<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training group by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
clientes<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training group by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")

#freq(BBDD_training$TARGET_objetivo,useNA="ifany")

BBDD_training2<-sqldf("select * from BBDD_training where TARGET_objetivo=2 ")
#churn<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 group by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
churn<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 group by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")


#churn<-table(BBDD_training2$MAX_SAL_CAPITA_objetivo_tram,BBDD_training2$modelo.prob_training_tram, useNA="ifany")

write.table(clientes,file=paste("SALIDA/Random_forest_down_tabla_seg_clientes",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(churn,file=paste("SALIDA/Random_forest_down_tabla_seg_churn",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_testing<-as.data.frame(cbind(test$CEDULAENC_objetivo,test$TARGET_objetivo, modelo.pred_testing , modelo.prob_testing ))

names(BBDD_testing)<-c("CEDULAENC", "target","modelo.pred_testing", "modelo.prob_testing")
save(BBDD_testing, BBDD_training, file="random_forest11_down.Rdata")
table(BBDD_testing$target,BBDD_testing$modelo.pred_testing, useNA="ifany")

write.table(BBDD_testing,file=paste("SALIDA/Random_forest_down_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(BBDD_training,file=paste("SALIDA/Random_forest_down_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
