#Limpieza 
rm(list=ls())

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

#Carguemos Balanceo down
load(file="Balanceo_down.Rdata")
load(file="balanceo_down_extras.Rdata")
#Para lo siguiente se requiere tener la BBDD de testing

#Modelo
load(file="RF_down.Rdata")
print(modelo_randomForest11$confusion)

#importance
png(file =paste("SALIDA/RF_down_importance_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
varImpPlot(modelo_randomForest11, sort=T, n.var = 10, main ='Top 10 - variables - bosque aleatorio - churn')
dev.off()
write.table(importance(modelo_randomForest11),file=paste("SALIDA/RF_down_importance_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#extraemos el último valor OOB (error de clasificación)
tail(modelo_randomForest11$err.rate[, 1], 1)
print(modelo_randomForest11)

# Evaluation metrics testing
churn.predict.prob <- predict(modelo_randomForest11, testing, type="prob")[,2]
churn.predict <- predict(modelo_randomForest11, testing)
confusionMatrix<-confusionMatrix(churn.predict, testing$TARGET_objetivo, positive = "1")
pr <- prediction(churn.predict.prob, testing$TARGET_objetivo)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

png(file =paste0("SALIDA/RF_down_ROC_testing_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
plot(prf ,main=paste("ROC Curve for Random Forest Testing AUC=",  round(auc,2) ) ,col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

save(modelo_randomForest11, churn.predict.prob, churn.predict.prob,confusionMatrix, pr, auc,file="salida/RF_down_metrics_testing.Rdata")
  
# Evaluation metrics training
churn.predict.prob_training <- predict(modelo_randomForest11, training, type="prob")[,2]
churn.predict_training <- predict(modelo_randomForest11, training)
confusionMatrix_training<-confusionMatrix(churn.predict_training, training$TARGET_objetivo, positive = "1")

#Roc training
# plotting ROC curve
pr_training <- prediction(churn.predict.prob_training, training$TARGET_objetivo)
prf_training <- performance(pr_training, measure = "tpr", x.measure = "fpr")

# AUC value
auc_training <- performance(pr_training, measure = "auc")
auc_training <- auc_training@y.values[[1]]
auc_training

png(file =paste0("SALIDA/RF_down_ROC_training_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
plot(prf_training ,main=paste("ROC Curve for Random Forest Training AUC=", round(auc_training,2) ),col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

save(modelo_randomForest11, churn.predict.prob_training, churn.predict.prob_training,confusionMatrix_training, pr_training, auc_training,file="SALIDA/RF_down_metrics_training.Rdata")
load(file="RF_down_metrics_training.Rdata.Rdata")

capture.output(print(modelo_randomForest11, na.print=''), file=paste("SALIDA/RF_down_confusion_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix , na.print=''), file=paste("SALIDA/RF_down_confusion_testing_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix_training , na.print=''), file=paste("SALIDA/RF_down_confusion_training_",format(today, format="%Y%m%d"),".txt", sep=""))

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

#Cálculo de los puntos de corte con la base training ()
BBDD_training<-sqldf("select MAX_SAL_CAPITA_objetivo, min_MADURACION_SALDO_objetivo, TARGET_objetivo from training")
BBDD_training<-cbind(BBDD_training, as.data.frame(churn.predict.prob_training) )

decil_prob<-decil(BBDD_training$churn.predict.prob_training)
decil_saldo<-decil(BBDD_training$MAX_SAL_CAPITA_objetivo)
decil_maduracion<-decil(BBDD_training$min_MADURACION_SALDO_objetivo)

#Distribución del training por puntos de corte
BBDD_training$prob_training <-  cut(as.numeric(BBDD_training$churn.predict.prob_training) , breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
BBDD_training$SAL_CAPITA_training <-  cut(BBDD_training$MAX_SAL_CAPITA_objetivo , breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
BBDD_training$MADURACION_SALDO_training <-  cut(BBDD_training$min_MADURACION_SALDO_objetivo , breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

#Backtesting de los puntos de corte
BBDD_testing<-sqldf("select MAX_SAL_CAPITA_objetivo, min_MADURACION_SALDO_objetivo, TARGET_objetivo from testing")
BBDD_testing<-cbind(BBDD_testing, as.data.frame(churn.predict.prob) )

BBDD_testing$prob_testing <-  cut(as.numeric(BBDD_testing$churn.predict.prob) , breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
BBDD_testing$SAL_CAPITA_testing <-  cut(BBDD_testing$MAX_SAL_CAPITA_objetivo , breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
BBDD_testing$MADURACION_SALDO_testing <-  cut(BBDD_testing$min_MADURACION_SALDO_objetivo , breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

resumen_prob_testing<-sqldf("select prob_testing, count() num_clientes , sum(TARGET_objetivo) num_target  from BBDD_testing group by  prob_testing")
resumen_prob_training<-sqldf("select prob_training, count(), sum(TARGET_objetivo) from BBDD_training group by  prob_training")
resumen_saldo_testing<-sqldf("select SAL_CAPITA_testing, count() num_clientes , sum(TARGET_objetivo) num_target  from BBDD_testing group by  SAL_CAPITA_testing")
resumen_saldo_training<-sqldf("select SAL_CAPITA_training, count(), sum(TARGET_objetivo) from BBDD_training group by  SAL_CAPITA_training")
resumen_MADURACION_SALDO_testing<-sqldf("select MADURACION_SALDO_testing, count() num_clientes , sum(TARGET_objetivo) num_target  from BBDD_testing group by  MADURACION_SALDO_testing")
resumen_MADURACION_SALDO_training<-sqldf("select MADURACION_SALDO_training, count() num_clientes , sum(TARGET_objetivo) num_target  from BBDD_training group by  MADURACION_SALDO_training")

write.table(resumen_prob_testing ,file=paste("SALIDA/RF_down_back_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_saldo_testing,file=paste("SALIDA/RF_down_back_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_MADURACION_SALDO_testing ,file=paste("SALIDA/RF_down_back_puntos_corte_mad_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

write.table(resumen_prob_training ,file=paste("SALIDA/RF_down_back_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_saldo_training,file=paste("SALIDA/RF_down_back_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_MADURACION_SALDO_training ,file=paste("SALIDA/RF_down_back_puntos_corte_mad_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Tablas para el análisis
write.table(rbind(freq(BBDD_training$prob_training, useNA="ifany"),freq(BBDD_testing$prob_testing, useNA="ifany")),file=paste("SALIDA/RF_down_back_training_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(rbind(freq(BBDD_training$SAL_CAPITA_training, useNA="ifany"),freq(BBDD_testing$SAL_CAPITA_testing, useNA="ifany")),file=paste("SALIDA/RF_down_back_training_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(rbind(freq(BBDD_training$MADURACION_SALDO_training, useNA="ifany"),freq(BBDD_testing$MADURACION_SALDO_testing, useNA="ifany")),file=paste("SALIDA/RF_down_back_training_puntos_corte_maduracion_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_trainingB<-sqldf("select * from BBDD_training where TARGET_objetivo=1 ")

write.table(rbind(table(BBDD_training$SAL_CAPITA_training, BBDD_training$prob_training, useNA="ifany"), table(BBDD_trainingB$SAL_CAPITA_training, BBDD_trainingB$prob_training, useNA="ifany")),file=paste("SALIDA/RF_down_tabla_saldo_prob_training_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 
write.table(rbind(table(BBDD_training$MADURACION_SALDO_training, BBDD_training$prob_training, useNA="ifany"),table(BBDD_trainingB$MADURACION_SALDO_training, BBDD_trainingB$prob_training, useNA="ifany") ),file=paste("SALIDA/RF_down_tabla_maduracion_prob_training_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 

BBDD_testingB<-sqldf("select * from BBDD_testing where TARGET_objetivo=1 ")
write.table(rbind(table(BBDD_testing$SAL_CAPITA_testing, BBDD_testing$prob_testing, useNA="ifany"), table(BBDD_testingB$SAL_CAPITA_testing, BBDD_testingB$prob_testing, useNA="ifany")),file=paste("SALIDA/RF_down_tabla_saldo_prob_testing_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 
write.table(rbind(table(BBDD_testing$MADURACION_SALDO_testing, BBDD_testing$prob_testing, useNA="ifany"),table(BBDD_testingB$MADURACION_SALDO_testing, BBDD_testingB$prob_testing, useNA="ifany") ),file=paste("SALIDA/RF_down_tabla_maduracion_prob_testing_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 

