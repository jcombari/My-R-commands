#Limpieza 
rm(list=ls())
.rs.restartR() #restar
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
load(file="Balanceo_smote2.Rdata")
load(file="balanceo_smote2_extras.Rdata")
#Para lo siguiente se requiere tener la BBDD de testing2

#Modelo
load(file="RF_smote2.Rdata")
print(modelo_randomForest12$confusion)

#importance
png(file =paste("SALIDA/RF_smote2_importance_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
varImpPlot(modelo_randomForest12, sort=T, n.var = 10, main ='Top 10 - variables - bosque aleatorio - churn')
dev.off()
write.table(importance(modelo_randomForest12),file=paste("SALIDA/RF_smote2_importance_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#extraemos el último valor OOB (error de clasificación)
tail(modelo_randomForest12$err.rate[, 1], 1)
print(modelo_randomForest12)

# Evaluation metrics testing2
churn.predict.prob <- predict(modelo_randomForest12, testing2, type="prob")[,2]
churn.predict <- predict(modelo_randomForest12, testing2)
confusionMatrix<-confusionMatrix(churn.predict, testing2$TARGET2_objetivo, positive = "1")
pr <- prediction(churn.predict.prob, testing2$TARGET2_objetivo)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

png(file =paste0("SALIDA/RF_smote2_ROC_testing2_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
plot(prf ,main=paste("ROC Curve for Random Forest testing2 AUC=",  round(auc,2) ) ,col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

save(modelo_randomForest12, churn.predict.prob, churn.predict.prob,confusionMatrix, pr, auc,file="salida/RF_smote2_metrics_testing2.Rdata")
  
# Evaluation metrics training2
churn.predict.prob_training2 <- predict(modelo_randomForest12, training2, type="prob")[,2]
churn.predict_training2 <- predict(modelo_randomForest12, training2)
confusionMatrix_training2<-confusionMatrix(churn.predict_training2, training2$TARGET2_objetivo, positive = "1")

#Roc training2
# plotting ROC curve
pr_training2 <- prediction(churn.predict.prob_training2, training2$TARGET2_objetivo)
prf_training2 <- performance(pr_training2, measure = "tpr", x.measure = "fpr")

# AUC value
auc_training2 <- performance(pr_training2, measure = "auc")
auc_training2 <- auc_training2@y.values[[1]]
auc_training2

png(file =paste0("SALIDA/RF_smote2_ROC_training2_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
plot(prf_training2 ,main=paste("ROC Curve for Random Forest training2 AUC=", round(auc_training2,2) ),col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

save(modelo_randomForest12, churn.predict.prob_training2, churn.predict.prob_training2,confusionMatrix_training2, pr_training2, auc_training2,file="SALIDA/RF_smote2_metrics_training2.Rdata")

capture.output(print(modelo_randomForest12, na.print=''), file=paste("SALIDA/RF_smote2_confusion_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix , na.print=''), file=paste("SALIDA/RF_smote2_confusion_testing2_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix_training2 , na.print=''), file=paste("SALIDA/RF_smote2_confusion_training2_",format(today, format="%Y%m%d"),".txt", sep=""))

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

#Cálculo de los puntos de corte con la base training2 ()
BBDD_training2<-sqldf("select MAX_SAL_CAPITA_objetivo, min_MADURACION_SALDO_objetivo, TARGET2_objetivo from training2")
BBDD_training2<-cbind(BBDD_training2, as.data.frame(churn.predict.prob_training2))

decil_prob<-decil(BBDD_training2$churn.predict.prob_training2)
decil_saldo<-decil(BBDD_training2$MAX_SAL_CAPITA_objetivo)
decil_maduracion<-decil(BBDD_training2$min_MADURACION_SALDO_objetivo)

#Distribución del training2 por puntos de corte
BBDD_training2$prob_training2 <-  cut(as.numeric(BBDD_training2$churn.predict.prob_training2) , breaks=unique(decil_prob),include.lowest=TRUE,dig.lab=10)
BBDD_training2$SAL_CAPITA_training2 <-  cut(BBDD_training2$MAX_SAL_CAPITA_objetivo , breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
BBDD_training2$MADURACION_SALDO_training2 <-  cut(BBDD_training2$min_MADURACION_SALDO_objetivo , breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

#Backtesting2 de los puntos de corte
BBDD_testing2<-sqldf("select MAX_SAL_CAPITA_objetivo, min_MADURACION_SALDO_objetivo, TARGET2_objetivo from testing2")
BBDD_testing2<-cbind(BBDD_testing2, as.data.frame(churn.predict.prob),as.data.frame(churn.predict ))

BBDD_testing2$prob_testing2 <-  cut(as.numeric(BBDD_testing2$churn.predict.prob) , breaks=unique(decil_prob),include.lowest=TRUE,dig.lab=10)
BBDD_testing2$SAL_CAPITA_testing2 <-  cut(BBDD_testing2$MAX_SAL_CAPITA_objetivo , breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
BBDD_testing2$MADURACION_SALDO_testing2 <-  cut(BBDD_testing2$min_MADURACION_SALDO_objetivo , breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

resumen_prob_testing2<-sqldf("select prob_testing2, count() num_clientes , sum(TARGET2_objetivo) num_TARGET2  from BBDD_testing2 group by  prob_testing2")
resumen_prob_training2<-sqldf("select prob_training2, count(), sum(TARGET2_objetivo) from BBDD_training2 group by  prob_training2")
resumen_saldo_testing2<-sqldf("select SAL_CAPITA_testing2, count() num_clientes , sum(TARGET2_objetivo) num_TARGET2  from BBDD_testing2 group by  SAL_CAPITA_testing2")
resumen_saldo_training2<-sqldf("select SAL_CAPITA_training2, count(), sum(TARGET2_objetivo) from BBDD_training2 group by  SAL_CAPITA_training2")
resumen_MADURACION_SALDO_testing2<-sqldf("select MADURACION_SALDO_testing2, count() num_clientes , sum(TARGET2_objetivo) num_TARGET2  from BBDD_testing2 group by  MADURACION_SALDO_testing2")
resumen_MADURACION_SALDO_training2<-sqldf("select MADURACION_SALDO_training2, count() num_clientes , sum(TARGET2_objetivo) num_TARGET2  from BBDD_training2 group by  MADURACION_SALDO_training2")

write.table(resumen_prob_testing2 ,file=paste("SALIDA/RF_smote2_back_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_saldo_testing2,file=paste("SALIDA/RF_smote2_back_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_MADURACION_SALDO_testing2 ,file=paste("SALIDA/RF_smote2_back_puntos_corte_mad_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

write.table(resumen_prob_training2 ,file=paste("SALIDA/RF_smote2_back_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_saldo_training2,file=paste("SALIDA/RF_smote2_back_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(resumen_MADURACION_SALDO_training2 ,file=paste("SALIDA/RF_smote2_back_puntos_corte_mad_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Tablas para el análisis
write.table(rbind(freq(BBDD_training2$prob_training2, useNA="ifany"),freq(BBDD_testing2$prob_testing2, useNA="ifany")),file=paste("SALIDA/RF_smote2_back_training2_puntos_corte_prob_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(rbind(freq(BBDD_training2$SAL_CAPITA_training2, useNA="ifany"),freq(BBDD_testing2$SAL_CAPITA_testing2, useNA="ifany")),file=paste("SALIDA/RF_smote2_back_training2_puntos_corte_saldo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(rbind(freq(BBDD_training2$MADURACION_SALDO_training2, useNA="ifany"),freq(BBDD_testing2$MADURACION_SALDO_testing2, useNA="ifany")),file=paste("SALIDA/RF_smote2_back_training2_puntos_corte_maduracion_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_training2B<-sqldf("select * from BBDD_training2 where TARGET2_objetivo=1 ")

write.table(rbind(table(BBDD_training2$SAL_CAPITA_training2, BBDD_training2$prob_training2, useNA="ifany"), table(BBDD_training2B$SAL_CAPITA_training2, BBDD_training2B$prob_training2, useNA="ifany")),file=paste("SALIDA/RF_smote2_tabla_saldo_prob_training2_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 
write.table(rbind(table(BBDD_training2$MADURACION_SALDO_training2, BBDD_training2$prob_training2, useNA="ifany"),table(BBDD_training2B$MADURACION_SALDO_training2, BBDD_training2B$prob_training2, useNA="ifany") ),file=paste("SALIDA/RF_smote2_tabla_maduracion_prob_training2_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 

BBDD_testing2B<-sqldf("select * from BBDD_testing2 where TARGET2_objetivo=1 ")
write.table(rbind(table(BBDD_testing2$SAL_CAPITA_testing2, BBDD_testing2$prob_testing2, useNA="ifany"), table(BBDD_testing2B$SAL_CAPITA_testing2, BBDD_testing2B$prob_testing2, useNA="ifany")),file=paste("SALIDA/RF_smote2_tabla_saldo_prob_testing2_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 
write.table(rbind(table(BBDD_testing2$MADURACION_SALDO_testing2, BBDD_testing2$prob_testing2, useNA="ifany"),table(BBDD_testing2B$MADURACION_SALDO_testing2, BBDD_testing2B$prob_testing2, useNA="ifany") ),file=paste("SALIDA/RF_smote2_tabla_maduracion_prob_testing2_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 

#Mas tablas para el analisis
names(BBDD_testing2)<-gsub("\\.", "_", names(BBDD_testing2))

tabla_negocio<-sqldf("select prob_testing2, count() clientes, avg(MAX_SAL_CAPITA_objetivo) saldo_promedio, MEDIAN(MAX_SAL_CAPITA_objetivo) AS MEDIAN_saldo, sum(TARGET2_objetivo) TARGET, avg(min_MADURACION_SALDO_objetivo) maduracion_promedio, MEDIAN(min_MADURACION_SALDO_objetivo) AS MEDIAN_maduracion from BBDD_testing2 group by prob_testing2")
write.table(tabla_negocio,file=paste("SALIDA/RF_smote2_tabla_negocio_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T) 

sqldf("select prob_testing2, churn.predict_training2, count() from BBDD_testing2 group by prob_testing2, churn.predict_training2")
