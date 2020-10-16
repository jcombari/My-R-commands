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

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

#Carguemos Balanceo down
load(file="Balanceo_down.Rdata")
load(file="balanceo_down_extras.Rdata")

#### Logistic regresión
gc()
eliminar<-c("Class")
incluir<-c("TARGET_objetivo","MAX_SAL_CAPITA_objetivo","TOTAL_CONSULTAS_ULT_6_MESES_buro","min_MADURACION_SALDO_objetivo","QUANTO2_buro","VLR_CUOTA_objetivo","RANGO_APROXIMADO_EDAD_buro","VALOR_CUOTAS_CB_buro","ACIERTA_A_FINANCIERO_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro")
down_training<-down_training[ , (names(down_training) %in% incluir)]
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
#Fitting the Logistic Regression Model:
modelo_logistic_11_full <- glm(TARGET_objetivo ~.,family=binomial(link="logit"),data=down_training)
print(summary(modelo_logistic_11_full))
fitted.results.prob <- predict(modelo_logistic_11_full,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results.prob > 0.5,1,0)

confusionMatrix(factor(fitted.results,levels=0:1), factor(testing$TARGET_objetivo,levels=0:1), positive = "1")

rm("fitted.results.prob","fitted.results")

#new model (optimize model by finding the min. AIC value)
library(MASS)
modelo_logistic_11 <-  stepAIC(modelo_logistic_11_full, trace = 0)
summary(modelo_logistic_11)

#Feature Analysis:
anova(modelo_logistic_11, test="Chisq")

fitted.results.prob <- predict(modelo_logistic_11,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results.prob > 0.5,1,0)
confusionMatrix<-confusionMatrix(factor(fitted.results,levels=0:1), factor(testing$TARGET_objetivo,levels=0:1), positive = "1")

pr <- prediction(fitted.results.prob, testing$TARGET_objetivo)

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
save(modelo_logistic_11, file="RL_down.Rdata")
save(modelo_logistic_11, fitted.results.prob, fitted.results,confusionMatrix, pr, auc,file="salida/LR_down_metrics_testing.Rdata")
capture.output(print(modelo_logistic_11, na.print=''), file=paste("SALIDA/LR_down_model_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix , na.print=''), file=paste("SALIDA/LR_down_confusion_testing_",format(today, format="%Y%m%d"),".txt", sep=""))





