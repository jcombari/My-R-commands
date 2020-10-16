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

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

#Carguemos Balanceo up
load(file="Balanceo_rose.Rdata")
load(file="balanceo_rose_extras.Rdata")

#### Logistic regresión
gc()
eliminar<-c("Class")
incluir<-c("TARGET_objetivo","min_RESULTADO_SCORE_huellas","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_RECUPE_CARTERA_HIST_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","ESTADO_CONSULTA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","CTAS_CTES_ACT_CTA_BANCA_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro")


rose_training<-rose_training[ , (names(rose_training) %in% incluir)]
rose_training<-rose_training[ , !(names(rose_training) %in% eliminar)]
#Fitting the Logistic Regression Model:
modelo_logistic_11_full <- glm(TARGET_objetivo ~.,family=binomial(link="logit"),data=rose_training)
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

png(file =paste0("SALIDA/RF_rose_ROC_testing_",format(today, format="%Y%m%d"),".png", sep=""), bg = "transparent")
plot(prf ,main=paste("ROC Curve for Random Forest Testing AUC=",  round(auc,2) ) ,col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()
save(modelo_logistic_11, file="RL_rose.Rdata")
save(modelo_logistic_11, fitted.results.prob, fitted.results,confusionMatrix, pr, auc,file="salida/LR_rose_metrics_testing.Rdata")
capture.output(print(modelo_logistic_11, na.print=''), file=paste("SALIDA/LR_rose_model_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(confusionMatrix , na.print=''), file=paste("SALIDA/LR_rose_confusion_testing_",format(today, format="%Y%m%d"),".txt", sep=""))

