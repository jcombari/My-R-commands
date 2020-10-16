#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR","zoo", "ROCR","randomForest")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Carguemos Balanceo down
load(file="Balanceo_down.Rdata")

#Para lo siguiente se requiere tener la BBDD de testing
load(file="pre_balanceo2.Rdata")

var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
training <-train[ , !(names(train) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo") )]
training2 <-train[ , !(names(train) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo") )]

testing <-test[ , !(names(test) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo") )]
testing2 <-test[ , !(names(test) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo") )]



#Modelamiento: RandomForest
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=down_training,  ntree=500, nsplit=10)
plot(modelo_randomForest11)
gc()

down_training2<-down_training2[ , !(names(down_training) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=down_training2, ntree=500, nsplit=10)
plot(modelo_randomForest12)
gc()
save(modelo_randomForest11, modelo_randomForest12, file="modelo_random_down.Rdata" )

#Modelamiento regresión logística
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
modelo_logistic21<-glm(TARGET_objetivo ~., family=binomial(link='logit'),data=down_training, maxit = 100)
#plot(modelo_logistic21)
gc()
#print(summary(modelo_logistic21))
#anova(modelo_logistic21, test="Chisq")
# fitted.results <- predict(modelo_logistic21,newdata=testing)
# misClasificError <- mean(fitted.results != testing$TARGET_objetivo)
# print(paste('Logistic Regression Accuracy',1-misClasificError))
# print("Confusion Matrix for Logistic Regression"); table(testing$TARGET_objetivo, fitted.results > 0.5)


down_training2<-down_training2[ , !(names(down_training2) %in% eliminar)]
modelo_logistic22<-glm(TARGET2_objetivo ~., family=binomial(link='logit'),data=down_training2)
#plot(modelo_logistic22)
#gc()

save(modelo_logistic21, modelo_logistic22, file="modelo_logistic_down.Rdata" )

#Funcion para analizar modelo con fidelizacion
#Ejemplo parametros_fid(modelo_randomForest11, testing) 
parametros<-function(modelo,testing){
  #plot(modelo)
  #varImpPlot(modelo, sort = T, main="Variable Importance")
  VI<-as.data.frame(unlist(importance(modelo)))
  VI<-VI[order( -VI$MeanDecreaseGini),] 
  varImp(modelo)
  importanceOrder<-order(-modelo$importance)
  importanceOrder<-importanceOrder[1:10]
  names<-rownames(modelo$importance)[importanceOrder]
  # Evaluation metrics  
  modelo.pred      <- predict(modelo, newdata = testing, type = "class")  
  modelo.result      <- confusionMatrix(data = modelo.pred, testing$TARGET_objetivo)  
  modelo.precision <- modelo.result$byClass['Pos Pred Value']  
  modelo.recall    <- modelo.result$byClass['Sensitivity']  
  modelo.F1        <- modelo.result$byClass['F1']
  
  #Curva roc
  #probabilidad modelo
  probs<-predict(modelo,testing, type="prob")[,2]
  #prediccion
  pred<-prediction(probs,testing$TARGET_objetivo)
  #Perfomance dle objeto
  pe<-performance(pred,"tpr", "fpr")
  
  #Area sobre la curva (AUC)
  
  au<-performance(pred,"auc")@y.values[[1]]
  
  pd<-data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  #dev.off()
  p<-ggplot(pd, aes(x=fpr, y=tpr))
  p<-p+geom_line(colour="red")
  p<-p+xlab("Falsos Positivos")+ylab("Verdaderos positivos")
  p<-p+ggtitle("Curva Roc")
  #p<-p+theme(plot.title = element_test(size=10))
  p<-p+annotate("text",x=0.5, y=0.0, hjust=0, vjust=0,size=5,label=paste("AUC =" ,round(au,2)))
  print(p)
  aux<-list( modelo.result, modelo.precision, modelo.precision, modelo.recall,modelo.F1)
  return(aux)
}

#Funcion para analizar modelo con fidelizacion
#Ejemplo parametros_fid(modelo_randomForest12, testing2) 
parametros_fid<-function(modelo,testing){
  #plot(modelo)
  #varImpPlot(modelo, sort = T, main="Variable Importance")
  VI<-as.data.frame(unlist(importance(modelo)))
  VI<-VI[order( -VI$MeanDecreaseGini),] 
  varImp(modelo)
  importanceOrder<-order(-modelo$importance)
  importanceOrder<-importanceOrder[1:10]
  names<-rownames(modelo$importance)[importanceOrder]
  # Evaluation metrics  
  modelo.pred      <- predict(modelo, newdata = testing, type = "class")  
  modelo.result      <- confusionMatrix(data = modelo.pred, testing$TARGET2_objetivo)  
  modelo.precision <- modelo.result$byClass['Pos Pred Value']  
  modelo.recall    <- modelo.result$byClass['Sensitivity']  
  modelo.F1        <- modelo.result$byClass['F1']

  
  #Curva roc
  #probabilidad modelo
  probs<-predict(modelo,testing, type="prob")[,2]
  #prediccion
  pred<-prediction(probs,testing$TARGET2_objetivo)
  #Perfomance dle objeto
  pe<-performance(pred,"tpr", "fpr")
  
  #Area sobre la curva (AUC)
  
  au<-performance(pred,"auc")@y.values[[1]]
  
  pd<-data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  #dev.off()
  p<-ggplot(pd, aes(x=fpr, y=tpr))
  p<-p+geom_line(colour="red")
  p<-p+xlab("Falsos Positivos")+ylab("Verdaderos positivos")
  p<-p+ggtitle("Curva Roc churn + fidelización")
  #p<-p+theme(plot.title = element_test(size=10))
  p<-p+annotate("text",x=0.5, y=0.0, hjust=0, vjust=0,size=5,label=paste("AUC =" ,round(au,2)))
  print(p)
  aux<-list( modelo.result, modelo.precision, modelo.precision, modelo.recall,modelo.F1)
  return(aux)
}
