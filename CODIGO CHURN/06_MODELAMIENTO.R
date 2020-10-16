#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR","zoo", "ROCR")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()


#Modelamiento: RandomForest
library(randomForest)
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=down_training,  ntree=500, nsplit=10)
plot(modelo_randomForest11)
gc()


down_training2<-down_training2[ , !(names(down_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=down_training2, ntree=500, nsplit=10)
plot(modelo_randomForest12)
gc()




#save(modelo_randomForest11, modelo_randomForest12, file="modelo_random_down.Rdata" )

#modelamiento Balanceo 1
load(file="Random_model_down.Rdata")
#Random Forest
library(randomForest)
set.seed(1976)
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]

plot(modelo_randomForest11)
varImpPlot(modelo_randomForest11, sort = T, main="Variable Importance")
VI<-as.data.frame(unlist(importance(modelo_randomForest11)))
VI<-VI[order( -VI$MeanDecreaseGini),] 
varImp(modelo_randomForest11)
importanceOrder<-order(-modelo_randomForest11$importance)
importanceOrder<-importanceOrder[1:10]
names<-rownames(modelo_randomForest11$importance)[importanceOrder]

par(mfrow=c(5, 2), xpd=NA)
# for (name in names){
#   partialPlot(modelo_randomForest11,down_training , eval(name), main=name, xlab=name,ylim=c(-.2,.9))
# }

# Evaluation metrics  
randomForest11.pred      <- predict(modelo_randomForest11, newdata = testing, type = "class")  
randomForest11.result      <- confusionMatrix(data = randomForest11.pred, testing$TARGET_objetivo)  
randomForest11.precision <- randomForest11.result$byClass['Pos Pred Value']  
randomForest11.recall    <- randomForest11.result$byClass['Sensitivity']  
randomForest11.F1        <- randomForest11.result$byClass['F1']

#Curva roc
library(ROCR)
#probabilidad modelo
probs<-predict(modelo_randomForest11,testing, type="prob")[,2]
#prediccion
pred<-prediction(probs,testing$TARGET_objetivo)
#Perfomance dle objeto
pe<-performance(pred,"tpr", "fpr")

#Area sobre la curva (AUC)

au<-performance(pred,"auc")@y.values[[1]]

pd<-data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))

p<-ggplot(pd, aes(x=fpr, y=tpr))
p<-p+geom_line(colour="red")
p<-p+xlab("False Posite Rate")+ylab("True Posite Rate")
p<-p+ggtitle("Curva Roc")
#p<-p+theme(plot.title = element_test(size=10))
print(p)


