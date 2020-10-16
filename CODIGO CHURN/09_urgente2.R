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

#Cargue balanceo down
load(file="balanceo_down.Rdata")
freq(down_training$TARGET_objetivo, na.rm=TRUE)
freq(down_training2$TARGET2_objetivo, na.rm=TRUE)

gc()
eliminar<-c("Class")
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=down_training,  mtry = 16, ntree = 200,
                                    importance = TRUE, nodesize = 1 )
gc()
save(modelo_randomForest11, file="RF_down.Rdata")
gc()
down_training2<-down_training2[ , !(names(down_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=down_training2,  mtry = 16, ntree = 200,
                                    importance = TRUE, nodesize = 1 )
save(modelo_randomForest12, file="RF_down2.Rdata")

gc()
eliminar<-c("Class")
load(file="balanceo_up.Rdata")
eliminar<-c("Class")
up_training<-up_training[ , !(names(up_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=up_training,  mtry = 10, ntree = 100,
                                    importance = TRUE, nodesize = 1 )
gc()
save(modelo_randomForest11, file="RF_up.Rdata")
gc()
load(file="balanceo_up2.Rdata")
up_training2<-up_training2[ , !(names(up_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=up_training2,  mtry = 10, ntree = 50,
                                    importance = TRUE, nodesize = 1 )
save(modelo_randomForest12, file="RF_up2.Rdata")

gc()
load(file="balanceo_rose.Rdata")
eliminar<-c("Class")
rose_training<-rose_training[ , !(names(rose_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=rose_training,  mtry = 10, ntree = 50,
                                    importance = TRUE, nodesize = 1 )
gc()
save(modelo_randomForest11, file="RF_rose.Rdata")
gc()
load(file="balanceo_rose2.Rdata")
rose_training2<-rose_training2[ , !(names(rose_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=rose_training2,  mtry = 10, ntree = 100,
                                    importance = TRUE, nodesize = 1 )
save(modelo_randomForest12, file="RF_rose2.Rdata")
gc()

load(file="balanceo_smote.Rdata")
eliminar<-c("Class")
smote_training<-smote_training[ , !(names(smote_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=smote_training,  mtry = 17, ntree = 200,
                                    importance = TRUE, nodesize = 1 )
gc()
save(modelo_randomForest11, file="RF_smote.Rdata")
gc()
load(file="balanceo_smote2.Rdata")
smote_training2<-smote_training2[ , !(names(smote_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=smote_training2,  mtry = 17, ntree = 200,
                                    importance = TRUE, nodesize = 1 )
save(modelo_randomForest12, file="RF_smote2.Rdata")
gc()

