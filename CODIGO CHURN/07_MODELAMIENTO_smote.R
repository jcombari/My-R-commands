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
#Para lo siguiente se requiere tener la BBDD de testing
load(file="pre_balanceo2.Rdata")

var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
training <-train[ , !(names(train) %in% c(var_identificacion,"TARGET22_objetivo","COD_PAG_objetivo","CTAS_EN_COBRADOR_CARTERA_ACT_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro") )]
training2 <-train[ , !(names(train) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo","CTAS_EN_COBRADOR_CARTERA_ACT_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro") )]

testing <-test[ , !(names(test) %in% c(var_identificacion,"TARGET22_objetivo","COD_PAG_objetivo","CTAS_EN_COBRADOR_CARTERA_ACT_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro") )]
testing2 <-test[ , !(names(test) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo","CTAS_EN_COBRADOR_CARTERA_ACT_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro") )]

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}


#Balanceo
load(file="balanceo_smote2.Rdata")
gc()

# 
# smote_training2<-smote_training[ , !(names(smote_training) %in% eliminar)]
# trees<-c(20,50,100,200)
# list_auc<-''
# for(i in trees){
#     aux<-''
#     modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=smote_training,  ntree=i)
#     probs<-predict(modelo_randomForest12,testing, type="prob")[,2]
#     #prediccion
#     pred<-prediction(probs,testing$TARGET2_objetivo)
#     au<-performance(pred,"auc")@y.values[[1]]
#     aux<-list(i,au)
#     list_auc<-cbind(list_auc,aux)
#     print(paste0("tree=",i))
#     print(paste0("auc=", au))
# }
# write.table(list_auc,file=paste("SALIDA/Random_forest_smote_iteracion",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
# 
# list_auc_fid<-''
# smote_training2<-smote_training2[ , !(names(smote_training2) %in% eliminar)]
# 
# for(i in trees){
#     aux<-''
#     modelo_randomForest12<-randomForest(TARGET22_objetivo ~., data=smote_training2,  ntree=i)
#     probs<-predict(modelo_randomForest12,testing2, type="prob")[,2]
#     #prediccion
#     pred<-prediction(probs,testing2$TARGET22_objetivo)
#     au<-performance(pred,"auc")@y.values[[1]]
#     aux<-list(i,au)
#     list_auc_fid<-cbind(list_auc_fid,aux)
#     print(paste0("tree=",i))
#     print(paste0("auc=", au))
# }

#write.table(list_auc_fid,file=paste("SALIDA/Random_forest_smote_iteracion_fid",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Modelamiento: RandomForest

modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=smote_training2, ntree=200, nsplit=10)
#save(smote_training,testing ,training, modelo_randomForest12,  file="modelo_random11_smote.Rdata" )
#load(file="modelo_random11_smote.Rdata" ) 
     
#Analisis

varImpPlot(modelo_randomForest12, sort=T, n.var = 10, main ='Top 10 - variables - bosque aleatorio - churn')
write.table(importance(modelo_randomForest12),file=paste("SALIDA/Random_forest_importance_smote_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

plot(modelo_randomForest12$err.rate[,1], log = "y") 
modelo_randomForest12$err.rate[50,1]

print(modelo_randomForest12)

# Evaluation metrics testing
modelo.pred_testing      <- predict(modelo_randomForest12, newdata = testing, type = "class") 
modelo.prob_testing <-predict(modelo_randomForest12,testing, type="prob")[,2]
write.table(modelo.pred_testing ,file=paste("SALIDA/Random_forest_pred_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_testing        <- confusionMatrix(data = modelo.pred_testing, testing$TARGET2_objetivo)  
modelo.F1_testing          <- modelo.result_testing  $byClass['F1']
save(modelo.pred_testing   ,modelo.result_testing   , modelo.F1_testing    , file="Random_forest_smote_testing.Rdata")

print("Matriz de confusion")
print(modelo.result_testing$table)

# Evaluation metrics training
modelo.pred_training      <- predict(modelo_randomForest12, newdata = training, type = "class") 
modelo.prob_training <-predict(modelo_randomForest12,training, type="prob")[,2]

write.table(modelo.pred_training ,file=paste("SALIDA/Random_forest_pred_training",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_training      <- confusionMatrix(data = modelo.pred_training, training$TARGET2_objetivo)  
modelo.F1_training        <- modelo.result_training$byClass['F1']

print("Matriz de confusion")
print(modelo.result_training$table)

save(modelo.pred_training  ,modelo.result_training  , modelo.F1_training    , file="Random_forest_smote_training.Rdata")

#Analisis de puntos de corte



modelo.prob_training_tram <-  cut(modelo.prob_training , breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
freq(modelo.prob_training_tram ,usena="ifany")

BBDD_training<-as.data.frame(cbind( modelo.pred_training , modelo.prob_training, training$TARGET2_objetivo ,training$MAX_SAL_CAPITA_objetivo, training$min_MADURACION_SALDO_objetivo))

names(BBDD_training)<-c("modelo.pred_training", "modelo.prob_training","TARGET2_objetivo","MAX_SAL_CAPITA_objetivo","min_MADURACION_SALDO_objetivo")

decil_prob<-decil(BBDD_training$modelo.prob_training)
BBDD_training$modelo_prob_training_tram<-  cut(BBDD_training$modelo.prob_training, breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
decil_saldo<-decil(training$MAX_SAL_CAPITA_objetivo)
BBDD_training$MAX_SAL_CAPITA_objetivo_tram<- cut(BBDD_training$MAX_SAL_CAPITA_objetivo, breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
decil_maduracion<-decil(BBDD_training$min_MADURACION_SALDO_objetivo)
BBDD_training$min_MADURACION_SALDO_objetivo_tram<- cut(BBDD_training$min_MADURACION_SALDO_objetivo, breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

#clientes<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training grosmote by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
clientes<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training grosmote by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")

#freq(BBDD_training$TARGET2_objetivo,useNA="ifany")

BBDD_training2<-sqldf("select * from BBDD_training where TARGET2_objetivo=2 ")
#churn<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 grosmote by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
churn<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 grosmote by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")


#churn<-table(BBDD_training2$MAX_SAL_CAPITA_objetivo_tram,BBDD_training2$modelo.prob_training_tram, useNA="ifany")

write.table(clientes,file=paste("SALIDA/Random_forest_smote_tabla_seg_clientes",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(churn,file=paste("SALIDA/Random_forest_smote_tabla_seg_churn",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_testing<-as.data.frame(cbind(test$CEDULAENC_objetivo,test$TARGET2_objetivo, modelo.pred_testing , modelo.prob_testing ))

names(BBDD_testing)<-c("CEDULAENC", "TARGET2","modelo.pred_testing", "modelo.prob_testing")
save(BBDD_testing, BBDD_training, file="random_forest11_smote.Rdata")
table(BBDD_testing$TARGET2,BBDD_testing$modelo.pred_testing, useNA="ifany")

write.table(BBDD_testing,file=paste("SALIDA/Random_forest_smote_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(BBDD_training,file=paste("SALIDA/Random_forest_smote_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

### Modelo con fidelizaciones
eliminar<-c("Class")
smote_training2<-smote_training2[ , !(names(smote_training2) %in% eliminar)]
modelo_randomForest12<-randomForest(TARGET22_objetivo ~., data=smote_training2, ntree=200)
#plot(modelo_randomForest12)
gc()

save(smote_training,testing ,training, modelo_randomForest12,  file="modelo_random12_smote.Rdata" )

#Analisis modelo
varImpPlot(modelo_randomForest12, sort=T, n.var = 10, main ='Top 10 - variables - bosque aleatorio - churn')
write.table(importance(modelo_randomForest12),file=paste("SALIDA/Random_forest_importance_smote_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

plot(modelo_randomForest12$err.rate[,1], log = "y") 
modelo_randomForest12$err.rate[200,1]

print(modelo_randomForest12)

# Evaluation metrics testing
modelo.pred_testing      <- predict(modelo_randomForest12, newdata = testing, type = "class") 
modelo.prob_testing <-predict(modelo_randomForest12,testing, type="prob")[,2]
write.table(modelo.pred_testing ,file=paste("SALIDA/Random_forest_pred_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_testing        <- confusionMatrix(data = modelo.pred_testing, testing$TARGET2_objetivo)  
modelo.F1_testing          <- modelo.result_testing  $byClass['F1']
save(modelo.pred_testing   ,modelo.result_testing   , modelo.F1_testing    , file="Random_forest_smote_testing.Rdata")

print("Matriz de confusion")
print(modelo.result_testing$table)

# Evaluation metrics training
modelo.pred_training      <- predict(modelo_randomForest12, newdata = training, type = "class") 
modelo.prob_training <-predict(modelo_randomForest12,training, type="prob")[,2]

write.table(modelo.pred_training ,file=paste("SALIDA/Random_forest_pred_training",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
modelo.result_training      <- confusionMatrix(data = modelo.pred_training, training$TARGET2_objetivo)  
modelo.F1_training        <- modelo.result_training$byClass['F1']

print("Matriz de confusion")
print(modelo.result_training$table)

save(modelo.pred_training  ,modelo.result_training  , modelo.F1_training    , file="Random_forest_smote_training.Rdata")

#Analisis de puntos de corte



modelo.prob_training_tram <-  cut(modelo.prob_training , breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
freq(modelo.prob_training_tram ,usena="ifany")

BBDD_training<-as.data.frame(cbind( modelo.pred_training , modelo.prob_training, training$TARGET2_objetivo ,training$MAX_SAL_CAPITA_objetivo, training$min_MADURACION_SALDO_objetivo))

names(BBDD_training)<-c("modelo.pred_training", "modelo.prob_training","TARGET2_objetivo","MAX_SAL_CAPITA_objetivo","min_MADURACION_SALDO_objetivo")

decil_prob<-decil(BBDD_training$modelo.prob_training)
BBDD_training$modelo_prob_training_tram<-  cut(BBDD_training$modelo.prob_training, breaks=decil_prob,include.lowest=TRUE,dig.lab=10)
decil_saldo<-decil(training$MAX_SAL_CAPITA_objetivo)
BBDD_training$MAX_SAL_CAPITA_objetivo_tram<- cut(BBDD_training$MAX_SAL_CAPITA_objetivo, breaks=decil_saldo,include.lowest=TRUE,dig.lab=10)
decil_maduracion<-decil(BBDD_training$min_MADURACION_SALDO_objetivo)
BBDD_training$min_MADURACION_SALDO_objetivo_tram<- cut(BBDD_training$min_MADURACION_SALDO_objetivo, breaks=decil_maduracion,include.lowest=TRUE,dig.lab=10)

#clientes<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training grosmote by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
clientes<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() clientes from BBDD_training grosmote by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")

#freq(BBDD_training$TARGET2_objetivo,useNA="ifany")

BBDD_training2<-sqldf("select * from BBDD_training where TARGET2_objetivo=2 ")
#churn<-sqldf("select MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 grosmote by MAX_SAL_CAPITA_objetivo_tram, modelo_prob_training_tram")
churn<-sqldf("select min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram, count() churn from BBDD_training2 grosmote by min_MADURACION_SALDO_objetivo_tram, modelo_prob_training_tram")


#churn<-table(BBDD_training2$MAX_SAL_CAPITA_objetivo_tram,BBDD_training2$modelo.prob_training_tram, useNA="ifany")

write.table(clientes,file=paste("SALIDA/Random_forest_smote_tabla_seg_clientes",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(churn,file=paste("SALIDA/Random_forest_smote_tabla_seg_churn",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_testing<-as.data.frame(cbind(test$CEDULAENC_objetivo,test$TARGET2_objetivo, modelo.pred_testing , modelo.prob_testing ))

names(BBDD_testing)<-c("CEDULAENC", "TARGET2","modelo.pred_testing", "modelo.prob_testing")
save(BBDD_testing, BBDD_training, file="random_forest11_smote.Rdata")
table(BBDD_testing$TARGET2,BBDD_testing$modelo.pred_testing, useNA="ifany")

write.table(BBDD_testing,file=paste("SALIDA/Random_forest_smote_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
write.table(BBDD_training,file=paste("SALIDA/Random_forest_smote_BBDD_testing",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
