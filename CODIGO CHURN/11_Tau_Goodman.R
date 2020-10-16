#Limpieza 
rm(list=ls())
.rs.restartR() #restart

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR","zoo", "ROCR","randomForest","CAvariantes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cagar la base
load(file="pre_balanceo2.Rdata")
a<-sapply(train, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero
a<-sapply(test, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero

#Eliminación Variables
var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
eliminar<-c("OBLIGA_MORA_60_CARTERA_ACTUAL_buro",
            "OBLIGA_MORA_90_CARTERA_ACTUAL_buro",
            "CTAS_EN_COBRADOR_CARTERA_ACT_buro",
            "VAL_MORASECTORBANCASIN_POPULAR_buro",
            "VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro",
            "VAL_MORA_SECTOR_HIP_SIN_POPU_buro", "CIUDAD_DE_EXPEDICION_buro","COD_PAG_objetivo ","min_MADURACION_CUOTA_objetivo")

training <-train[ , !(names(train) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo",eliminar) )]
training2 <-train2[ , !(names(train2) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo",eliminar) )]

testing <-test[ , !(names(test) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo", eliminar))]
testing2 <-test2[ , !(names(test2) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo",eliminar) )]

BBDD<-rbind(training,testing)
eliminar_target1<-c("TARGET_objetivo")

BBDD_VARIABLES<-BBDD[,!(names(BBDD) %in% eliminar_target1)]
variables<-names(BBDD_VARIABLES)
require(GoodmanKruskal)

#funcion para calcular decil
decil <- function(x) {
  quantile(x, probs = seq(0,1, by=0.1),na.rm = TRUE)
}

list_tau<-""
for(variable in variables){
  if(is.numeric(BBDD[[variable]])){
    aux_decil<-''
    aux<-''
    aux_decil<-unique(decil(BBDD[[variable]]))
    aux <-  cut(BBDD[[variable]], breaks=aux_decil,include.lowest=TRUE,dig.lab=10)
    list_tau<-rbind(cbind(variable, GKtau(aux, BBDD$TARGET_objetivo)), list_tau)}
  else{
    list_tau<-rbind(cbind(variable, GKtau(BBDD[[variable]], BBDD$TARGET_objetivo)), list_tau)
  }
 print(variable)
}

write.table(list_tau ,file=paste("SALIDA/salida_tau_GK_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

list_chisq<-""
for(variable in variables){
  aux2<-''
  if(is.numeric(BBDD[[variable]])){
    aux_decil<-''
    aux<-''
    aux_decil<-unique(decil(BBDD[[variable]]))
    aux <-  cut(BBDD[[variable]], breaks=aux_decil,include.lowest=TRUE,dig.lab=10)
    tabla<-table(aux, BBDD$TARGET_objetivo,  useNA = "ifany")
    aux2<-chisq.test(tabla)
    list_chisq<-rbind(cbind(variable, aux2[1],aux2[2],aux2[3] ), list_chisq)
  }
  else{
    tabla<-table(BBDD[[variable]], BBDD$TARGET_objetivo,  useNA = "ifany")
    aux2<-chisq.test(tabla)
    list_chisq<-rbind(cbind(variable, aux2[1],aux2[2],aux2[3] ), list_chisq)
  }
  print(variable)
}

write.table(list_chisq,file=paste("SALIDA/salida_chisq_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
