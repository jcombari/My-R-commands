#Esteprograma debe ser ejecutado antes de la etapa 5_modelamiento

#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

load(file= "base_objetivo_fid2_Julio_agosto.RData")

nums<-sapply(BBDD3, is.numeric)

aux<-BBDD3[,nums]

variables<-names(aux)

for(variable in variables){
  BBDD3[[variable]]<-ifelse(BBDD3[[variable]]<0,0,BBDD3[[variable]])
}

#save(BBDD2, BBDD3,file = "base_objetivo_fid2_Julio_agosto.RData")

