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
load(file="super_HUELLAS_0.DAT")

super_HUELLAS$FECHA_HUELLA2<-as.numeric(gsub("-", "", format(as.Date(super_HUELLAS$FECHA_HUELLA), "%Y-%m")))

huellas<-sqldf("select No__IDENTIFICACION, ESTADO , FECHA_HUELLA2 from super_HUELLAS")

freq(huellas$ESTADO, useNA="ifany")
huellas$marca<-ifelse(huellas$ESTADO !="VIGENTE",1,0)
freq(huellas$ESTADO , useNA="ifany")
freq(huellas$marca, useNA="ifany")
huellas<-sqldf("select * from huellas where marca=1")

names(huellas)<-paste0(names(huellas),"_prueba")

huellas<-huellas[order(huellas$No__IDENTIFICACION_prueba, -huellas$FECHA_HUELLA2_prueba),] #ordeno por clave=cedula+fecha

huellas<-huellas[!duplicated(huellas$No__IDENTIFICACION_prueba), ] #luego de ordenamos, elimino dupicados

BBDD3<-merge(x=BBDD3, y=huellas, by.x=c("CEDULAENC_objetivo", "fecha_objetivo" ), by.y=c("No__IDENTIFICACION_prueba","FECHA_HUELLA2_prueba"), all.x = TRUE)

freq(BBDD3$marca_prueba, useNA="ifany")

BBDD3$marca_prueba[is.na(BBDD3$marca_prueba)] <- 0

BBDD3<-sqldf("select * from BBDD3 where marca_prueba=0")

input.list<-names(BBDD3)

eliminar<-input.list[grep("*prueba*", input.list)]

BBDD3<-BBDD3[ , !(names(BBDD3) %in% eliminar)]

#save(BBDD2, BBDD3,file = "base_objetivo_fid2_Julio_agosto.RData")

