#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","xlsx","summarytools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()
sub_ruta="./SUPERBASE_OBJETIVO/";
lista_SUPERBASE<-list.files(sub_ruta,pattern="SUPERBASE_")
ventana_back<-6
load("tabla_mes.Rdata")
super_super<-list()

ruta<-"E:/Jennyfer_C/04_CHURN/BBDD/tercera_entrega"
setwd(ruta)

