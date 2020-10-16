#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")

if(length(new.packages)) install.packages(new.packages)lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()




#colocacion<-read.csv(gsub(" ", "",paste("COLOCACION/","PLEXUS_COLOCACION_ENC")), header = T,sep=";")
#save(colocacion, file = "colocacion.RData")

#load("colocacion.RData")
#write.xlsx(unlist(names(colocacion)),file=paste0("SALIDA/colocacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Cruces captacion
load("captacion.RData")
#Ojo captacion es productos del pasivo por lo tanto solo debe cruzar a nivel de cliente
captacion_subset<-as.data.frame(captacion[,c("CEDULAENC","OBLIGACIONENC","FEC_APECUENTA")])
#cedula
colocacion_captacion_ced<-merge(x=colocacion, y=captacion_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )
colocacion_captacion_ced2<-sqldf("select FEC_APECUENTA, count(FEC_APECUENTA) from colocacion_captacion_ced group by FEC_APECUENTA")
#credito
colocacion_captacion_cred<-merge(x=colocacion, y=captacion_subset, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC", all.x=TRUE )
colocacion_captacion_cred2<-sqldf("select NOM_PRODUCTO, count(NOM_PRODUCTO) from colocacion_captacion_cred group by NOM_PRODUCTO")

#Validando cruce con buro a nivel de cliente
load("buro.RData")
buro_subset<-as.data.frame(Buro[,c("TIPO_ID","TOTAL_CONSULTAS_ULT_6_MESES","CEDULAENC")])
colocacion_buro<-merge(x=colocacion, y=buro_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )
colocacion_buro<-sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from colocacion_buro group by TOTAL_CONSULTAS_ULT_6_MESES")

#Validando cruce con superbase a nivel de cliente
#load("superbase.RData")
super_subset<-as.data.frame(super_super[,c("CALIF_CART","CEDULAENC","OBLIGACIONENC")])
#Cedula
colocacion_super_ced<-merge(x=colocacion, y=super_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )
colocacion_super_ced2<-sqldf("select CALIF_CART, count(CALIF_CART) from colocacion_super_ced group by CALIF_CART")
#Credito
colocacion_super_cred<-merge(x=colocacion, y=super_subset, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC", all.x=TRUE )
colocacion_super_cred2<-sqldf("select CALIF_CART, count(CALIF_CART) from colocacion_super_cred group by CALIF_CART")

#archivo prepago
load("prepago.RData")

prepago_subset<-as.data.frame(prepagos[,c("OBLIGACIONENC","CEDULAENC","NOM_UNIDAD")])
#Cedula
colocacion_prepago_ced<-merge(x=colocacion, y=prepago_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )
colocacion_prepago_ced2<-sqldf("select NOM_UNIDAD, count(NOM_UNIDAD) from colocacion_prepago_ced group by NOM_UNIDAD")
#Credito
colocacion_prepago_cred<-merge(x=prepago, y=super_subset, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC", all.x=TRUE )
colocacion_prepago_cred2<-sqldf("select NOM_UNIDAD, count(NOM_UNIDAD) from colocacion_prepago_cred group by NOM_UNIDAD")



