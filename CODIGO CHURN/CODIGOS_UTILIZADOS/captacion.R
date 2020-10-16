#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()




captacion<-read.csv(gsub(" ", "",paste("CAPTACION/","PLEXUS_CAPTACION_ENC")), header = T,sep=";")

save(captacion, file = "captacion.RData")

write.xlsx(unlist(names(captacion)),file=paste0("SALIDA/captacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Validaciones

captacion_subset<-captacion[,c("COD_PRODUCTO",  "NOM_PRODUCTO",     "COD_OFICINA",      "NOM_OFICINA",      "FEC_APECUENTA" , "NOM_OFICINAH" ,    "NOM_CIUDAD"  ,     "NOMBRE_REGIONAL"  ,"NOM_DEPARTAMENTO", "CIUDAD_OFICINA"  )
]
freq_tables <- lapply(captacion_subset, freq)
capture.output(freq_tables, file=paste("SALIDA/captacion_",format(today, format="%Y%m%d"),".txt", sep=""))

Buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")

buro_subset<-as.data.frame(Buro[,c("TIPO_ID","TOTAL_CONSULTAS_ULT_6_MESES","CEDULAENC")])

captacion2<-merge(x=captacion, y=buro_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from captacion2 group by TOTAL_CONSULTAS_ULT_6_MESES")

super_subset<-as.data.frame(super_super[,c("CALIF_CART","CEDULAENC")])

captacion3<-merge(x=captacion, y=super_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

sqldf("select CALIF_CART, count(CALIF_CART) from captacion3 group by CALIF_CART")

load("prepago.RData")

prepago_subset<-as.data.frame(prepagos[,c("OBLIGACIONENC","NOM_UNIDAD")])

captacion$largo_contrato<-str_length(captacion$OBLIGACIONENC)

sqldf("select largo_contrato, count(largo_contrato) from captacion group by largo_contrato")

prepagos$largo_contrato<-str_length(prepagos$OBLIGACIONENC)

sqldf("select largo_contrato, count(largo_contrato) from prepagos group by largo_contrato")

#load("prepago.RData")

prepago_subset<-as.data.frame(prepagos[,c("CEDULAENC","OBLIGACIONENC","NOM_UNIDAD")])

captacion4<-merge(x=captacion, y=prepago_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

sqldf("select NOM_UNIDAD, count(NOM_UNIDAD) from captacion4 group by NOM_UNIDAD")

captacion_sin_prepago<-captacion4[!is.na(captacion4[,"NOM_UNIDAD"]),]
library(xlsx)
write.xlsx(captacion_sin_prepago,file=paste0("SALIDA/captacion_sin_prepago_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

captacion5<-merge(x=captacion, y=prepago_subset, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC", all.x=TRUE )

sqldf("select NOM_UNIDAD, count(NOM_UNIDAD) from captacion5 group by NOM_UNIDAD")

