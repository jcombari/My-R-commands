#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Carga de Base de datos

Buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")
save(Buro, file = "buro.RData")
sqldf("select Fecha, count(FECHA_DATA) from Buro group by FECHA_DATA")

library(xlsx)
write.xlsx(unlist(names(Buro)),file=paste0("SALIDA/buro_nombre_variables_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
write.xlsx(unlist(names(SUPERBASE_ABR17_ENC)),file=paste0("SALIDA/super_super_variables_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)


ESTADO_CONSULTA
sqldf("select ESTADO_CONSULTA, count(ESTADO_CONSULTA) from Buro group by ESTADO_CONSULTA")

sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from BBDD2 group by TOTAL_CONSULTAS_ULT_6_MESES")


sqldf("select FECHA_DATA,ESTADO_CONSULTA, count(ESTADO_CONSULTA) from Buro group by FECHA_DATA, ESTADO_CONSULTA order by FECHA_DATA, ESTADO_CONSULTA ")


load("superbase.RData")


buro_subset<-as.data.frame(Buro[,c("TIPO_ID","TOTAL_CONSULTAS_ULT_6_MESES","CEDULAENC")])

BBDD2<-merge(x=super_super, y=buro_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

BBDD2$largo_contrato<-str_length(BBDD2$OBLIGACIONENC)

sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from BBDD2 group by TOTAL_CONSULTAS_ULT_6_MESES")

sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from BBDD2 group by TOTAL_CONSULTAS_ULT_6_MESES")

sqldf("select FECHA_DATA, count(FECHA_DATA) from Buro group by FECHA_DATA")

sqldf("select FECHA_ENVIO, count(FECHA_ENVIO) from Buro group by FECHA_ENVIO")




