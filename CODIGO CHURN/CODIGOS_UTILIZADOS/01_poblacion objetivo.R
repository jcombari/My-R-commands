#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","xlsx")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cargamos la información de colocación

#colocacion<-read.csv(gsub(" ", "",paste("COLOCACION/","PLEXUS_COLOCACION_ENC")), header = T,sep=";")
#save(colocacion, file = "colocacion.RData")

#load("colocacion.RData")
#write.xlsx(unlist(names(colocacion)),file=paste0("SALIDA/colocacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

colocacion_FEC_DESEMBOLSO<-sqldf("select FEC_DESEMBOLSO,count(FEC_DESEMBOLSO) from colocacion group by FEC_DESEMBOLSO")
write.xlsx(colocacion_FEC_DESEMBOLSO,file=paste0("SALIDA/colocacion_FEC_DESEMBOLSO_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

colocacion$FEC_DESEMBOLSO2<-paste0(substr(colocacion$FEC_DESEMBOLSO,1, 4 ),substr(colocacion$FEC_DESEMBOLSO,6, 7 ))
write.xlsx(colocacion_FEC_DESEMBOLSO,file=paste0("SALIDA/colocacion_FEC_DESEMBOLSO_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#memory.limit(size=35000)
#Seleccionamos solo las colocaciones entre 201711 y 201802

colocacion2<-sqldf("select * from colocacion where FEC_DESEMBOLSO2 in ('201711', '201712', '201801', '201802')")
colocacion2_fecha<-sqldf("select FEC_DESEMBOLSO2,count(FEC_DESEMBOLSO2) from colocacion2 group by FEC_DESEMBOLSO2")
write.xlsx(colocacion2_fecha,file=paste0("SALIDA/colocacion2_FEC_DESEMBOLSO2_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Analisis COD_PRODUCTO NOM_PRODUCTO PRODUCTO

colocacion_NOM_PRODUCTO<-sqldf("select NOM_PRODUCTO,count(NOM_PRODUCTO) from colocacion2 group by NOM_PRODUCTO")
write.xlsx(colocacion_NOM_PRODUCTO,file=paste0("SALIDA/colocacion_NOM_PRODUCTO_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Seleccionamos producto LIBRANZAS

colocacion3<-sqldf("select * from colocacion2 where NOM_PRODUCTO in ('LIBRANZAS')")

#save(colocacion2, colocacion3, file = "colocacion_def.RData")
colocacion3_fecha<-sqldf("select FEC_DESEMBOLSO2,count(FEC_DESEMBOLSO2) from colocacion3 group by FEC_DESEMBOLSO2")
write.xlsx(colocacion3_fecha,file=paste0("SALIDA/colocacion3_FEC_DESEMBOLSO2_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

colocacion$FEC_DESEMBOLSO2<-paste0(substr(colocacion$FEC_DESEMBOLSO,1, 4 ),substr(colocacion$FEC_DESEMBOLSO,6, 7 ))
#Buscamos estos clientes en la base de datos de prepagos
#load("prepago.RData")

duplicados_prepago<-prepagos[duplicated(prepagos$OBLIGACIONENC), ]
#colocacion4[duplicated(colocacion4$OBLIGACIONENC), ]

duplicados_prepago<-sqldf("select OBLIGACIONENC, count(OBLIGACIONENC) as num_dup from prepagos group by OBLIGACIONENC ")
write.table(as.data.frame(duplicados_prepago),file=paste("SALIDA/duplicados_prepago_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#dado que la base tiene creditos repetidos vamos a consolidarla a nivel de 1 solo credito
prepagos2<-sqldf("select COD_PAGADURIA, COD_SUCURSAL, COD_OFIADMIN, FEC_PAGO, sum(VLR_PAGO) as VLR_PAGO, VLR_SALDOOBLIG, 
      COD_OFIPAGO,    NOM_PAGADURIA , COD_SECTOR,     NOM_SECTOR    , COD_SUBSECTOR,  NOM_SUBSECTOR, 
      COD_OFICINA,    NOM_SECTORANT,  NOM_UNIDAD,     CEDULAENC,      OBLIGACIONENC,  FEC_PAGO2 from prepagos group by OBLIGACIONENC  ")


colocacion4<-merge(x=colocacion3, y=prepagos2, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC", all.x=TRUE, suffixes = c(".coloc",".prep") )

resumen_colocacion_prepago0<-sqldf("select FEC_DESEMBOLSO2 , count(FEC_PAGO2) as malo, count(FEC_DESEMBOLSO2) as bueno from colocacion4 group by FEC_DESEMBOLSO2")
resumen_colocacion_prepago<-sqldf("select FEC_DESEMBOLSO2, FEC_PAGO2 , count(FEC_PAGO2) as malo, count(FEC_DESEMBOLSO2) as bueno from colocacion4 group by FEC_DESEMBOLSO2, FEC_PAGO2 ")

write.table(as.data.frame(resumen_colocacion_prepago0),file=paste("SALIDA/resumen_colocacion_prepago0_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
write.table(as.data.frame(resumen_colocacion_prepago),file=paste("SALIDA/resumen_colocacion_prepago_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Analisis de captación
load("captacion.RData")
captacion_productos<-sqldf("select NOM_PRODUCTO, count(NOM_PRODUCTO) from captacion group by NOM_PRODUCTO")
write.xlsx(captacion_productos,file=paste0("SALIDA/captacion_productos_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
captacion_productos<-sqldf("select NOM_PRODUCTO, count(NOM_PRODUCTO) from captacion group by NOM_PRODUCTO")
sqldf("select CEDULAENC, count(CEDULAENC) from captacion group by CEDULAENC")
captacion$FEC_APECUENTA2<-paste0(substr(captacion$FEC_APECUENTA,1, 4 ),substr(captacion$FEC_APECUENTA,6, 7 ))
FEC_APECUENTA<-sqldf("select FEC_APECUENTA2, count(FEC_APECUENTA2) from captacion group by FEC_APECUENTA2 ")
write.xlsx(FEC_APECUENTA,file=paste0("SALIDA/captacion_FEC_APECUENTA_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Analisis de buró
load("buro.RData")
Buro$FECHA_DATA2<-paste0(substr(Buro$FECHA_DATA,1, 4 ),substr(Buro$FECHA_DATA,6, 7 ))
Buro$clave<-paste0(Buro$CEDULAENC,"_",Buro$FECHA_DATA2)
sqldf("select FECHA_DATA2,count(FECHA_DATA2) from Buro group by FECHA_DATA2")
Buro_201711<-sqldf("select * from Buro where FECHA_DATA2 in ('201711')")
Buro_201711_subset<-Buro_201711[,c("RANGO_APROXIMADO_EDAD","GENERO","ACIERTA_A_FINANCIERO","QUANTO","NUMERO_OBLIGACIONES_ACTIVAS","NUMERO_CREDITOS_CB","VALOR_MORA_CB","NUMERO_CREDITOS_CV","VALOR_MORA_CV","NUMERO_TDC","VALOR_CUPOS","VALOR_UTILIZADO","PORCENTAJE_UTILIZACION","NUMERO_CREDITOS_SR","VALOR_MORA_SR","NUMERO_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","TDC_ALTURA_MAXIMA_DE_MORA","TOTAL_CONSULTAS_ULT_6_MESES","ESTADO_CONSULTA","ENDEUDAMIENTO","UTILIZACION_SIN_POPULAR","EXPERIENCIA_FINANCIERA","FECHA_ENVIO","FECHA_DATA","CEDULAENC")]

colocacion4$clave<-paste0(colocacion4$CEDULAENC,"_",colocacion4$FEC_DESEMBOLSO2)

sqldf("select FEC_DESEMBOLSO2 from colocacion4 group by FEC_DESEMBOLSO2")

colocacion5<-merge(x=colocacion4, y=Buro_201711_subset, by.x="CEDULAENC.coloc", by.y="CEDULAENC", all.x=TRUE, suffixes = c(".coloc",".buro") )
 
write.xlsx(unlist(names(colocacion5)),file=paste0("SALIDA/poblacion_objetivo_nombres_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Super bases
load("superbase.RData")
sqldf("select CUOTA_PAGA, count(CUOTA_PAGA) from super_super group by CUOTA_PAGA")

load("tabla_mes.Rdata")

super_super$fecha2<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-4 )
super_super$anio<-substr(super_super$fecha, nchar(super_super$fecha)-5, nchar(super_super$fecha)-4 )
super_super$mes<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-6 )
super_super<-merge(x=super_super, y=tabla_mes, by.x="mes", by.y="mes1",suffixes = c(""," "))

#colocacion6<-merge(x=colocacion5, y=Buro_201711_subset, by.x="CEDULAENC.coloc", by.y="CEDULAENC", all.x=TRUE, suffixes = c(".coloc",".buro") )

#Analisis paralizado

colocacion$FEC_DESEMBOLSO2<-as.numeric(as.character(colocacion$FEC_DESEMBOLSO2))
BBDD<-sqldf("select  *  from colocacion where FEC_DESEMBOLSO2<201803")
Fecha_desembolso_BBDD<-sqldf("select FEC_DESEMBOLSO2, count(FEC_DESEMBOLSO2) from BBDD group by FEC_DESEMBOLSO2 ")

super_super$FECDES2<-as.numeric(as.character(paste0(substr(super_super$FECDES,1, 4 ),substr(super_super$FECDES,6, 7 ))))
super_super$FECVEN2<-as.numeric(as.character(paste0(substr(super_super$FECVEN,1, 4 ),substr(super_super$FECVEN,6, 7 ))))
super_super$anio<-as.numeric(as.character(paste0("20",super_super$anio)))

super_super$mes2<-paste0("0",super_super$mes2)
super_super$mes2<-substr(super_super$mes2,str_length(super_super$mes2)-1,str_length(super_super$mes2))
super_super$fecha3=as.numeric(as.character(paste0(super_super$anio, super_super$mes2)))

super_super_obj<-sqldf("select * from super_super where fecha3>201710 AND fecha3<201803")
sqldf("select fecha3,count(fecha3) from super_super_obj group by fecha3")

#Cruzamos colocacion con  super_base
BBDD2<-merge(x=BBDD, y=super_super , by.x="OBLIGACIONENC", by.y="OBLIGACIONENC",suffixes = c("",".super"))



