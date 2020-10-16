#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()
sub_ruta="./SUPERBASE_OBJETIVO/";
lista_SUPERBASE<-list.files(sub_ruta,pattern="SUPERBASE_")
ventana_back<-2
load("tabla_mes.Rdata")
super_super<-list()

#save(SUPERBASE_201711, SUPERBASE_201712, SUPERBASE_201801, SUPERBASE_201802, SUPERBASE_201803, SUPERBASE_201804,SUPERBASE_201805, SUPERBASE_201806, SUPERBASE_201807, SUPERBASE_201808, super_super, file="objetivo.Rdata")
load("objetivo.Rdata")

#Lista de fechas
lista_ini<-list("201710","201711", "201712", "201801", "201802", "201803", "201804", "201805", "201806" , "201807")

#Para saber de esas cuantas se han ido K meses despues
lista_fin<-list()
largo<-length(lista_ini)
for(i in (1:largo)){
  if(as.numeric(substr(lista_ini[i],str_length(lista_ini[i])-1, str_length(lista_ini[i]) ))  %in%  c(2:12-ventana_back)){
    lista_fin[i]<- as.numeric(as.character(lista_ini[i])) + ventana_back    
  }else{
    lista_fin[i]<-as.numeric(as.character(lista_ini[i])) + 89 + ventana_back-1
    
  }
}

#
lista_fecha<-as.data.frame(cbind(lista_ini, lista_fin))

#Seleccionar base de datos en el rango de tiempo 
#BBDD<-sqldf("select * from  super_super where fecha between 201711 and 201808")
query=paste0("select * from  super_super where fecha between ",as.character(lista_ini[1]), " and ",as.character(lista_fin[largo]))

BBDD<-sqldf(query,verbose=TRUE) 

freq(BBDD$fecha)
#BBDD$SAL_CAPITA<-as.numeric(BBDD$SAL_CAPITA)
#Estadistica<-sqldf("select fecha, segmentacion, count(fecha) num_creditos, sum(SAL_CAPITA) SAL_CAPITA  from BBDD group by fecha, segmentacion order by fecha, segmentacion ")
#write.xlsx(as.data.frame(Estadistica),file=paste0("SALIDA/segmentacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

#**************************************************
#Filtro 1
#**************************************************

#Implementamos la marca de novacion y fidelizacion
base_novada<-read.csv(gsub(" ", "",paste("Novacion/QUINTA_ENTREGA/","base_R.csv")) , header = T,sep=";")
base_novada<-sqldf("select * from base_novada where C20IDTERCERO<> '' ")

old_names<-names(base_novada)
names(base_novada)<-gsub("V.", "",old_names) 

#freq(base_novada$MODALIDAD)
base_novada$MODALIDAD<-  gsub("Ó", "O",base_novada$MODALIDAD) 

#base_novada$MODALIDAD<-  gsub(" " , "",base_novada$MODALIDAD)
#freq(base_novada$MODALIDAD)

#Incluimos en la BBDD la información de novacion
sum(is.na(base_novada$C69OBLIGACIONNODA)!=TRUE)
base_novada$MOD_DEF<-ifelse(is.na(base_novada$C69OBLIGACIONNODA)!=TRUE & base_novada$MODALIDAD=="FIDELIZACION", "FIDELIZACION", ifelse(is.na(base_novada$C69OBLIGACIONNODA)!=TRUE & base_novada$MODALIDAD!="FIDELIZACION" ,"NOVACION", "DESEMBOLSO" ))
#base_novada$MOD_DEF<-ifelse(base_novada$C69OBLIGACIONNODA!="" & base_novada$MODALIDAD=="FIDELIZACION", "FIDELIZACION", ifelse(base_novada$C69OBLIGACIONNODA!="" & base_novada$MODALIDAD!="FIDELIZACION" ,"NOVACION", "DESEMBOLSO" ))
base_novada$MARCA<-ifelse(base_novada$MOD_DEF=="FIDELIZACION"|  base_novada$MOD_DEF=="NOVACION", 1, 0)
#freq(base_novada$MARCA)
#freq(base_novada$MOD_DEF)

base_novada$FECDES_NF<-as.numeric(as.character(paste0(substr(base_novada$C70FECHADESEM,str_length(base_novada$C70FECHADESEM)-3, str_length(base_novada$C70FECHADESEM)),substr(base_novada$C70FECHADESEM,str_length(base_novada$C70FECHADESEM)-6, str_length(base_novada$C70FECHADESEM)-5) )))
#base_novada<-sqldf("select * from base_novada where MARCA=1 AND FECDES_NF between 201801 and 201808 ")

query=paste0("select * from  base_novada where MARCA=1 AND FECDES_NF between ",as.character(lista_fin[1]), " and ",as.character(lista_fin[largo]))
base_novada<-sqldf(query,verbose=TRUE) 


#Validación de las fechas
freq(base_novada$FECDES_NF)
freq(base_novada$MARCA)

#Se incorpora la información de novación a la base de datos
subset<-base_novada[ ,c("C70IDOBLIGACION" ,  "C69OBLIGACIONNODA", "MOD_DEF"  ,         "FECDES_NF"   ,      "MARCA")]
names(subset)<-c("OBLIGACIONENC2", "OBLIGACIONENC", "MOD_DEF"  ,         "FECDES_NF"   ,      "MARCA")
BBDD<-merge(x=BBDD, y=subset, by.x="OBLIGACIONENC",  by.y="OBLIGACIONENC",all.x=TRUE)
BBDD$MARCA[is.na(BBDD$MARCA)] <- 0
freq(BBDD$MARCA)

#**************************************************
#Filtro 2
#**************************************************

#Importamos archivo prepagos
prepago<-read.csv(gsub(" ", "",paste("PREPAGOS/","PLEXUS_PREPAGOS_ENC")), header = T,sep=";")
#prepago$VLR_PAGO<-as.numeric(prepago$VLR_PAGO)
#prepago$VLR_SALDOOBLIG<-as.numeric(prepago$VLR_SALDOOBLIG)
prepago$FEC_PAGO2<-as.numeric(as.character(paste0(substr(prepago$FEC_PAGO,1, 4 ),substr(prepago$FEC_PAGO,6, 7 ))))
query=paste0("select * from  prepago where FEC_PAGO2 between ",as.character(lista_fin[1]), " and ",as.character(lista_fin[largo]))
prepago2<-sqldf(query,verbose=TRUE) 

#Importamos el segunda archivo de prepago, pero primero lo convertimos en csv
#setwd(paste0(ruta,"/PREPAGOS"))
#xlsx2csv("BasePrepagosv2.xlsx") 

setwd(ruta)
prepago_marca<-read.csv(gsub(" ", "",paste("PREPAGOS/","BasePrepagosv2x.csv")), header = T,sep=",")
names(prepago_marca)<-gsub("\\.", "_",as.character(names(prepago_marca)))
names(prepago_marca)<-gsub("__", "_",as.character(names(prepago_marca)))
names(prepago_marca)<-gsub("Ó", "O",as.character(names(prepago_marca)))
names(prepago_marca)

prepago_marca$FECHA_DEL_PAGO_marca<-as.numeric(as.character(str_sub(gsub("-", "", as.character(as.Date(prepago_marca$FECHA_DEL_PAGO, format = "%m/%d/%Y"))),1,6)))

query=paste0("select * from  prepago_marca where FECHA_DEL_PAGO_marca between ",as.character(lista_fin[1]), " and ",as.character(lista_fin[largo]))

prepago_marca<-sqldf(query,verbose=TRUE) 
freq(prepago_marca$FECHA_DEL_PAGO_marca)

#Limpieza del tipo de prepago
prepago_marca$TIPO_DE_PAGO<-gsub("_", "",prepago_marca$TIPO_DE_PAGO)
prepago_marca$TIPO_DE_PAGO<-gsub("\\.", "_",prepago_marca$TIPO_DE_PAGO)
prepago_marca$TIPO_DE_PAGO<-gsub(" ", "",prepago_marca$TIPO_DE_PAGO)
freq(prepago_marca$TIPO_DE_PAGO)

query=paste0("select * from  prepago_marca where TIPO_DE_PAGO='P_INDIVIDUAL'")
prepago_marca<-sqldf(query,verbose=TRUE) 
freq(prepago_marca$TIPO_DE_PAGO)

subset_prepago<-prepago_marca[,c("NO_OBLIGACION","FECHA_DEL_PAGO_marca","TIPO_DE_PAGO")]
freq(subset_prepago$TIPO_DE_PAGO)
freq(subset_prepago$FECHA_DEL_PAGO_marca)

subset_prepago2<-sqldf("select NO_OBLIGACION, max(FECHA_DEL_PAGO_marca) FEC_PAGO_marca, TIPO_DE_PAGO from subset_prepago group by NO_OBLIGACION")
prepago3<-merge(x=prepago2,y=subset_prepago2, by.x ="OBLIGACIONENC", by.y = "NO_OBLIGACION", all.x=TRUE, suffixes=c(".super", "prepago") )
prepago3<-prepago3[order(prepago3$OBLIGACIONENC),]
prepago3<-prepago3[!duplicated(prepago3$OBLIGACIONENC), ]



subset_prepago2<-sqldf("select NO_OBLIGACION, max(FECHA_DEL_PAGO_marca) FEC_PAGO_marca, TIPO_DE_PAGO from subset_prepago group by NO_OBLIGACION")
BBDD2<-merge(x=BBDD,y=prepago3, by.x ="OBLIGACIONENC", by.y = "OBLIGACIONENC", all.x=TRUE, suffixes=c(".super", "prepago") )
freq(BBDD2$TIPO_DE_PAGO)


#vERIFICAMOS SI TODOS EN PREPAGO3 TIENEN TIPO_DE_PAGO SI NO COMPLETAMOS CON 0
freq(BBDD2$TIPO_DE_PAGO)
BBDD2$TIPO_DE_PAGO[is.na(BBDD2$TIPO_DE_PAGO)]<-0
freq(BBDD2$TIPO_DE_PAGO)

#Arreglemos la cuota pagada
freq(BBDD$CUOTA_PAGA)
BBDD2$CUOTA_PAGA2<-as.numeric(substr(BBDD2$CUOTA_PAGA,1,str_length(BBDD2$CUOTA_PAGA)-2))
#write.xlsx(table(BBDD2$CUOTA_PAGA, useNA="ifany"),file=paste0("SALIDA/prueba_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
#write.xlsx(table(BBDD2$CUOTA_PAGA2, useNA="ifany"),file=paste0("SALIDA/prueba2_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
BBDD2$CUOTA_PENDIENTE<-as.numeric(BBDD2$PLAZO)-as.numeric(BBDD2$CUOTA_PAGA2)
freq(BBDD2$CUOTA_PENDIENTE)
BBDD2$CUOTA_P_MARCA<-ifelse(BBDD2$CUOTA_PENDIENTE>6,0,1) #Hay que eliminar los 1
freq(BBDD2$CUOTA_P_MARCA)
BBDD2$CUOTA_P_MARCA[is.na(BBDD2$CUOTA_P_MARCA)] <- 0
freq(BBDD2$CUOTA_P_MARCA)
freq(BBDD2$TIPO_DE_PAGO)

#Hamos marca_prepago
BBDD2$marca_prepago<-ifelse(BBDD2$TIPO_DE_PAGO=='P_INDIVIDUAL' & BBDD2$CUOTA_P_MARCA==1, 1,0)
freq(BBDD2$marca_prepago)
table(BBDD2$TIPO_DE_PAGO, BBDD2$CUOTA_P_MARCA, useNA="ifany")

#Para poder aplicar las reglas de rescate se debe tener información de la fecha fin
BBDD3<-merge(x=BBDD2, y=lista_fecha, by.x="fecha",  by.y="lista_ini",suffixes = c("",".lista"),all.x=TRUE)

BBDD3$lista_fin<-as.numeric(BBDD3$lista_fin)

#freq(BBDD3$lista_fin)
#table(BBDD3$fecha, BBDD3$lista_fin, useNA="ifany")
#BBDD3$MARCA_PREPAGO_RANGO<- ifelse(BBDD3$fecha==as.numeric(lista_fin[largo]) & is.na(BBDD3$FEC_PAGO)!=TRUE , 1, ifelse(is.na(BBDD3$FEC_PAGO)!=TRUE & BBDD3$FEC_PAGO<=BBDD3$lista_fin, 1,0))
BBDD3$MARCA_PREPAGO_RANGO<- ifelse(BBDD3$fecha==as.numeric(lista_fin[largo]) & is.na(BBDD3$FEC_PAGO2)!=TRUE , 1, ifelse(is.na(BBDD3$FEC_PAGO2)!=TRUE & BBDD3$FEC_PAGO2<=BBDD3$lista_fin & as.numeric(BBDD3$FEC_PAGO2)>as.numeric(BBDD3$fecha), 1,ifelse(BBDD3$CUOTA_P_MARCA==1,1, 0)))
table(BBDD3$MARCA_PREPAGO_RANGO, useNA="ifany")

#**************************************************
#Filtro 3
#**************************************************

#Eliminamos fecha de vencimiento
#BBDD3$MARCA_VENCIMIENTO<-ifelse(BBDD3$fecha==BBDD3$FECVEN2 , 1 ,0)
BBDD3$MARCA_VENCIMIENTO<-ifelse(BBDD3$fecha>=BBDD3$FECVEN2 , 1 ,0)
table(BBDD3$MARCA_VENCIMIENTO, useNA="ifany")

#MARCA_VENCIMIENTO<-ifelse(BBDD3$fecha>=BBDD3$FECVEN2  , 1 ,0)
#freq(MARCA_VENCIMIENTO)
#freq(BBDD3$MARCA_VENCIMIENTO)
BBDD3$MARCA_PREPAGO_RANGO2<- ifelse(BBDD3$MARCA_PREPAGO_RANGO==1 & BBDD3$MARCA_VENCIMIENTO==1, 0, BBDD3$MARCA_PREPAGO_RANGO)
table(BBDD3$MARCA_PREPAGO_RANGO2, useNA="ifany")

#table(BBDD3$MARCA_PREPAGO_RANGO2, useNA="ifany")

#**************************************************
#Filtro 4
#**************************************************

#Importamos el segunda archivo de prepago, pero primero lo convertimos en csv
#setwd(paste0(ruta,"/Novacion/QUINTA_ENTREGA"))
#xlsx2csv("Base_siniestrosLib.xlsx") 
setwd(paste0(ruta,"/Novacion/QUINTA_ENTREGA"))
base_siniestro<-read.csv(gsub(" ", "",paste("Base_siniestrosLibx.csv")), header = T,sep=",")
setwd(ruta)

names(base_siniestro)<-gsub("\\.", "_",as.character(names(base_siniestro)))
names(base_siniestro)<-gsub("__", "_",as.character(names(base_siniestro)))
names(base_siniestro)<-gsub("Ó", "O",as.character(names(base_siniestro)))
names(base_siniestro)

base_siniestro$FEC_PAGO_SINIESTRO<-as.numeric(as.character(str_sub(gsub("-", "", as.character(as.Date(base_siniestro$FECHA_DE_PAGO, format = "%m/%d/%Y"))),1,6)))

freq(base_siniestro$FEC_PAGO_SINIESTRO)

query=paste0("select * from  base_siniestro where FEC_PAGO_SINIESTRO between ",as.character(lista_ini[1]), " and ",as.character(lista_fin[largo]))
base_siniestro<-sqldf(query,verbose=TRUE) 
freq(base_siniestro$FEC_PAGO_SINIESTRO)
subset_base_siniestro<-base_siniestro[,c("DOCUMENTO","FEC_PAGO_SINIESTRO" )]
subset_base_siniestro2<-sqldf("select DOCUMENTO, min(FEC_PAGO_SINIESTRO) FEC_PAGO_SINIESTRO from subset_base_siniestro group by DOCUMENTO")
#Eliminamos Marca siniestro

#sum(is.na(BBDD3$CEDULAENC.super))
BBDD4<-merge(x=BBDD3, y=subset_base_siniestro2, by.x="CEDULAENC.super",  by.y="DOCUMENTO",all.x=TRUE)

#BBDD4$MARCA_SINIESTRO_RANGO<- ifelse(BBDD4$fecha==as.numeric(lista_fin[largo]) & is.na(BBDD4$FEC_PAGO_SINIESTRO)!=TRUE , 1, ifelse(is.na(BBDD4$FEC_PAGO_SINIESTRO)!=TRUE & BBDD4$FEC_PAGO_SINIESTRO<=BBDD4$lista_fin, 1,0))
#freq(BBDD4$MARCA_SINIESTRO_RANGO)

BBDD4$MARCA_SINIESTRO_RANGO<- ifelse(BBDD4$fecha>=as.numeric(lista_fin[largo]) & is.na(BBDD4$FEC_PAGO_SINIESTRO)!=TRUE , 1, ifelse(is.na(BBDD4$FEC_PAGO_SINIESTRO)!=TRUE & BBDD4$FEC_PAGO_SINIESTRO<=BBDD4$lista_fin & BBDD4$FEC_PAGO_SINIESTRO>BBDD4$fecha, 1,0))
#freq(BBDD4$MARCA_SINIESTRO_RANGO)

table(BBDD4$MARCA_SINIESTRO_RANGO,  useNA = "ifany")
table(BBDD4$fecha,BBDD4$MARCA_SINIESTRO_RANGO,  useNA = "ifany")

write.xlsx(freq(base_siniestro$FEC_PAGO_SINIESTRO),file=paste0("SALIDA/siniestro_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)


#**************************************************
#LIMPIEZA FINAL
#**************************************************

#Limpiamos MARCA_SINIESTRO_RANGO

BBDD4$MARCA_PREPAGO_RANGO3<- ifelse(BBDD4$MARCA_PREPAGO_RANGO2==1 & BBDD4$MARCA_SINIESTRO_RANGO==1,0, BBDD4$MARCA_PREPAGO_RANGO2)
table(BBDD4$MARCA_PREPAGO_RANGO, useNA ="ifany")
table(BBDD4$MARCA_PREPAGO_RANGO2, useNA ="ifany")
table(BBDD4$MARCA_PREPAGO_RANGO3, useNA ="ifany")

prop.table(t(table(BBDD4$fecha, BBDD4$MARCA_PREPAGO_RANGO, useNA ="ifany")))*100

#Validemos que no estoy contando los vencimientos (1,1)
table(BBDD4$MARCA_PREPAGO_RANGO3, BBDD4$MARCA_VENCIMIENTO, useNA ="ifany")

#Validemos que no estoy contando los siniestros (1,1)
table(BBDD4$MARCA_PREPAGO_RANGO3, BBDD4$MARCA_SINIESTRO_RANGO, useNA ="ifany")

#Ahora se dejamos solo los prepagos con tipo de pago  individual con mas de 6 

BBDD4$MARCA_PREPAGO_RANGO4<-ifelse(BBDD4$MARCA_PREPAGO_RANGO3==1 & BBDD4$TIPO_DE_PAGO=='P_INDIVIDUAL' & BBDD4$CUOTA_P_MARCA==0,1,0)

table(BBDD4$MARCA_PREPAGO_RANGO4, useNA ="ifany")

distr_fecha<-sqldf("select fecha, segmentacion, MARCA_PREPAGO_RANGO4, count() from BBDD4 group by fecha, segmentacion, MARCA_PREPAGO_RANGO4 order by fecha, segmentacion, MARCA_PREPAGO_RANGO4")

write.xlsx(as.data.frame(distr_fecha),file=paste0("SALIDA/churn_def_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

valida_fecha<-sqldf("select fecha, count() from BBDD4 group by fecha")

write.xlsx(as.data.frame(valida_fecha),file=paste0("SALIDA/valida_fecha_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

sqldf("select fecha, count() población from BBDD4 group by fecha")

#Validacion<-sqldf("select * from BBDD4 where MARCA_PREPAGO_RANGO3==1")
#write.csv(Validacion, file=paste0("SALIDA/Validacion_",ventana_back,"_" ,format(today, format="%Y%m%d"),".xlsx"))

#Creacion de la base de datos a utilizar
query=paste0("select * from  BBDD4 where fecha between ",as.character(lista_ini[2]), " and ",as.character(lista_ini[largo-1]))
OBJETIVO<-sqldf(query,verbose=TRUE) 
OBJETIVO$TARGET<-OBJETIVO$MARCA_PREPAGO_RANGO4
freq(OBJETIVO$fecha)
names(OBJETIVO)
freq(OBJETIVO$CUOTA_PAGA2)

#FILTRO: SE DEBE FILTRAR DESEMBOLSOS MAYORES A SEIS MESES.
OBJETIVO<-sqldf("select * from OBJETIVO where CUOTA_PAGA2>6 ")
freq(OBJETIVO$CUOTA_PAGA2)
save(OBJETIVO, file="POBLACION_OBJETIVO.Rdata")

