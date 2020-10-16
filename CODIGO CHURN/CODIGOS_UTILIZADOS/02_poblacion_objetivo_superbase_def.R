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
ventana_back<-1
load("tabla_mes.Rdata")
super_super<-list()

for(file in lista_SUPERBASE){
  rfile=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("SUPERBASE/",file)), n = 1)
  if (grepl(";", L)){
    rfile<-read.csv(gsub(" ", "",paste("SUPERBASE/",file)), header = T,sep=";")
  } else{
    rfile<-read.csv(gsub(" ", "",paste("SUPERBASE/",file)), header = T,sep=",")
  }
  fecha<-paste(file)
  fecha_aux<-substr(file, nchar(file)-8, nchar(file)-4 )
  anio<-as.numeric(as.character(paste0("20",substr(fecha, nchar(fecha)-5, nchar(fecha)-4 ))))
  mes<-substr(fecha, nchar(fecha)-8, nchar(fecha)-6 )
  rfile$fecha<-paste(file)
  if (mes %in% ("ENE")){
    mes2="01"
  }else if (mes %in% ("FEB")){
    mes2="02"
  }else if (mes %in% ("MAR")){
    mes2="03"
  }else if (mes %in% ("ABR")){
    mes2="04"
  }else if (mes %in% ("MAY")){
    mes2="05"
  }else if (mes %in% ("JUN")){
    mes2="06"
  }else if (mes %in% ("JUL")){
    mes2="07"
  }else if (mes %in% ("AGO")){
    mes2="08"
  }else if (mes %in% ("SEP")){
    mes2="09"
  }else if (mes %in% ("OCT")){
    mes2="10"
  }else if (mes %in% ("NOV")){
    mes2="11"
  }else if (mes %in% ("DIC")){
    mes2="12"
  }
  
  rfile$FECDES2<-as.numeric(as.character(paste0(substr(rfile$FECDES,1, 4 ),substr(rfile$FECDES,6, 7 ))))
  rfile$FECVEN2<-as.numeric(as.character(paste0(substr(rfile$FECVEN,1, 4 ),substr(rfile$FECVEN,6, 7 ))))
 
  rfile$fecha<-as.numeric(as.character(paste0(anio, mes2)))
  
  #rfile<-rfile[ , !(names(rfile) %in% c("fecha2","anio", "mes") )]
  
  f_seg<-paste0(anio, mes2)
  
  rfile$segmentacion<-ifelse(rfile$FECDES2==f_seg , "desembolso" , "coloc_actual")

    
  super_super<-rbind(super_super,rfile)
  
  assign(paste0(substr(file,1, str_length(file)-10 ),"_",paste0(anio, mes2)), rfile)  
  print(file)
}

#save(SUPERBASE_201711, SUPERBASE_201712, SUPERBASE_201801, SUPERBASE_201802, SUPERBASE_201803, SUPERBASE_201804,SUPERBASE_201805, SUPERBASE_201806, SUPERBASE_201807, SUPERBASE_201808, super_super, file="objetivo.Rdata")
load("objetivo.Rdata")

#Lista de fechas
lista_ini<-list("201711", "201712", "201801", "201802", "201803", "201804", "201805", "201806" , "201807")

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
BBDD$SAL_CAPITA<-as.numeric(BBDD$SAL_CAPITA)

#Estadistica<-sqldf("select fecha, segmentacion, count(fecha) num_creditos, sum(SAL_CAPITA) SAL_CAPITA  from BBDD group by fecha, segmentacion order by fecha, segmentacion ")
#write.xlsx(as.data.frame(Estadistica),file=paste0("SALIDA/segmentacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

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

#Contrucción del target
lista_target<-list()
for(i in (1:largo)){
  query=paste0("select OBLIGACIONENC, fecha from SUPERBASE_",as.character(lista_ini[i]), " where OBLIGACIONENC not in (select OBLIGACIONENC from  SUPERBASE_",as.character(lista_fin[i]),")")
  faux<-sqldf(query,verbose=TRUE) 
  lista_target<-rbind(lista_target,faux)
}

#Cruzamos con la base_def
target<-as.data.frame(lista_target)
#write.table(as.data.frame(target),file=paste("SALIDA/target_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
target<-sqldf("select OBLIGACIONENC, fecha  FROM target order by OBLIGACIONENC, fecha")

target$OBLIGACIONENC_cruce<-paste0(target$OBLIGACIONENC,"_",target$fecha)
target$TARGET_BINARY_TARG<-1

BBDD$OBLIGACIONENC_cruce<-paste0(BBDD$OBLIGACIONENC,"_",BBDD$fecha)

base_def<-merge(x=BBDD, y=target, by.x="OBLIGACIONENC_cruce",  by.y="OBLIGACIONENC_cruce",suffixes = c("",".TARGET"),all.x=TRUE)

#sqldf("select TARGET_BINARY_TARG, count() from base_def group by TARGET_BINARY_TARG")
#sum(!is.na(base_def$TARGET_BINARY_TARG))
#save(base_def, file="base_target.Rdata")
base_def$TARGET_BINARY_TARG[is.na(base_def$TARGET_BINARY_TARG)] <- 0
#freq(base_def$TARGET_BINARY_TARG)

#Para poder aplicar las reglas de rescate se debe tener información de la fecha fin
base_def<-merge(x=base_def, y=lista_fecha, by.x="fecha",  by.y="lista_ini",suffixes = c("",".lista"),all.x=TRUE)
#freq(as.numeric(base_def$lista_fin))

base_def$lista_fin<-as.numeric(base_def$lista_fin)
base_def$lista_fin[is.na(base_def$lista_fin)] <- 999999

#Marca2 para identificar las novaciones y fidelizaciones que se realizaron durante la ventana bajo estudio
base_def$MARCA2<-ifelse(is.na(base_def$MOD_DEF)!=TRUE & base_def$FECDES_NF<=base_def$lista_fin, 1, 0)

freq(base_def$MARCA2)

freq(base_def$MARCA)
#Reglas de rescate
base_def$TARGET_BINARY<-ifelse(base_def$TARGET_BINARY_TARG=="1" & base_def$MARCA==1 & base_def$MARCA2==1, 0, base_def$TARGET_BINARY_TARG)
sqldf("select TARGET_BINARY_TARG, TARGET_BINARY , marca, count() from base_def group by  TARGET_BINARY_TARG, TARGET_BINARY , marca order by TARGET_BINARY_TARG, TARGET_BINARY , marca")
#freq(base_def$TARGET_BINARY)
#freq(base_def$TARGET_BINARY_TARG)
#Porcentaje de los que se fueron
data.frame(table(base_def$TARGET_BINARY)) %>%
  mutate(Rel_Freq = Freq*100/sum(Freq)) 

#Marca para identificar churn voluntario

#base_siniestro<-read.csv(gsub(" ", "",paste("Novacion/CUARTA_ENTREGA/","base_siniestro.csv")) , header = T,sep=";")
#base_siniestro$FEC_PAGO_SINIESTRO<-as.numeric(paste0(substr(base_siniestro$FECHA_DE_PAGO,str_length(base_siniestro$FECHA_DE_PAGO)-3, str_length(base_siniestro$FECHA_DE_PAGO) ), substr(base_siniestro$FECHA_DE_PAGO,str_length(base_siniestro$FECHA_DE_PAGO)-6, str_length(base_siniestro$FECHA_DE_PAGO)-5 )))
#freq(base_siniestro$FEC_PAGO_SINIESTRO)
#query=paste0("select * from  base_siniestro where FEC_PAGO_SINIESTRO between ",as.character(lista_ini[1]), " and ",as.character(lista_fin[largo]))
#base_siniestro<-sqldf(query,verbose=TRUE) 
#freq(base_siniestro$FEC_PAGO_SINIESTRO)
#base_def<-merge(x=base_def, y=base_siniestro, by.x="OBLIGACIONENC",  by.y="OBLIGACION",all.x=TRUE)
#freq(base_def$FEC_PAGO_SINIESTRO)

base_def$marca_churn_vencimiento<-ifelse(base_def$fecha==base_def$FECVEN2, 1 ,0)
freq(base_def$marca_churn_vencimiento)
sqldf("select fecha, marca_churn_vencimiento, count() from base_def group by fecha, marca_churn_vencimiento")


#Fecha vencimiento distinta al mes de curso pero dentro del periodo de entrenamiento
#query=paste0("select * from base_def where FECVEN2>=",as.numeric(as.character(lista_fin[largo] )))
#base_def2<-sqldf(query,verbose=TRUE) 
base_def$marca_fecha_venc_dist<-ifelse(base_def$FECVEN2>=lista_fin[1] & base_def$FECVEN2<=lista_fin[largo] , 1 ,0)
freq(base_def$marca_fecha_venc_dist)

#Distribución por año por fecha
distr_fecha<-base_def %>% group_by(fecha,TARGET_BINARY) %>% tally()
write.xlsx(as.data.frame(distr_fecha),file=paste0("SALIDA/churn_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)


distr_fecha<-sqldf("select fecha, segmentacion, TARGET_BINARY,marca_churn_temporal, marca_fecha_dist, count() from base_def group by fecha, segmentacion, TARGET_BINARY,marca_churn_temporal, marca_fecha_dist order by fecha, segmentacion,TARGET_BINARY , marca_churn_temporal , marca_fecha_dist ")

write.xlsx(as.data.frame(distr_fecha),file=paste0("SALIDA/churn_def_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

#Marca para identificar los desembolsos inferiores a 6 meses

base_def$FECDES<-  gsub("-", "/",base_def$FECDES) 
base_def$prueba<-as.Date(factor(base_def$FECDES))

as.Date(factor(paste0(str_sub(base_def$fecha,1,4),"/",str_sub(base_def$fecha,5,6),"/","01")))

date.end.month <- (seq(as.Date(factor(paste0(str_sub(base_def$fecha,1,4),"/",str_sub(base_def$fecha,5,6),"/","01"))),length=2,by="months")-1)[2]


str_sub(base_def$fecha,1,4)




freq(base_def$prueba)
prueba<-sqldf("select fecha, FECDES2, prueba from base_def group by fecha, FECDES2, prueba order by fecha, FECDES2, prueba ")
write.xlsx(as.data.frame(prueba),file=paste0("SALIDA/prueba_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
base_def$marca_d6m<-ifelse(base_def$FECVEN2<=lista_fin[largo] , 1 ,0)
freq(base_def$marca_fvl)

# 
install.packages("lubridate")
library("lubridate")
base_def$prueba2<-ymd(base_def$fecha)

names(base_def)
#*******************************************
#Basura que puede ser util

sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201710")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201711")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201712")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201801")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201802")

#save("super_super", "SUPERBASE_201710","SUPERBASE_201711" ,"SUPERBASE_201712", "SUPERBASE_201801" ,"SUPERBASE_201802","SUPERBASE_201803","SUPERBASE_201804", "SUPERBASE_201805","SUPERBASE_201806", "SUPERBASE_201807", "SUPERBASE_201808" ,  file = "superbase_o.RData")
#load("SUPERBASE_O.Rdata")
#Lista donde estarán las fechas a analizar 



names(super_super)
freq(super_super$FECVEN2)
freq(super_super$CUOTA_PAGA)
sqldf("select distinct(FECDES2), count() from SUPERBASE_201711 where CUOTA_PAGA in ('0')" )

sqldf("select distinct(FECDES2), count() from SUPERBASE_201711 where CUOTA_PAGA=0")
sqldf("select distinct(FECDES2), count() from SUPERBASE_201711 where CUOTA_PAGA=0")

#Base de datos de créditos 

target<-as.data.frame(lista_target)
#write.table(as.data.frame(target),file=paste("SALIDA/target_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
target<-sqldf("select OBLIGACIONENC, fecha  FROM target order by OBLIGACIONENC, fecha")
target<-target  %>% 
  group_by(OBLIGACIONENC) %>% 
  filter(row_number()==1)

target$OBLIGACIONENC_cruce<-paste0(target$OBLIGACIONENC,"_",target$fecha)
target$TARGET_BINARY_TARG<-1





base_def<-get(paste0("SUPERBASE_",lista_ini[1]))

#freq(base_def$fecha_marca)
#freq(base_def$TIPO_DE_PRODUCTO)
#names(base_def)

segmentacion<-sqldf("select fecha, segmentacion, count() from super_super group by fecha, segmentacion order by fecha, segmentacion")
write.xlsx(as.data.frame(segmentacion),file=paste0("SALIDA/segmentacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)


#Para saber de esas cuantas se han ido Ventana meses despues

lista_fin<-list()
largo<-length(lista_ini)
for(i in (1:largo)){
  if(substr(lista_ini[i],str_length(lista_ini[i])-1, str_length(lista_ini[i]) ) %in%  c('07','08','09','10','11','12')){
    lista_fin[i]<-as.numeric(as.character(lista_ini[i])) + 88 + ventana_back
  }else{
    lista_fin[i]<- as.numeric(as.character(lista_ini[i])) + ventana_back
  }
}

#Elaboración de filtros
#Fecha vencimiento lejana
fecha_lejana<-lista_fin[largo]

filtro1<-sqldf("select * from super_super ")

query=paste0("select *  from super_super",as.character(lista_fin[largo]), " where OBLIGACIONENC not in (select OBLIGACIONENC from  SUPERBASE_",as.character(lista_fin[i]),")")
faux<-sqldf(query,verbose=TRUE) 



#Construcción de la base de datos 
file2<-get(paste0("SUPERBASE_",lista_ini[2]))
file1<-get(paste0("SUPERBASE_",lista_ini[1]))
base_aux<-sqldf("select * from file2 where OBLIGACIONENC not in (select OBLIGACIONENC from  file1) and fecha=FECDES2")
base_def<-rbind(base_aux, base_def)

file2<-get(paste0("SUPERBASE_",lista_ini[3]))
file1<-get(paste0("SUPERBASE_",lista_ini[2]))
base_aux<-sqldf("select * from file2 where OBLIGACIONENC not in (select OBLIGACIONENC from  file1) and fecha=FECDES2")
base_def<-rbind(base_aux, base_def)

file2<-get(paste0("SUPERBASE_",lista_ini[4]))
file1<-get(paste0("SUPERBASE_",lista_ini[3]))
base_aux<-sqldf("select * from file2 where OBLIGACIONENC not in (select OBLIGACIONENC from  file1) and fecha=FECDES2")
base_def<-rbind(base_aux, base_def)

#Distribución de libranzas por año
sqldf("select fecha, count() from base_def group by fecha order by fecha")

lista_target<-list()

for(i in (1:largo)){
  query=paste0("select OBLIGACIONENC, fecha from SUPERBASE_",as.character(lista_ini[i]), " where OBLIGACIONENC not in (select OBLIGACIONENC from  SUPERBASE_",as.character(lista_fin[i]),")")
  faux<-sqldf(query,verbose=TRUE) 
  lista_target<-rbind(lista_target,faux)
}

target<-as.data.frame(lista_target)
#write.table(as.data.frame(target),file=paste("SALIDA/target_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
target<-sqldf("select OBLIGACIONENC, fecha  FROM target order by OBLIGACIONENC, fecha")
target<-target  %>% 
  group_by(OBLIGACIONENC) %>% 
  filter(row_number()==1)

target$OBLIGACIONENC_cruce<-paste0(target$OBLIGACIONENC,"_",target$fecha)
target$TARGET_BINARY_TARG<-1

#Cruzamos con la base_def
base_def$OBLIGACIONENC_cruce<-paste0(base_def$OBLIGACIONENC,"_",base_def$fecha)
base_def<-merge(x=base_def, y=target, by.x="OBLIGACIONENC_cruce",  by.y="OBLIGACIONENC_cruce",suffixes = c("",".TARGET"),all.x=TRUE)

sqldf("select TARGET_BINARY_TARG, count() from base_def group by TARGET_BINARY_TARG")

sum(!is.na(base_def$TARGET_BINARY_TARG))

#save(base_def, file="base_target.Rdata")
base_def$TARGET_BINARY_TARG[is.na(base_def$TARGET_BINARY_TARG)] <- 0
#save(base_def, file="base_def.Rdat")

#Vamos a identificar la población de 201711 que es nueva
file2<-get(paste0("SUPERBASE_",lista_ini[1]))
file1<-get(paste0("SUPERBASE_",lista_ini0))
base_aux<-sqldf("select OBLIGACIONENC from file2 where OBLIGACIONENC not in (select OBLIGACIONENC from  file1)")
base_aux$fecha_marca<-1
base_def<-merge(x=base_def, y=base_aux, by.x="OBLIGACIONENC", by.y="OBLIGACIONENC",suffixes = c("",""), all.x=T)
base_def$fecha_marca[is.na(base_def$fecha_marca)] <- 0
#freq(base_def$fecha_marca)

base_def$fecha<-as.numeric(as.character(base_def$fecha))
base_def$fecha_temporal<-ifelse(base_def$fecha==201711 & base_def$fecha_marca==1, "201711_1" ,base_def$fecha)
freq(base_def$fecha_temporal)

#Porcentaje de los que se fueron
data.frame(table(base_def$TARGET_BINARY_TARG)) %>%
  mutate(Rel_Freq = Freq*100/sum(Freq)) 

#Distribución por año opr fecha
distr_fecha<-base_def %>% group_by(fecha,TARGET_BINARY_TARG) %>% tally()

#Distribución por año por fecha discrimando 201711
distr_fecha2<-base_def %>% group_by(fecha_temporal,TARGET_BINARY_TARG) %>% tally()

#write.xlsx(as.data.frame(rbind(distr_fecha,distr_fecha2)),file=paste0("SALIDA/resumen_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)
#Analisis de perimetro


#Limpieza 
base_def$SAL_CAPITA<-as.numeric(as.character(base_def$SAL_CAPITA))
base_def$VLR_CUOTA<-as.numeric(as.character(base_def$VLR_CUOTA))
base_def$VLR_MORA<-as.numeric(as.character(base_def$VLR_MORA))
base_def$PLAZO<-as.numeric(as.character(base_def$PLAZO))

perimetro_nov20117_mora<-sqldf("select  fecha_temporal,TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def where fecha_temporal=201711 and VLR_MORA>0  group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")
perimetro_nov20117_mora_sin_target<-sqldf("select  fecha_temporal, count(fecha_temporal) total, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def where fecha_temporal=201711 and VLR_MORA>0 ")

perimetro_nov20117_nomora<-sqldf("select  fecha_temporal,TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def where fecha_temporal=201711 and VLR_MORA=0  group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")
perimetro_nov20117_nomora_sin_target<-sqldf("select  fecha_temporal, count(fecha_temporal) total, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def where fecha_temporal=201711 and VLR_MORA=0 ")

perimetro_nov20117<-sqldf("select  fecha_temporal,TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def where fecha_temporal=201711 group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")

perimetro<-sqldf("select fecha_temporal, TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def group by fecha_temporal, TARGET_BINARY_TARG order by fecha_temporal, TARGET_BINARY_TARG ")
perimetro_total<-sqldf("select  TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")
perimetro_total_sin_target<-sqldf("select  sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA, sum(VLR_MORA) VLR_MORA, avg(PLAZO) PROM_PLAZO from base_def")


#nueva_colocacion_rara<-sqldf("select * from base_def where fecha_temporal in ('201712') and VLR_MORA>0")

write.xlsx(as.data.frame(perimetro),file=paste0("SALIDA/analisis_perimetro",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
write.xlsx(as.data.frame(perimetro_total),file=paste0("SALIDA/analisis_perimetro_total",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

write.xlsx(as.data.frame(perimetro_nov20117_nomora),file=paste0("SALIDA/analisis_perimetro_nov2017_nomora",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
write.xlsx(as.data.frame(perimetro_nov20117_mora),file=paste0("SALIDA/analisis_perimetro_nov2017_mora",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
write.xlsx(as.data.frame(perimetro_nov20117),file=paste0("SALIDA/analisis_perimetro_nov2017",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)



#sqldf("select avg(PLAZO) from base_def")
#sqldf("select fecha, avg(PLAZO) from base_def group by fecha")


#Población objetivo, se eliminaran aquellos créditos con valor en mora>0

base_def2<-sqldf("select * from base_def where VLR_MORA=0")

#MISHEL

MISHEL<-sqldf("select distinct(FECDES2) from SUPERBASE_201711")

