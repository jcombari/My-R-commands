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
  super_super<-rbind(super_super,rfile)
  
  assign(paste0(substr(file,1, str_length(file)-10 ),"_",paste0(anio, mes2)), rfile)  
  print(file)
}

#Estadística básica para fecha

sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201710")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201711")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201712")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201801")
sqldf("select FECVEN, count(FECVEN) from SUPERBASE_201802")

#save("super_super", "SUPERBASE_201710","SUPERBASE_201711" ,"SUPERBASE_201712", "SUPERBASE_201801" ,"SUPERBASE_201802","SUPERBASE_201803","SUPERBASE_201804", "SUPERBASE_201805","SUPERBASE_201806", "SUPERBASE_201807", "SUPERBASE_201808" ,  file = "superbase_o.RData")
#load("SUPERBASE_O.Rdata")
names(super_super)

lista_ini<-list("201711", "201712", "201801", "201802")
#Base de datos de créditos 

#Ojo pendiente resolver 201711-1 (201801-1???)
lista_ini0<-as.numeric(lista_ini[1])-1

base_def<-get(paste0("SUPERBASE_",lista_ini[1]))

#freq(base_def$fecha_marca)
#freq(base_def$TIPO_DE_PRODUCTO)
#names(base_def)

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

#Para saber de esas cuantas se han ido 6 meses despues

lista_fin<-list()
largo<-length(lista_ini)
for(i in (1:largo)){
  if(substr(lista_ini[i],str_length(lista_ini[i])-1, str_length(lista_ini[i]) ) %in%  c('07','08','09','10','11','12')){
    lista_fin[i]<-as.numeric(as.character(lista_ini[i])) + 88 + ventana_back
  }else{
    lista_fin[i]<- as.numeric(as.character(lista_ini[i])) + ventana_back
  }
}

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

