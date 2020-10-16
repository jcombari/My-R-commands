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
sub_ruta="./contratos_OBJETIVO/";
lista_contratos<-list.files(sub_ruta,pattern="contratos_")
ventana_back<-6
load("tabla_mes.Rdata")
super_contratos<-list()

for(file in lista_contratos){
  rfile=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("contratos/",file)), n = 1)
  if (grepl(";", L)){
    rfile<-read.csv(gsub(" ", "",paste("contratos/",file)), header = T,sep=";")
  } else{
    rfile<-read.csv(gsub(" ", "",paste("contratos/",file)), header = T,sep=",")
  }
  names(rfile)<-c("ID_CONTRATO","ID_CLIENTE","BRANCH_DEL_CONTRATO","BRANCH_DEL_CLIENTE","FECHA_DE_INICIO","FECHA_DE_FINALIZACION","STATUS_CONTRATO","FECHA_CANCELACION","RAZON_CANCELACION","CANCELACION_TEMPRANA","FECHA_ULTIMA_RENOVACION","CONTRATO_EN_MORA","CONTRATO_EN_DEFAULT","INDICADOR_SISTEMA_ALERTA_TEMPRANA","INDICADOR_DE_BLOQUEO","DIVISA","RAZON_DE_CREDITO","IDIOMA_CONTRATO","TIPO_DE_PRODUCTO","REFINANCIADO","DEUDA_TOTAL_INICIAL","SAL_CAPITA","VLR_CUOTA")
  rfile$FECHA_DE_INICIO2<-as.numeric(as.character(paste0(substr(rfile$FECHA_DE_INICIO,1, 4 ),substr(rfile$FECHA_DE_INICIO,6, 7 ))))
  rfile$FECHA_DE_FINALIZACION2<-as.numeric(as.character(paste0(substr(rfile$FECHA_DE_FINALIZACION,1, 4 ),substr(rfile$FECHA_DE_FINALIZACION,6, 7 ))))
  rfile$fecha<-substr(file,str_length(file)-9, str_length(file)-4 )
  rfile$largo_contrato<-str_length(rfile$ID_CONTRATO)
  rfile$ID_CONTRATO<-as.numeric(rfile$ID_CONTRATO)
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile)  
  super_contratos<-rbind(super_contratos,rfile)
  print(file)
}

lista_ini<-list("201711", "201712", "201801", "201802")
#Base de datos de créditos 

#Ojo pendiente resolver 201711-1 (201801-1???)
lista_ini0<-as.numeric(lista_ini[1])-1

base_def<-get(paste0("contratos_",lista_ini[1]))

#freq(base_def$fecha_marca)
#freq(base_def$TIPO_DE_PRODUCTO)
#names(base_def)

#Construcción de la base de datos 
file2<-get(paste0("contratos_",lista_ini[2]))
file1<-get(paste0("contratos_",lista_ini[1]))
base_aux<-sqldf("select * from file2 where ID_CONTRATO not in (select ID_CONTRATO from  file1) and fecha=FECHA_DE_INICIO2")
base_def<-rbind(base_aux, base_def)

file2<-get(paste0("contratos_",lista_ini[3]))
file1<-get(paste0("contratos_",lista_ini[2]))
base_aux<-sqldf("select * from file2 where ID_CONTRATO not in (select ID_CONTRATO from  file1) and fecha=FECHA_DE_INICIO2")
base_def<-rbind(base_aux, base_def)

file2<-get(paste0("contratos_",lista_ini[4]))
file1<-get(paste0("contratos_",lista_ini[3]))
base_aux<-sqldf("select * from file2 where ID_CONTRATO not in (select ID_CONTRATO from  file1) and fecha=FECHA_DE_INICIO2")
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
  query=paste0("select ID_CONTRATO, fecha from contratos_",as.character(lista_ini[i]), " where ID_CONTRATO not in (select ID_CONTRATO from  contratos_",as.character(lista_fin[i]),")")
  faux<-sqldf(query,verbose=TRUE) 
  lista_target<-rbind(lista_target,faux)
}

target<-as.data.frame(lista_target)
#write.table(as.data.frame(target),file=paste("SALIDA/target_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
target<-sqldf("select ID_CONTRATO, fecha  FROM target order by ID_CONTRATO, fecha")
target<-target  %>% 
  group_by(ID_CONTRATO) %>% 
  filter(row_number()==1)

target$ID_CONTRATO_cruce<-paste0(target$ID_CONTRATO,"_",target$fecha)
target$TARGET_BINARY_TARG<-1

#Cruzamos con la base_def
base_def$ID_CONTRATO_cruce<-paste0(base_def$ID_CONTRATO,"_",base_def$fecha)
base_def<-merge(x=base_def, y=target, by.x="ID_CONTRATO_cruce",  by.y="ID_CONTRATO_cruce",suffixes = c("",".TARGET"),all.x=TRUE)

sqldf("select TARGET_BINARY_TARG, count() from base_def group by TARGET_BINARY_TARG")

sum(!is.na(base_def$TARGET_BINARY_TARG))

#save(base_def, file="base_target.Rdata")
base_def$TARGET_BINARY_TARG[is.na(base_def$TARGET_BINARY_TARG)] <- 0
#save(base_def, file="base_def.Rdat")

#Vamos a identificar la población de 201711 que es nueva
file2<-get(paste0("contratos_",lista_ini[1]))
file1<-get(paste0("contratos_",lista_ini0))
base_aux<-sqldf("select ID_CONTRATO from file2 where ID_CONTRATO not in (select ID_CONTRATO from  file1)")
base_aux$fecha_marca<-1
base_def<-merge(x=base_def, y=base_aux, by.x="ID_CONTRATO", by.y="ID_CONTRATO",suffixes = c("",""), all.x=T)
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

perimetro_nov20117<-sqldf("select  fecha_temporal,TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA from base_def where fecha_temporal=201711   group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")

perimetro<-sqldf("select fecha_temporal, TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA from base_def group by fecha_temporal, TARGET_BINARY_TARG order by fecha_temporal, TARGET_BINARY_TARG ")
perimetro_total<-sqldf("select  TARGET_BINARY_TARG, count(TARGET_BINARY_TARG) target, sum(SAL_CAPITA) SAL_CAPITA  , sum(VLR_CUOTA) VLR_CUOTA from base_def group by  TARGET_BINARY_TARG order by  TARGET_BINARY_TARG ")


#nueva_colocacion_rara<-sqldf("select * from base_def where fecha_temporal in ('201712') and VLR_MORA>0")

write.xlsx(as.data.frame(perimetro),file=paste0("SALIDA/analisis_perimetro",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
write.xlsx(as.data.frame(perimetro_total),file=paste0("SALIDA/analisis_perimetro_total",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

write.xlsx(as.data.frame(perimetro_nov20117),file=paste0("SALIDA/analisis_perimetro_nov2017",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)
