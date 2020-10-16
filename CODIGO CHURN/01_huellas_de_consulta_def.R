#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
sub_ruta="/HUELLAS_DEF/"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Renombremos las HUELLAS DE CONSULTA
setwd(paste0(ruta,sub_ruta))
lista_ini<-list.files(pattern="*HUELLAS*")
lista_fin<-gsub(" ", "_",list.files(pattern="*HUELLAS*"))
lista_fin<-gsub("2.016", "2016",lista_fin)
lista_fin<-gsub("2.017", "2017",lista_fin)
lista_fin<-gsub("2.018", "2018",lista_fin)
lista_fin<-gsub(" ", "",lista_fin)
lista_fin<-gsub("BASE_", "",lista_fin)
lista_fin<-gsub("CONSULTA_", "",lista_fin)

largo<-length(lista_ini)
#Para renombrar y convertir xls y xlsx a csv
 for (i in 1:largo){
   file.rename(from=lista_ini[i], to=lista_fin[i]) #Renombra el archivo
#   xlsx2csv(lista_fin[i]) #lo convierte en csv
 }

#Creación de BBDD SUPER_HUELLAS
setwd(paste0(ruta,sub_ruta))
patron="*HUELLAS*"
lista_SUPERHUELLA<-list.files(pattern=patron)
#setwd(ruta)
lista_super_cedula<-list()
lista_super_contrato<-list()
super_HUELLAS=list()

for(file in lista_SUPERHUELLA){
  rfile=list()
  rfile<-read_excel(file)
  names(rfile)<-gsub("\\.", "_",as.character(names(rfile)))
  #names(rfile)<-gsub(" ", "_",as.character(names(file)))
  rfile$nombre_archivo<-paste(file)
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile) 
  rfile <- rfile[, ! names(rfile) %in% c("No__IDENTIFICACION_1","No__IDENTIFICACION__1","No__IDENTIFICACION__1"), drop = F]
  #names(rfile) %in% names(super_HUELLAS) #Para saber si todos los elemenos de una lista estan en la otra   
  super_HUELLAS<-rbind(super_HUELLAS,rfile)
  print(file)
}

names(super_HUELLAS)<-gsub(" ", "_",as.character(names(super_HUELLAS)))

#save(super_HUELLAS,file="super_HUELLAS_0.DAT")
#load(file="super_HUELLAS.DAT")

#Validacion largo de la cedula

largo_CEDULA<-str_length(super_HUELLAS$No__IDENTIFICACION)
freq(as.numeric(largo_CEDULA), useNA="ifany")

#Huellas por mes
super_HUELLAS$FECHA_HUELLA2<-as.numeric(gsub("-", "", format(as.Date(super_HUELLAS$FECHA_HUELLA), "%Y-%m")))
names(super_HUELLAS)
freq(super_HUELLAS$FECHA_HUELLA2, useNA="ifany")

super_HUELLAS<-sqldf("select * from super_HUELLAS where FECHA_HUELLA2>201610 AND FECHA_HUELLA2<201809")

freq(super_HUELLAS$FECHA_HUELLA2, useNA="ifany")

setwd(ruta)

percentiles<-c(0,0.05, 0.1, 0.15,0.20, 0.25, 0.30, 0.35, 0.40 , 0.45, 0.5, 0.55,0.60 , 0.65 , 0.70 ,0.75,  0.80, 0.85, 0.90, 0.95, 1)
quantile(as.numeric(super_HUELLAS$RESULTADO_SCORE), probs = percentiles, na.rm=TRUE ) # quartile
sum(is.na(super_HUELLAS$RESULTADO_SCORE), na.rm=TRUE)

# variables<-c("No__IDENTIFICACION", "ESTADO",                        "ENTIDAD",                  
#              "FECHA_HUELLA"     ,             "SUCURSAL",                      "CIUDAD"  ,                     
#              "CUPO_TOTAL_TC"     ,            "SALDO_TOTAL_TC",                "%_UTILIZACION",                
#              "CUPO_TOTAL_TC_CON_LA_ENTIDAD",  "SALDO_TOTAL_TC_CON_LA_ENTIDAD" ,"RESULTADO_SCORE",              
#              "SALDO_CONSUMO_MERCADO"        , "SALDO_HIPOTECARIO_MERCADO"    , "ENDEUDAMIENTO_TOTAL_S_FINAN" ,            
#              "FECHA_HUELLA2")     

freq(super_HUELLAS$ESTADO,useNA="ifany")

#super_HUELLAS<-sqldf("select * from super_HUELLAS where ESTADO='VIGENTE' ")

super_HUELLAS$CLAVE_HUELLAS<-paste0(super_HUELLAS$No__IDENTIFICACION_HUELLAS,"_",super_HUELLAS$FECHA_HUELLA2_HUELLAS)
#Creación de dummys
freq(super_HUELLAS$ENTIDAD,useNA="ifany")

write.xlsx(freq(super_HUELLAS$ENTIDAD,useNA="ifany"),file=paste0("SALIDA/Huellas_entidad_","_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = TRUE, col.names = TRUE)

super_HUELLAS$entidad2<-ifelse(super_HUELLAS$ENTIDAD %in% c("BANCOLOMBIA"),"BANCOLOMBIA", ifelse(super_HUELLAS$ENTIDAD %in% c("AV VILLAS"), "AV_VILLAS", ifelse( super_HUELLAS$ENTIDAD %in% c("BBVA  COLOMBIA"), "BBVA" ,ifelse(super_HUELLAS$ENTIDAD %in% c("DE BOGOTA") , "BOGOTA", ifelse(super_HUELLAS$ENTIDAD %in% c("GNB  SUDAMERIS"), "SUDAMERIS","OTROS")))))
freq(super_HUELLAS$entidad2, useNA="ifany")

super_HUELLAS$BANCOLOMBIA<-ifelse(super_HUELLAS$entidad2 %in% c("BANCOLOMBIA"),1,0)
freq(super_HUELLAS$BANCOLOMBIA, useNA="ifany")

super_HUELLAS$AV_VILLAS<-ifelse(super_HUELLAS$entidad2 %in% c("AV_VILLAS"),1,0)
freq(super_HUELLAS$AV_VILLAS, useNA="ifany")

super_HUELLAS$BBVA<-ifelse(super_HUELLAS$entidad2 %in% c("BBVA"),1,0)
freq(super_HUELLAS$BBVA, useNA="ifany")

super_HUELLAS$BOGOTA<-ifelse(super_HUELLAS$entidad2 %in% c("BOGOTA"),1,0)
freq(super_HUELLAS$BOGOTA, useNA="ifany")

super_HUELLAS$SUDAMERIS<-ifelse(super_HUELLAS$entidad2 %in% c("SUDAMERIS"),1,0)
freq(super_HUELLAS$SUDAMERIS, useNA="ifany")

super_HUELLAS$OTROS<-ifelse(super_HUELLAS$entidad2 %in% c("OTROS"),1,0)
freq(super_HUELLAS$OTROS, useNA="ifany")

#Huellas por día
super_HUELLAS$FECHA_HUELLA3<-as.numeric(gsub("-","",as.Date(super_HUELLAS$FECHA_HUELLA)))

#Contar numero de huellas por mes

super_HUELLAS$clave_num_Huella_mes<-paste0(super_HUELLAS$No__IDENTIFICACION,"_",super_HUELLAS$FECHA_HUELLA2)
#Huellas_mes<-sqldf("select No__IDENTIFICACION, count(clave_num_Huella_mes)  num_consultas, max(FECHA_HUELLA3) fecha_ultima_consulta, FECHA_HUELLA2 from super_HUELLAS group by No__IDENTIFICACION, FECHA_HUELLA2")

colnames(super_HUELLAS)[colnames(super_HUELLAS)=="%_UTILIZACION"] <- "P_UTILIZACION"
Huellas_mes<-sqldf("select No__IDENTIFICACION, FECHA_HUELLA2, count(clave_num_Huella_mes)  num_consultas_huellas , max(FECHA_HUELLA3) f_ult_huella,  min(RESULTADO_SCORE) min_RESULTADO_SCORE_huellas, sum(BANCOLOMBIA) Huellas_BANCOLOMBIA, sum(AV_VILLAS) Huellas_AV_VILLAS, sum(BBVA) Huellas_BBVA, sum(BOGOTA) Huellas_BOGOTA,  sum(SUDAMERIS) Huellas_SUDAMERIS, sum(OTROS) Huellas_OTROS   from super_HUELLAS group by No__IDENTIFICACION, FECHA_HUELLA2")


#Huellas_mes$f_ult_huella

Huellas_mes$mes_huella<- substr(Huellas_mes$f_ult_huella,5,6)

freq(Huellas_mes$mes_huella,useNA="ifany")

Huellas_mes$dia_huella<-weekdays(as.Date(as.character(Huellas_mes$f_ult_huella),format="%Y%m%d"))

freq(Huellas_mes$dia_huella,useNA="ifany")

Huellas_mes$semana_mes_huella<-ceiling(day(as.Date(as.character(Huellas_mes$f_ult_huella),format="%Y%m%d") )/7)
freq(Huellas_mes$semana_mes_huella,useNA="ifany")

Huellas_mes$semana_anio_huella<-strftime(as.Date(as.character(Huellas_mes$f_ult_huella),format="%Y%m%d"), format = "%V")
freq(Huellas_mes$semana_anio_huella,useNA="ifany")

#Ojo verificar
#save(super_HUELLAS,file="super_HUELLAS_DEF.DAT")
freq(Huellas_mes$num_consultas_huellas,  useaNA="ifany")
sqldf("select * from Huellas_mes where num_consultas_huellas>100")

super_HUELLAS<-Huellas_mes

save(super_HUELLAS ,file="super_HUELLAS_DEF.DAT")

#write.table(Huellas_mes,file=paste0("SALIDA/Huellas_","_" ,format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
#write.table(super_HUELLAS,file=paste0("SALIDA/HBBDD_Huellas","_" ,format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#names(super_HUELLAS)<-paste0(names(super_HUELLAS),"_HUELLAS")

#Basura que puede ser útil

#load(file="super_HUELLAS_DEF.DAT")

#Lennys: Ver nombres y establecer estructura final, comparar con archivo de las ETL's
#write.xlsx(as.data.frame(names(super_HUELLAS)),file=paste0("SALIDA/nombre_variable_super_huellas","_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

#Analisis rapido de frecuencias

freq(super_HUELLAS$ESTADO,useNA="ifany")
#Filtro elimino vigentes
super_HUELLAS<-sqldf("select * from super_HUELLAS where ESTADO='VIGENTE'")
freq(super_HUELLAS$ESTADO,useNA="ifany")
freq(super_HUELLAS$ENTIDAD,useNA="ifany")
freq(super_HUELLAS$SUCURSAL,useNA="ifany")
freq(super_HUELLAS$CIUDAD,useNA="ifany")


#Analisis rapido de distribuciones


quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC), probs = percentiles ) # quartile

quantile(as.numeric(super_HUELLAS$CUPO_TOTAL_TC), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$X__UTILIZACION), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$CUPO_TOTAL_TC_CON_LA_ENTIDAD), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC_CON_LA_ENTIDAD), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_CONSUMO_MERCADO), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_HIPOTECARIO_MERCADO), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$ENDEUDAMIENTO_TOTAL_S_FINAN), probs = percentiles ) # quartile

aggregate(super_HUELLAS$CUPO_TOTAL_TC, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$SALDO_TOTAL_TC, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$X__UTILIZACION, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$CUPO_TOTAL_TC_CON_LA_ENTIDAD, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$SALDO_TOTAL_TC_CON_LA_ENTIDAD, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$RESULTADO_SCORE, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$SALDO_CONSUMO_MERCADO, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$SALDO_HIPOTECARIO_MERCADO, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
aggregate(super_HUELLAS$ENDEUDAMIENTO_TOTAL_S_FINAN, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))

write.csv(super_HUELLAS, file = "huellas_consulta/super_HUELLAS.csv")
