#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
sub_ruta="/huellas_consulta/"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")
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
#for (i in 1:largo){
#  file.rename(from=lista_ini[i], to=lista_fin[i]) #Renombra el archivo
#  xlsx2csv(lista_fin[i]) #lo convierte en csv
#}

#Creación de BBDD SUPER_HUELLAS

patron="*HUELLAS*"
lista_SUPERHUELLA<-list.files(pattern=patron)
#setwd(ruta)
lista_super_cedula<-list()
lista_super_contrato<-list()
super_HUELLAS=list()

for(file in lista_SUPERHUELLA){
  rfile=list()
  rfile<-read.csv(file, header = T,sep=",")
  names(rfile)<-gsub("\\.", "_",as.character(names(rfile)))
  rfile$nombre<-paste(file)
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile) 
  rfile <- rfile[, ! names(rfile) %in% c("No__IDENTIFICACION_1","No__IDENTIFICACION__1","No__IDENTIFICACION__1"), drop = F]
  #names(rfile) %in% names(super_HUELLAS) #Para saber si todos los elemenos de una lista estan en la otra   
  super_HUELLAS<-rbind(super_HUELLAS,rfile)
  print(file)
}

save(super_HUELLAS,file="super_HUELLAS.DAT")
setwd(ruta)
#Lennys: Ver nombres y establecer estructura final, comparar con archivo de las ETL's
write.xlsx(as.data.frame(names(super_HUELLAS)),file=paste0("SALIDA/nombre_variable_super_huellas","_" ,format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = TRUE)

#Analisis rapido de frecuencias

freq(super_HUELLAS$ESTADO,useNA="ifany")
freq(super_HUELLAS$ENTIDAD,useNA="ifany")
freq(super_HUELLAS$SUCURSAL,useNA="ifany")
freq(super_HUELLAS$CIUDAD,useNA="ifany")

table(super_HUELLAS$ESTADO, super_HUELLAS$nombre,useNA="ifany")
table(super_HUELLAS$ENTIDAD, super_HUELLAS$nombre, useNA="ifany")
table(super_HUELLAS$SUCURSAL, super_HUELLAS$nombre, useNA="ifany")
table(super_HUELLAS$CIUDAD,super_HUELLAS$nombre,  useNA="ifany")

#Analisis rapido de distribuciones
percentiles<-c(0,0.05, 0.1, 0.15,0.20, 0.25, 0.30, 0.35, 0.40 , 0.45, 0.5, 0.55,0.60 , 0.65 , 0.70 ,0.75,  0.80, 0.85, 0.90, 0.95, 1)
quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC), probs = percentiles ) # quartile

quantile(as.numeric(super_HUELLAS$CUPO_TOTAL_TC), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$X__UTILIZACION), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$CUPO_TOTAL_TC_CON_LA_ENTIDAD), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$SALDO_TOTAL_TC_CON_LA_ENTIDAD), probs = percentiles ) # quartile
quantile(as.numeric(super_HUELLAS$RESULTADO_SCORE), probs = percentiles ) # quartile
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
