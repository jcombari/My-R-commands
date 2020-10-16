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

sub_ruta="./CLIENTES_S3_ZIP2/";
lista_clientes<-list.files(sub_ruta,pattern="clientes_")
ventana_back<-6

#Limpieza para el S3
#file<-lista_clientes[1]

for(file in lista_clientes){
  rfile<-read.csv(gsub(" ", "",paste(sub_ruta,file)), header = T,sep=";")
  names(rfile)<-c("AA_NIT","AA_TIPO_DOC","ID_CLIENTE","GENERO","ESTADO_CIVIL","FECHA_NACIMIENTO","OCUPACION_OFICIO","NIVEL_EDUCACION","PROFESION","NUM_HIJOS","ZONA_UBICACION","PAIS","NOMBRE_DEPTO","NOMBRE_CIUDAD","DIRECCION","TOTAL_ACTIVOS","TOTAL_PASIVOS","INGRESO_BRUTOS","OTROS_INGRESOS")
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile)  
  #write.csv(rfile, file = paste0("SALIDA/",file))
} 



