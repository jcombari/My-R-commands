#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/01_Cobranza/02_Temprana_bucket_1/02_POPULAR/Backtesting/MASTER"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

lista_MASTER<-list.files(pattern="masterTable_")
lista_MASTER_cedula<-list()
lista_MASTER_contrato<-list()
SUPER_MASTER=list()

for(file in lista_MASTER){
  rfile=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",file), n = 1)
  if (grepl(";", L)){
    rfile<-read.csv(gsub(" ", "",file), header = T,sep=";")
  } else{
    rfile<-read.csv(gsub(" ", "",file), header = T,sep=",")
  }
  rfile$fecha<-substr(file,str_length(file)-11, str_length(file)-4 )
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile)
  SUPER_MASTER<-rbind(SUPER_MASTER,rfile)
  print(file)
}

#masterTable_20180201.csv
write.xlsx(names(rfile),file=paste0("Var_B2_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#masterTable_20180123.csv
write.xlsx(names(SUPER_MASTER),file=paste0("Var_B1_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
