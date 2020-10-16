#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","openxlsx", "summarytools", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#******************* VALIDACION A NIVEL DE CLIENTE


#list.files("./CLIENTES")
#CLIENTES<-read.table(file="CLIENTES/BO-INFORMACION DE CLIENTES ENC.txt", header = T,sep=";")

#save(CLIENTES, file = "data.RData")
#load("data.RData")

#list.files("./BURO")
#results_clientes_mora=list() 

resultado_mora<-list()
lista_mora<-list.files("./mora",pattern="mora_")
super_mora=list() 
for(file in lista_mora){
  rfile=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("mora/",file)), n = 1)
  if (grepl(";", L)){
    rfile<-read.csv(gsub(" ", "",paste("mora/",file)), header = T,sep=";")
  } else{
    rfile<-read.csv(gsub(" ", "",paste("mora/",file)), header = T,sep=",")
  }
  if("VLR_CUOTA" %in% names(rfile))
  {
    rfile <- rfile[, ! names(rfile) %in% c("VLR_CUOTA"), drop = F]
  }
  
  rfile$fecha<-substr(file,str_length(file)-11, str_length(file)-4 )
  names(rfile)<-c("FECHAPROC","ID_CONTRATO","FECHA_PAGO","DEUDA_INICIAL","EN_MORA","DIAS","ESTADO_PAGO","ESTRATEGIA_RECOBRO","RIESGO","DIVISA","SALDO_CAPITAL","SALDO_INTERESES_CAPITAL","SALDO_INTERESES_MORA","LINEACREDITO","LINEA","fecha")
  super_mora<-rbind(super_mora,rfile)
  print(file)
}


save(super_mora, file = "mora.RData") #Hasta "mora_20180401.csv"

names(super_mora)

#sqldf("select fecha, count() creditos from super_mora group by fecha")

#Construcción de fecha de fecha en formato AAAAMM

super_mora$fecha2<-substr(super_mora$fecha,1,6)

sqldf("select fecha2, count() creditos from super_mora group by fecha2")

#listado de fechas de mora máxima

fechas_mora<-sqldf("select fecha2 from super_mora group by fecha2")

save(super_mora, fechas_mora, file = "mora.RData") #Hasta "mora_20180401.csv"

#Basura que puede ser útil
super_mora$largo_contrato<-str_length(super_mora$ID_CONTRATO)

sqldf("select distinct(largo_contrato), count(largo_contrato) from super_mora group by largo_contrato")

sqldf("select distinct(fecha), distinct(largo_contrato), count(largo_contrato) from super_mora group by fecha, largo_contrato order by fecha, largo_contrato")



write.table(resultado_mora,file=paste("SALIDA/resultado_mora_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

capture.output(print(resultado_mora), file = paste("SALIDA/resultado_mora_",format(today, format="%Y%m%d"),".txt", sep=""))

write.xlsx(unlist(resultado_mora), file =paste("SALIDA/resultado_mora_",format(today, format="%Y%m%d"),".xlsx", sep=""))



lista_mora<-list.files("./mora_prueba",pattern="mora_")
aux<-lista_mora[1]
super_mora=list() 
lista_mora_contrato=list()
for(aux in lista_mora){
  mora=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("mora_prueba/",aux)), n = 1)
  if (grepl(";", L)) mora<-read.csv(gsub(" ", "",paste("mora_prueba/",aux)), header = T,sep=";") else mora<-read.csv(gsub(" ", "",paste("mora_prueba/",aux)), header = T,sep=",")
  mora$fecha<-paste(aux)
    if( !("VLR_CUOTA" %in% names(mora)) )
  {
    mora$VLR_CUOTA<-"SIN DATO"
    }
  
  aux_contrato<-str_length(mora$Contrato)
  aux_contrato2<-freq(aux_contrato)
  aux2<-cbind(aux,aux_contrato2)
  print(aux2)
  lista_mora_contrato<- list.append(lista_mora_contrato,aux2)
  super_mora<-rbind(super_mora,mora)
  print(aux)
}

capture.output(print(lista_mora_contrato),file=paste("SALIDA/lista_mora_contrato_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)









lista_mora<-list.files("./mora_prueba",pattern="mora_")
aux<-lista_mora[1]
super_mora=list() 
lista_mora_contrato=list()
lista_coma=list()
lista_punto_coma=list()
lista_ok=list()
lista_VLR_CUOTA=list()
for(aux in lista_mora){
  mora=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("mora_prueba/",aux)), n = 1)
  if (grepl(";", L)){
    mora<-read.csv(gsub(" ", "",paste("mora_prueba/",aux)), header = T,sep=";")
    lista_punto_coma<-rbind(lista_punto_coma,aux)
    } 
  else{
    mora<-read.csv(gsub(" ", "",paste("mora_prueba/",aux)), header = T,sep=",")
    lista_coma<-rbind(lista_punto_coma,aux)
  }
  mora$fecha<-paste(aux)
  if( ("VLR_CUOTA" %in% names(mora)) )
  {
    mora<-mora[,-c(mora$VLR_CUOTA)]
    lista_VLR_CUOTA<-rbind(lista_VLR_CUOTA,aux)
  }
  else{
    lista_ok<-rbind(lista_ok,aux)
  }
  aux_contrato<-str_length(mora$Contrato)
  aux_contrato2<-freq(aux_contrato)
  aux2<-cbind(aux,aux_contrato2)
  lista_mora_contrato<- list.append(lista_mora_contrato,aux2)
  super_mora<-rbind(super_mora,mora)
  print(aux)
}

capture.output(print(lista_mora_contrato),file=paste("SALIDA/lista_mora_contrato_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)
capture.output(print(lista_coma),file=paste("SALIDA/lista_coma_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)
capture.output(print(lista_punto_coma),file=paste("SALIDA/lista_punto_coma_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)
capture.output(print(lista_ok),file=paste("SALIDA/lista_ok_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)
capture.output(print(lista_VLR_CUOTA),file=paste("SALIDA/lista_VLR_CUOTA_",format(today, format="%Y%m%d"),".csv", sep=""), split = FALSE)

lista_coma=list()
lista_punto_coma=list()
lista_ok=list()
lista_VLR_CUOTA=list()
names(mora)
nombre_mora<-names(mora)
nombre_mora
c("FECHAPROC",               "ID_CONTRATO"    ,         "FECHA_PAGO",             
, "DEUDA_INICIAL" ,          "EN_MORA"      ,           "DIAS"                   
[7] "ESTADO_PAGO"             "ESTRATEGIA_RECOBRO"      "RIESGO"                 
[10] "DIVISA"                  "SALDO_CAPITAL"           "SALDO_INTERESES_CAPITAL"
[13] "SALDO_INTERESES_MORA"    "LINEACREDITO"            "LINEA"                  
[16] "fecha" 

