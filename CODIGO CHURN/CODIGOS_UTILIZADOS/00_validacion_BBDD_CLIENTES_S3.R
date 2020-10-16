#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#load("CLIENTES.RData")

#Creación de la CLIENTES

lista_CLIENTES<-list.files("./CLIENTES_S3",pattern="clientes_2018")
lista_CLIENTES_cedula<-list()
lista_CLIENTES_contrato<-list()
SUPER_CLIENTES=list()

for(file in lista_CLIENTES){
  rfile=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("CLIENTES_S3/",file)), n = 1)
  if (grepl(";", L)){
    rfile<-read.csv(gsub(" ", "",paste("CLIENTES_S3/",file)), header = T,sep=";")
  } else{
    rfile<-read.csv(gsub(" ", "",paste("CLIENTES_S3/",file)), header = T,sep=",", quote = "\"")
  }
  
  if (!("AA_NIT" %in% names(rfile))){
   rfile$AA_NIT<-""
   colnames(rfile)[which(names(rfile) == "ID_CLIENTE")]<-"AA_NIT_ENCRIPTADO"
  } 
  #else{
  #  rfile<-read.csv(gsub(" ", "",paste("CLIENTES_S3/",file)), header = T,sep=",\")
  #}
  rfile$fecha<-substr(file,str_length(file)-9, str_length(file)-4 )
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile)
  SUPER_CLIENTES<-rbind(SUPER_CLIENTES,rfile)
  print(file)
}

bbdd2017<-SUPER_CLIENTES

save(bbdd2017,file="clientes.Rdata")

file<- "clientes_201805.csv"
L <- readLines(gsub(" ", "",paste("CLIENTES_S3/",file)), n = 1)
file<- "clientes_201806.csv"
L2 <- readLines(gsub(" ", "",paste("CLIENTES_S3/",file)), n = 1)


rfile<-read.csv(gsub(" ", "",paste("CLIENTES_S3/",file)), header = T, sep = ",", quote = "\"")


fread(paste("CLIENTES_S3/",file))

CLIENTES<-CLIENTES_CLIENTES
des<- str(CLIENTES)

#Exportar resumen
capture.output(print(names(CLIENTES), na.print=''), file=paste("SALIDA/CLIENTES_S3_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(CLIENTES), na.print=''), file=paste("SALIDA/CLIENTES_S3_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(lista_CLIENTES_contrato, na.print=''), file=paste("SALIDA/CLIENTES_S3_lista_contrato_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(lista_CLIENTES_cedula, na.print=''), file=paste("SALIDA/CLIENTES_S3_lista_cedula_",format(today, format="%Y%m%d"),".txt", sep=""))
#Analisis de variables


#install.packages("summarytools")
#install.packages("rmarkdown")

library(summarytools)
library(rmarkdown)

CLIENTES$Fecha2<-gsub(" ", "",paste(substr(CLIENTES$FEC_APECUENTA, 1, 4),substr(CLIENTES$FEC_APECUENTA, 6, 7)))
#Resumen_fecha2<-CLIENTES %>% freq(Fecha2)

#Numero de variables
size(names(CLIENTES))

#Analisis cualitativo
CLIENTES_subset <- as.data.frame(CLIENTES[ ,c("NOM_PRODUCTO","NOM_OFICINA","FEC_APECUENTA","NOM_OFICINAH","NOM_CIUDAD","NOMBRE_REGIONAL","NOM_DEPARTAMENTO","CIUDAD_OFICINA","Fecha2","COD_OFICINAH")])

#Eliminar espaciones en blanco de todo el dataset

CLIENTES_nombres<-names(CLIENTES_subset)

for (aux in CLIENTES_nombres){
  CLIENTES_subset[,aux]<-gsub(" ", "_",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("Á", "A",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("á", "a",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("É", "E",CLIENTES_subset[,aux]) 
  CLIENTES_subset[,aux]<-gsub("é", "e",CLIENTES_subset[,aux])  
  CLIENTES_subset[,aux]<-gsub("í", "i",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("Í", "I",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("ó", "o",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("Ó", "O",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("Ú", "U",CLIENTES_subset[,aux])
  CLIENTES_subset[,aux]<-gsub("ú", "u",CLIENTES_subset[,aux])
}

#

#view(dfSummary(CLIENTES_subset))
#Para excluir
#DATOS_S <- subset(CLIENTES_subset, select = -c(Edad, CEDULAENC))

freq_tables_CLIENTES <- lapply(CLIENTES_subset, freq)

capture.output(print(freq_tables_CLIENTES), file = paste("SALIDA/CLIENTES_S3_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_CLIENTES , file = "frecuencia_CLIENTES.RData")

CLIENTES_subset %>% freq(NOM_PRODUCTO)
freq(CLIENTES_subset$NOM_PRODUCTO, style = "rmarkdown")

gsub(" ", "",CLIENTES_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

CLIENTES_subset2 <- as.data.frame(CLIENTES[ ,c("FEB15","MAR15","ABR15","MAY15","JUN15","JUL15","AGO15","SEP15","OCT15","NOV15","DIC15","ENE16","FEB16","MAR16","ABR16","MAY16","JUN16","JUL16","AGO16","SEP16","OCT16","NOV16","DIC16","ENE17","FEB17","MAR17","ABR17","MAY17","JUN17","JUL17","AGO17","SEP17","OCT17","NOV17","DIC17","ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","CNT_PLAZO")])
CLIENTES_subset2$Total_Egresos_Mensuales<-as.numeric(CLIENTES_subset2$Total_Egresos_Mensuales)
class(CLIENTES_subset2$Total_Egresos_Mensuales)
CLIENTES_subset2$Ingresos_Brutos_Mensuales<-as.numeric(CLIENTES_subset2$Ingresos_Brutos_Mensuales)

quantile(CLIENTES_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(CLIENTES_subset[,aux], probs = seq(0, 1, by= 0.05)) 

results_CLIENTES_percentil=list() 
nombre_CLIENTES2<-names(CLIENTES_subset2)
for(aux in nombre_CLIENTES2){
  CLIENTES_subset2[,aux]<-as.numeric(CLIENTES_subset2[,aux])
  aux2<-quantile(CLIENTES_subset2[,aux], probs = seq(0, 1, by= 0.05), na.rm= TRUE) 
  aux2<-rbind(aux,aux2)
  results_CLIENTES_percentil<- list.append(results_CLIENTES_percentil,aux2)
  print(aux)
}

capture.output(print(results_CLIENTES_percentil), file=paste("SALIDA/results_CLIENTES_percentil_",format(today, format="%Y%m%d"),".txt", sep=""))
write.table(as.data.frame(results_CLIENTES_percentil),file=paste("SALIDA/results_CLIENTES_percentil_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Validacion numero de largo del contrato

CLIENTES$largo_contrato<-str_length(CLIENTES$OBLIGACIONENC)
view(freq(CLIENTES$largo_contrato, style = "rmarkdown"))

CLIENTES$largo_cedula<-str_length(CLIENTES$CEDULAENC)
view(freq(CLIENTES$largo_cedula, style = "rmarkdown"))

