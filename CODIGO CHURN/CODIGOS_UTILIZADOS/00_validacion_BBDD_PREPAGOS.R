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

load("data.RData")

des<- str(PREPAGOS)

#Exportar resumen
capture.output(print(names(PREPAGOS), na.print=''), file=paste("SALIDA/PREPAGOS_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(PREPAGOS), na.print=''), file=paste("SALIDA/PREPAGOS_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))

#Analisis de variables


#install.packages("summarytools")
#install.packages("rmarkdown")

library(summarytools)
library(rmarkdown)

PREPAGOS$Fecha2<-gsub(" ", "",paste(substr(PREPAGOS$FEC_APECUENTA, 1, 4),substr(PREPAGOS$FEC_APECUENTA, 6, 7)))
#Resumen_fecha2<-PREPAGOS %>% freq(Fecha2)

#Numero de variables
size(names(PREPAGOS))

#Analisis cualitativo
PREPAGOS_subset <- as.data.frame(PREPAGOS[ ,c("NOM_PRODUCTO","NOM_OFICINA","FEC_APECUENTA","NOM_OFICINAH","NOM_CIUDAD","NOMBRE_REGIONAL","NOM_DEPARTAMENTO","CIUDAD_OFICINA","Fecha2","COD_OFICINAH")])

#Eliminar espaciones en blanco de todo el dataset

PREPAGOS_nombres<-names(PREPAGOS_subset)

for (aux in PREPAGOS_nombres){
  PREPAGOS_subset[,aux]<-gsub(" ", "_",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("Á", "A",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("á", "a",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("É", "E",PREPAGOS_subset[,aux]) 
  PREPAGOS_subset[,aux]<-gsub("é", "e",PREPAGOS_subset[,aux])  
  PREPAGOS_subset[,aux]<-gsub("í", "i",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("Í", "I",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("ó", "o",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("Ó", "O",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("Ú", "U",PREPAGOS_subset[,aux])
  PREPAGOS_subset[,aux]<-gsub("ú", "u",PREPAGOS_subset[,aux])
}

#

#view(dfSummary(PREPAGOS_subset))
#Para excluir
#DATOS_S <- subset(PREPAGOS_subset, select = -c(Edad, CEDULAENC))

freq_tables_PREPAGOS <- lapply(PREPAGOS_subset, freq)

capture.output(print(freq_tables_PREPAGOS), file = paste("SALIDA/PREPAGOS_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_PREPAGOS , file = "frecuencia_PREPAGOS.RData")

PREPAGOS_subset %>% freq(NOM_PRODUCTO)
freq(PREPAGOS_subset$NOM_PRODUCTO, style = "rmarkdown")

gsub(" ", "",PREPAGOS_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

PREPAGOS_subset2 <- as.data.frame(PREPAGOS[ ,c("FEB15","MAR15","ABR15","MAY15","JUN15","JUL15","AGO15","SEP15","OCT15","NOV15","DIC15","ENE16","FEB16","MAR16","ABR16","MAY16","JUN16","JUL16","AGO16","SEP16","OCT16","NOV16","DIC16","ENE17","FEB17","MAR17","ABR17","MAY17","JUN17","JUL17","AGO17","SEP17","OCT17","NOV17","DIC17","ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","CNT_PLAZO")])
PREPAGOS_subset2$Total_Egresos_Mensuales<-as.numeric(PREPAGOS_subset2$Total_Egresos_Mensuales)
class(PREPAGOS_subset2$Total_Egresos_Mensuales)
PREPAGOS_subset2$Ingresos_Brutos_Mensuales<-as.numeric(PREPAGOS_subset2$Ingresos_Brutos_Mensuales)

quantile(PREPAGOS_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(PREPAGOS_subset[,aux], probs = seq(0, 1, by= 0.05)) 

results_PREPAGOS_percentil=list() 
nombre_PREPAGOS2<-names(PREPAGOS_subset2)
for(aux in nombre_PREPAGOS2){
  PREPAGOS_subset2[,aux]<-as.numeric(PREPAGOS_subset2[,aux])
  aux2<-quantile(PREPAGOS_subset2[,aux], probs = seq(0, 1, by= 0.05), na.rm= TRUE) 
  aux2<-rbind(aux,aux2)
  results_PREPAGOS_percentil<- list.append(results_PREPAGOS_percentil,aux2)
  print(aux)
}

capture.output(print(results_PREPAGOS_percentil), file=paste("SALIDA/results_PREPAGOS_percentil_",format(today, format="%Y%m%d"),".txt", sep=""))
write.table(as.data.frame(results_PREPAGOS_percentil),file=paste("SALIDA/results_PREPAGOS_percentil_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Validacion numero de largo del contrato

PREPAGOS$largo_contrato<-str_length(PREPAGOS$OBLIGACIONENC)
view(freq(PREPAGOS$largo_contrato, style = "rmarkdown"))

PREPAGOS$largo_cedula<-str_length(PREPAGOS$CEDULAENC)
view(freq(PREPAGOS$largo_cedula, style = "rmarkdown"))

