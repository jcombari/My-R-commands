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

des<- str(COLOCACION)

#Exportar resumen
capture.output(print(names(COLOCACION), na.print=''), file=paste("SALIDA/COLOCACION_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(COLOCACION), na.print=''), file=paste("SALIDA/COLOCACION_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))

#Analisis de variables


#install.packages("summarytools")
#install.packages("rmarkdown")

library(summarytools)
library(rmarkdown)

COLOCACION$Fecha2<-gsub(" ", "",paste(substr(COLOCACION$FEC_APECUENTA, 1, 4),substr(COLOCACION$FEC_APECUENTA, 6, 7)))
#Resumen_fecha2<-COLOCACION %>% freq(Fecha2)

#Numero de variables
size(names(COLOCACION))

#Analisis cualitativo
COLOCACION_subset <- as.data.frame(COLOCACION[ ,c("NOM_PRODUCTO","NOM_OFICINA","FEC_APECUENTA","NOM_OFICINAH","NOM_CIUDAD","NOMBRE_REGIONAL","NOM_DEPARTAMENTO","CIUDAD_OFICINA","Fecha2","COD_OFICINAH")])

#Eliminar espaciones en blanco de todo el dataset

COLOCACION_nombres<-names(COLOCACION_subset)

for (aux in COLOCACION_nombres){
  COLOCACION_subset[,aux]<-gsub(" ", "_",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("Á", "A",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("á", "a",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("É", "E",COLOCACION_subset[,aux]) 
  COLOCACION_subset[,aux]<-gsub("é", "e",COLOCACION_subset[,aux])  
  COLOCACION_subset[,aux]<-gsub("í", "i",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("Í", "I",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("ó", "o",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("Ó", "O",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("Ú", "U",COLOCACION_subset[,aux])
  COLOCACION_subset[,aux]<-gsub("ú", "u",COLOCACION_subset[,aux])
}

#

#view(dfSummary(COLOCACION_subset))
#Para excluir
#DATOS_S <- subset(COLOCACION_subset, select = -c(Edad, CEDULAENC))

freq_tables_COLOCACION <- lapply(COLOCACION_subset, freq)

capture.output(print(freq_tables_COLOCACION), file = paste("SALIDA/COLOCACION_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_COLOCACION , file = "frecuencia_COLOCACION.RData")

COLOCACION_subset %>% freq(NOM_PRODUCTO)
freq(COLOCACION_subset$NOM_PRODUCTO, style = "rmarkdown")

gsub(" ", "",COLOCACION_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

COLOCACION_subset2 <- as.data.frame(COLOCACION[ ,c("FEB15","MAR15","ABR15","MAY15","JUN15","JUL15","AGO15","SEP15","OCT15","NOV15","DIC15","ENE16","FEB16","MAR16","ABR16","MAY16","JUN16","JUL16","AGO16","SEP16","OCT16","NOV16","DIC16","ENE17","FEB17","MAR17","ABR17","MAY17","JUN17","JUL17","AGO17","SEP17","OCT17","NOV17","DIC17","ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","CNT_PLAZO")])
COLOCACION_subset2$Total_Egresos_Mensuales<-as.numeric(COLOCACION_subset2$Total_Egresos_Mensuales)
class(COLOCACION_subset2$Total_Egresos_Mensuales)
COLOCACION_subset2$Ingresos_Brutos_Mensuales<-as.numeric(COLOCACION_subset2$Ingresos_Brutos_Mensuales)

quantile(COLOCACION_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(COLOCACION_subset[,aux], probs = seq(0, 1, by= 0.05)) 

results_COLOCACION_percentil=list() 
nombre_COLOCACION2<-names(COLOCACION_subset2)
for(aux in nombre_COLOCACION2){
  COLOCACION_subset2[,aux]<-as.numeric(COLOCACION_subset2[,aux])
  aux2<-quantile(COLOCACION_subset2[,aux], probs = seq(0, 1, by= 0.05), na.rm= TRUE) 
  aux2<-rbind(aux,aux2)
  results_COLOCACION_percentil<- list.append(results_COLOCACION_percentil,aux2)
  print(aux)
}

capture.output(print(results_COLOCACION_percentil), file=paste("SALIDA/results_COLOCACION_percentil_",format(today, format="%Y%m%d"),".txt", sep=""))
write.list(results_COLOCACION_percentil, prueba)
write.table(as.data.frame(results_COLOCACION_percentil),file=paste("SALIDA/results_COLOCACION_percentil_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Validacion numero de largo del contrato

COLOCACION$largo_contrato<-str_length(COLOCACION$OBLIGACIONENC)
view(freq(COLOCACION$largo_contrato, style = "rmarkdown"))

COLOCACION$largo_cedula<-str_length(COLOCACION$CEDULAENC)
view(freq(COLOCACION$largo_cedula, style = "rmarkdown"))

