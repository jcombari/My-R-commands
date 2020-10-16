#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

load("data.RData")

Buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")

des<- str(CAPTACION)

#Exportar resumen
capture.output(print(names(CAPTACION), na.print=''), file=paste("SALIDA/CAPTACION_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(CAPTACION), na.print=''), file=paste("SALIDA/CAPTACION_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))

#Analisis de variables


#install.packages("summarytools")
#install.packages("rmarkdown")

library(summarytools)
library(rmarkdown)

CAPTACION$Fecha2<-gsub(" ", "",paste(substr(CAPTACION$FEC_APECUENTA, 1, 4),substr(CAPTACION$FEC_APECUENTA, 6, 7)))
#Resumen_fecha2<-CAPTACION %>% freq(Fecha2)

#Numero de variables
size(names(CAPTACION))

#Analisis cualitativo
CAPTACION_subset <- as.data.frame(CAPTACION[ ,c("NOM_PRODUCTO","NOM_OFICINA","FEC_APECUENTA","NOM_OFICINAH","NOM_CIUDAD","NOMBRE_REGIONAL","NOM_DEPARTAMENTO","CIUDAD_OFICINA","Fecha2","COD_OFICINAH")])

#Eliminar espaciones en blanco de todo el dataset

CAPTACION_nombres<-names(CAPTACION_subset)

for (aux in CAPTACION_nombres){
  CAPTACION_subset[,aux]<-gsub(" ", "_",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("Á", "A",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("á", "a",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("É", "E",CAPTACION_subset[,aux]) 
  CAPTACION_subset[,aux]<-gsub("é", "e",CAPTACION_subset[,aux])  
  CAPTACION_subset[,aux]<-gsub("í", "i",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("Í", "I",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("ó", "o",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("Ó", "O",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("Ú", "U",CAPTACION_subset[,aux])
  CAPTACION_subset[,aux]<-gsub("ú", "u",CAPTACION_subset[,aux])
}

#

#view(dfSummary(CAPTACION_subset))
#Para excluir
#DATOS_S <- subset(CAPTACION_subset, select = -c(Edad, CEDULAENC))

freq_tables_CAPTACION <- lapply(CAPTACION_subset, freq)

capture.output(print(freq_tables_CAPTACION), file = paste("SALIDA/CAPTACION_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_CAPTACION , file = "frecuencia_CAPTACION.RData")

CAPTACION_subset %>% freq(NOM_PRODUCTO)
freq(CAPTACION_subset$NOM_PRODUCTO, style = "rmarkdown")

gsub(" ", "",CAPTACION_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

CAPTACION_subset2 <- as.data.frame(CAPTACION[ ,c("FEB15","MAR15","ABR15","MAY15","JUN15","JUL15","AGO15","SEP15","OCT15","NOV15","DIC15","ENE16","FEB16","MAR16","ABR16","MAY16","JUN16","JUL16","AGO16","SEP16","OCT16","NOV16","DIC16","ENE17","FEB17","MAR17","ABR17","MAY17","JUN17","JUL17","AGO17","SEP17","OCT17","NOV17","DIC17","ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","CNT_PLAZO")])
CAPTACION_subset2$Total_Egresos_Mensuales<-as.numeric(CAPTACION_subset2$Total_Egresos_Mensuales)
class(CAPTACION_subset2$Total_Egresos_Mensuales)
CAPTACION_subset2$Ingresos_Brutos_Mensuales<-as.numeric(CAPTACION_subset2$Ingresos_Brutos_Mensuales)

quantile(CAPTACION_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(CAPTACION_subset[,aux], probs = seq(0, 1, by= 0.05)) 

results_captacion_percentil=list() 
nombre_captacion2<-names(CAPTACION_subset2)
for(aux in nombre_captacion2){
  CAPTACION_subset2[,aux]<-as.numeric(CAPTACION_subset2[,aux])
  aux2<-quantile(CAPTACION_subset2[,aux], probs = seq(0, 1, by= 0.05), na.rm= TRUE) 
  aux2<-rbind(aux,aux2)
  results_captacion_percentil<- list.append(results_captacion_percentil,aux2)
  print(aux)
}

capture.output(print(results_captacion_percentil), file=paste("SALIDA/results_captacion_percentil_",format(today, format="%Y%m%d"),".txt", sep=""))
write.list(results_captacion_percentil, prueba)
write.table(as.data.frame(results_captacion_percentil),file=paste("SALIDA/results_captacion_percentil_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Validacion numero de largo del contrato

CAPTACION$largo_contrato<-str_length(CAPTACION$OBLIGACIONENC)
view(freq(CAPTACION$largo_contrato, style = "rmarkdown"))

CAPTACION$largo_cedula<-str_length(CAPTACION$CEDULAENC)
view(freq(CAPTACION$largo_cedula, style = "rmarkdown"))

