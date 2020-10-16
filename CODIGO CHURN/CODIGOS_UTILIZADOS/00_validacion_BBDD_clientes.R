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
names(CLIENTES)<- c("Tipo_Documento","Ciudad_Nacimiento","Edad","Segmento","Ocupacion","Profesion","Actividad_Economica","Nivel_Educativo","Estado_Civil","Fecha_Actualizacion","Ingresos_Brutos_Mensuales","Total_Egresos_Mensuales","Ciudad_Dir_Ppal","CEDULAENC")

des<- str(CLIENTES)

#Exportar resumen
capture.output(print(names(CLIENTES), na.print=''), file=paste("SALIDA/CLIENTES_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(CLIENTES), na.print=''), file=paste("SALIDA/CLIENTES_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))

#Analisis de variables


#install.packages("summarytools")
#install.packages("rmarkdown")

library(summarytools)
library(rmarkdown)

CLIENTES$Fecha2<-gsub(" ", "",paste(substr(CLIENTES$Fecha_Actualizacion, 1, 4),substr(CLIENTES$Fecha_Actualizacion, 6, 7)))
#Resumen_fecha2<-CLIENTES %>% freq(Fecha2)

#Numero de variables
size(names(CLIENTES))

#Analisis cualitativo
clientes_subset <- as.data.frame(CLIENTES[ ,c("Tipo_Documento","Ciudad_Nacimiento","Segmento","Ocupacion","Profesion","Actividad_Economica","Nivel_Educativo","Estado_Civil","Fecha2","Ciudad_Dir_Ppal")])

#Eliminar espaciones en blanco de todo el dataset

clientes_nombres<-names(clientes_subset)

for (aux in clientes_nombres){
  clientes_subset[,aux]<-gsub(" ", "_",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("Á", "A",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("á", "a",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("É", "E",clientes_subset[,aux]) 
  clientes_subset[,aux]<-gsub("é", "e",clientes_subset[,aux])  
  clientes_subset[,aux]<-gsub("í", "i",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("Í", "I",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("ó", "o",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("Ó", "O",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("Ú", "U",clientes_subset[,aux])
  clientes_subset[,aux]<-gsub("ú", "u",clientes_subset[,aux])
}

#

#view(dfSummary(clientes_subset))
#Para excluir
#DATOS_S <- subset(clientes_subset, select = -c(Edad, CEDULAENC))

freq_tables_clientes <- lapply(clientes_subset, freq)

capture.output(print(freq_tables_clientes), file = paste("SALIDA/CLIENTES_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_clientes , file = "frecuencia_clientes.RData")


gsub(" ", "",clientes_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

clientes_subset2 <- as.data.frame(CLIENTES[ ,c("Total_Egresos_Mensuales","Ingresos_Brutos_Mensuales")])
clientes_subset2$Total_Egresos_Mensuales<-as.numeric(clientes_subset2$Total_Egresos_Mensuales)
class(clientes_subset2$Total_Egresos_Mensuales)
clientes_subset2$Ingresos_Brutos_Mensuales<-as.numeric(clientes_subset2$Ingresos_Brutos_Mensuales)

quantile(clientes_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(clientes_subset2$Ingresos_Brutos_Mensuales, probs = seq(0, 1, by= 0.05)) 

#Conclusion esta base no sirve

