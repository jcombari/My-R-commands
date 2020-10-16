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

#load("superbase.RData")

#Renombrenado las SUPERBASES
setwd(paste0(ruta,"/SUPERBASE"))
lista_ini<-list.files(pattern="SUPERBASE*")
lista_fin<-gsub(" ", "_",list.files(pattern="SUPERBASE*"))

largo<-length(lista_ini)

for (i in 1:largo){
  file.rename(from=lista_ini[i], to=lista_fin[i])
}

#Creación de la SUPERBASE
setwd(ruta)
lista_SUPERBASE<-list.files("./SUPERBASE",pattern="SUPERBASE_")
lista_super_cedula<-list()
lista_super_contrato<-list()
super_super=list()

for(aux in lista_SUPERBASE){
  super=list()
  L = ""
  L = character(0)
  L <- readLines(gsub(" ", "",paste("SUPERBASE/",aux)), n = 1)
  if (grepl(";", L)){
    super<-read.csv(gsub(" ", "",paste("SUPERBASE/",aux)), header = T,sep=";")
  } else{
    super<-read.csv(gsub(" ", "",paste("SUPERBASE/",aux)), header = T,sep=",")
  }
  
  super$fecha<-paste(aux)
  
  #Analisis contrato
  super$largo_contrato<-str_length(super$OBLIGACIONENC)
  aux_contrato2<-sqldf("SELECT largo_contrato, count(largo_contrato) from super")
  aux_contrato2<-cbind(aux, aux_contrato2)
  
  #Analisis cedula
  super$largo_cedula<-str_length(super$CEDULAENC)
  aux_cedula2<-sqldf("SELECT largo_cedula, count(largo_cedula) from super")
  aux_cedula2<-cbind(aux, aux_cedula2)  

  lista_super_contrato<- list.append(lista_super_contrato,aux_contrato2)
  lista_super_cedula<- list.append(lista_super_cedula,aux_cedula2)
  super_super<-rbind(super_super,super)
  print(aux)
}

for(aux in lista_fin){
  faux<-read.csv(gsub(" ", "",paste("SUPERBASE/",aux)), header = T,sep=";")
  assign(paste(aux), faux)  
} 


save( "super_super", "SUPERBASE_ENE17_ENC" ,"SUPERBASE_ENE18_ENC" ,"SUPERBASE_FEB17_ENC" ,"SUPERBASE_FEB18_ENC",
      "SUPERBASE_JUL17_ENC" ,"SUPERBASE_JUL18_ENC" ,"SUPERBASE_JUN17_ENC" ,"SUPERBASE_JUN18_ENC",
      "SUPERBASE_MAR17_ENC" ,"SUPERBASE_MAR18_ENC" ,"SUPERBASE_MAY17_ENC" ,"SUPERBASE_MAY18_ENC",
      "SUPERBASE_NOV16_ENC" ,"SUPERBASE_NOV17_ENC" ,"SUPERBASE_OCT16_ENC" ,"SUPERBASE_OCT17_ENC",
      "SUPERBASE_SEP17_ENC" ,"SUPERBASE_ABR17_ENC" ,"SUPERBASE_ABR18_ENC" ,"SUPERBASE_AGO17_ENC",
      "SUPERBASE_AGO18_ENC" ,"SUPERBASE_DIC16_ENC" ,"SUPERBASE_DIC17_ENC",  file = "superbase.RData")
      
      
SUPERBASE<-super_super
des<- str(SUPERBASE)

#Exportar resumen
capture.output(print(names(SUPERBASE), na.print=''), file=paste("SALIDA/SUPERBASE_nombre_variables_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(str(SUPERBASE), na.print=''), file=paste("SALIDA/SUPERBASE_DESCRIPCION_BBDDS_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(lista_super_contrato, na.print=''), file=paste("SALIDA/SUPERBASE_lista_contrato_",format(today, format="%Y%m%d"),".txt", sep=""))
capture.output(print(lista_super_cedula, na.print=''), file=paste("SALIDA/SUPERBASE_lista_cedula_",format(today, format="%Y%m%d"),".txt", sep=""))

#Analisis de variables
#install.packages("xlsx")
library("xlsx")

#install.packages("summarytools")
#install.packages("rmarkdown")

#library(summarytools)
#library(rmarkdown)

names(super_super)
load("tabla_mes.Rdata")
super_super<-merge(x=super_super, y=tabla_mes, by.x="mes", by.y="mes1")
super_super$fecha2<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-4 )
super_super$anio<-substr(super_super$fecha, nchar(super_super$fecha)-5, nchar(super_super$fecha)-4 )
super_super$mes<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-6 )



#tabla_mes<-read.csv("tabla_mes.csv", header = T,sep=";")
#save( tabla_mes,  file = "tabla_mes.RData")
      
salida<-list()

distr_calificacion_fecha<-sqldf("select anio, fecha2,CALIF_CART, count(CALIF_CART) as num_obligaciones from super_super group by anio, mes2, CALIF_CART order by anio, mes2, CALIF_CART")
salida<-list.append(salida,distr_calificacion_fecha )

distr_obligaciones_fecha<-sqldf("select fecha2, count(fecha2) as num_obligaciones from super_super group by fecha2")
salida<-list.append(salida,distr_obligaciones_fecha)

sqldf("select COD_OFIC, count(COD_OFIC) from super_super group by COD_OFIC")

distr_COD_OFIC_fecha<-sqldf("select anio, fecha2,COD_OFIC, count(COD_OFIC) as num_obligaciones from super_super group by anio, mes2, COD_OFIC order by anio, mes2, COD_OFIC")
salida<-list.append(salida,distr_COD_OFIC_fecha)

#sqldf("select PLAZO, count(PLAZO) from super_super group by PLAZO")

distr_PLAZO_OFIC_fecha<-sqldf("select anio, fecha2,PLAZO, count(PLAZO) as num_obligaciones from super_super group by anio, mes2, PLAZO order by anio, mes2, PLAZO")
salida<-list.append(salida,distr_COD_OFIC_fecha)

distr_PLAZO_OFIC_anio<-sqldf("select anio,PLAZO, count(PLAZO) as num_obligaciones from super_super group by anio, PLAZO order by anio, PLAZO")
salida<-list.append(salida,distr_PLAZO_OFIC_anio)

distr_PLAZO_OFIC_anio<-sqldf("select anio,PLAZO, count(PLAZO) as num_obligaciones from super_super group by anio, PLAZO order by anio, PLAZO")
salida<-list.append(salida, distr_PLAZO_OFIC_anio)

library(xlsx)
write.xlsx(distr_PLAZO_OFIC_anio,"temp.xlsx")



super_super2<-super_super[,c("VLR_DESEMB", "SAL_CAPITA" , "VLR_CUOTA", "VLR_MORA" , "DIASMORA_I", "CUOTA_PAGA")]

quants <- c(0,0.05,0.25,0.50,0.75,0.90,0.95,0.99,1)

apply( super_super2, 2 , quantile , probs = quants , na.rm = TRUE, names=T )

aux<-ddply(dat, "Watershed", summarise, percentile = quantile(WQ, quants))

rbind(quants,aux)
#Resumen_fecha2<-SUPERBASE %>% freq(Fecha2)

#Numero de variables
size(names(SUPERBASE))

#Analisis cualitativo
SUPERBASE_subset <- as.data.frame(SUPERBASE[ ,c("NOM_PRODUCTO","NOM_OFICINA","FEC_APECUENTA","NOM_OFICINAH","NOM_CIUDAD","NOMBRE_REGIONAL","NOM_DEPARTAMENTO","CIUDAD_OFICINA","Fecha2","COD_OFICINAH")])

#Eliminar espaciones en blanco de todo el dataset

SUPERBASE_nombres<-names(SUPERBASE_subset)

for (aux in SUPERBASE_nombres){
  SUPERBASE_subset[,aux]<-gsub(" ", "_",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("Á", "A",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("á", "a",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("É", "E",SUPERBASE_subset[,aux]) 
  SUPERBASE_subset[,aux]<-gsub("é", "e",SUPERBASE_subset[,aux])  
  SUPERBASE_subset[,aux]<-gsub("í", "i",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("Í", "I",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("ó", "o",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("Ó", "O",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("Ú", "U",SUPERBASE_subset[,aux])
  SUPERBASE_subset[,aux]<-gsub("ú", "u",SUPERBASE_subset[,aux])
}

#
#view(dfSummary(SUPERBASE_subset))
#Para excluir

#DATOS_S <- subset(SUPERBASE_subset, select = -c(Edad, CEDULAENC))

freq_tables_SUPERBASE <- lapply(SUPERBASE_subset, freq)

capture.output(print(freq_tables_SUPERBASE), file = paste("SALIDA/SUPERBASE_DESC_VARIABLES_",format(today, format="%Y%m%d"),".txt", sep=""))

save(freq_tables_SUPERBASE , file = "frecuencia_SUPERBASE.RData")

SUPERBASE_subset %>% freq(NOM_PRODUCTO)
freq(SUPERBASE_subset$NOM_PRODUCTO, style = "rmarkdown")

gsub(" ", "",SUPERBASE_subset$TipoDocumento) 


view(freq_tables, footnote = NA, file = 'freq-tables.html')

#Analisis Cualitativo

SUPERBASE_subset2 <- as.data.frame(SUPERBASE[ ,c("FEB15","MAR15","ABR15","MAY15","JUN15","JUL15","AGO15","SEP15","OCT15","NOV15","DIC15","ENE16","FEB16","MAR16","ABR16","MAY16","JUN16","JUL16","AGO16","SEP16","OCT16","NOV16","DIC16","ENE17","FEB17","MAR17","ABR17","MAY17","JUN17","JUL17","AGO17","SEP17","OCT17","NOV17","DIC17","ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","CNT_PLAZO")])
SUPERBASE_subset2$Total_Egresos_Mensuales<-as.numeric(SUPERBASE_subset2$Total_Egresos_Mensuales)
class(SUPERBASE_subset2$Total_Egresos_Mensuales)
SUPERBASE_subset2$Ingresos_Brutos_Mensuales<-as.numeric(SUPERBASE_subset2$Ingresos_Brutos_Mensuales)

quantile(SUPERBASE_subset2$Total_Egresos_Mensuales, probs = seq(0, 1, by= 0.05)) 
quantile(SUPERBASE_subset[,aux], probs = seq(0, 1, by= 0.05)) 

results_SUPERBASE_percentil=list() 
nombre_SUPERBASE2<-names(SUPERBASE_subset2)
for(aux in nombre_SUPERBASE2){
  SUPERBASE_subset2[,aux]<-as.numeric(SUPERBASE_subset2[,aux])
  aux2<-quantile(SUPERBASE_subset2[,aux], probs = seq(0, 1, by= 0.05), na.rm= TRUE) 
  aux2<-rbind(aux,aux2)
  results_SUPERBASE_percentil<- list.append(results_SUPERBASE_percentil,aux2)
  print(aux)
}

capture.output(print(results_SUPERBASE_percentil), file=paste("SALIDA/results_SUPERBASE_percentil_",format(today, format="%Y%m%d"),".txt", sep=""))
write.table(as.data.frame(results_SUPERBASE_percentil),file=paste("SALIDA/results_SUPERBASE_percentil_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)

#Validacion numero de largo del contrato

SUPERBASE$largo_contrato<-str_length(SUPERBASE$OBLIGACIONENC)
view(freq(SUPERBASE$largo_contrato, style = "rmarkdown"))

SUPERBASE$largo_cedula<-str_length(SUPERBASE$CEDULAENC)
view(freq(SUPERBASE$largo_cedula, style = "rmarkdown"))

