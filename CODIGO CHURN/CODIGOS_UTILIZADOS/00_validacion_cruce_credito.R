#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist")
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

lista_mora<-list.files("./MORA_churn",pattern="mora_")
super_mora=list() 
for(aux in lista_mora){
  mora<-read.csv(gsub(" ", "",paste("mora_churn/",aux)), header = T,sep=";")
  mora$fecha<-paste(aux)
  mora$VLR_CUOTA<-"SIN DATO"
  super_mora<-rbind(super_mora,mora)
  print(aux)
  }

#save(CLIENTES, buro_201612, buro_201703, buro_201706, buro_201709,buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

results_prepagos_mora=list() 
#PREPAGOS<-read.table(file="PLEXUS/PLEXUS_PREPAGOS_ENC", header = TRUE, sep = ";")

for(aux in lista_mora){
  PREPAGOS2<-merge(x=PREPAGOS, y=super_mora, by.x="OBLIGACIONENC", by.y="Contrato", all.x=TRUE, .y=c("Dias.de.mora") )
  aux2<-PREPAGOS2 %>% freq(Dias.de.mora)
  results_prepagos_mora<- list.append(results_prepagos_mora,aux2)
  print(aux)
  }

capture.output(print(results_prepagos_mora), file=paste("SALIDA/results_prepagos_mora_",format(today, format="%Y%m%d"),".txt", sep=""))
#COLOCACION<-read.table(file="PLEXUS/PLEXUS_COLOCACION_ENC", header = TRUE,  sep = ";")
#save(CLIENTES,PREPAGOS, COLOCACION, buro_201612, buro_201703, buro_201706, buro_201709, buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

results_colocacion_mora=list() 
for(aux in lista_mora){
  COLOCACION2<-merge(x=COLOCACION, y=super_mora, by.x="OBLIGACIONENC", by.y="Contrato", all.x=TRUE, .y=c("Dias.de.mora") )
  aux2<-COLOCACION2 %>% freq(Dias.de.mora)
  results_colocacion_mora<- list.append(results_colocacion_mora,aux2)
  print(aux)
}

capture.output(print(results_colocacion_mora), file=paste("SALIDA/results_colocacion_mora_",format(today, format="%Y%m%d"),".txt", sep=""))


#CAPTACION<-read.table(file="PLEXUS/PLEXUS_ CAPTACION_ENC", header = TRUE,  sep = ";")
#save(CLIENTES,PREPAGOS, COLOCACION, CAPTACION, buro_201612, buro_201703, buro_201706, buro_201709, buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

results_captacion_mora=list() 

for(aux in lista_mora){
  CAPTACION2<-merge(x=CAPTACION, y=super_mora, by.x="OBLIGACIONENC", by.y="Contrato", all.x=TRUE, .y=c("Dias.de.mora") )
  aux2<-CAPTACION2 %>% freq(Dias.de.mora)
  results_captacion_mora<- list.append(results_captacion_mora,aux2)
  print(aux)
}

capture.output(print(results_captacion_mora), file=paste("SALIDA/results_captacion_mora_",format(today, format="%Y%m%d"),".txt", sep=""))


#************************ SUPER BASE ************************
load("superbase.RData")
lista_superbase<-list.files("./SUPERBASE",pattern="SUPERBASE*")
results_superbase_mora=list() 

for(j in lista_superbase){
  for(aux in lista_mora){
    SUPERBASE2<-merge(x=get(j), y=super_mora, by.x="OBLIGACIONENC", by.y="Contrato", all.x=TRUE, .y=c("Dias.de.mora") )
    aux2<-SUPERBASE2 %>% freq(Dias.de.mora)
    results_superbase_mora<- list.append(results_superbase_mora,aux2)
    print(aux)
  }
}

capture.output(print(results_superbase_mora), file=paste("SALIDA/results_superbase_mora_",format(today, format="%Y%m%d"),".txt", sep=""))

buro_201612 %>% freq(Dias.de.mora)
buro_201703 %>% freq(Dias.de.mora)

summary(buro_201612)

breaks<-quantile(buro_201612$ACIERTA_A_FINANCIERO, probs = seq(0, 1, by= 0.1)) # decile

size(cut(buro_201612$ACIERTA_A_FINANCIERO, breaks, right=FALSE))

###BASURA QUE PUEDE SER UTIL

options(encoding="utf-8")
#list.files("./PLEXUS")
df_utf8 <- read.table(file="E:/Jennyfer_C/04_CHURN/BBDD/PLEXUS/PLEXUS_ CAPTACION_ENC",
                      sep=";",
                      header=TRUE, 
                      encoding="UTF-8", 
                      stringsAsFactors=FALSE ,
                      dec="."
)

read.table("test.utf8",sep=",",header=TRUE,encoding="utf-8")

for(aux in lista_mora){
  CAPTACION2<-merge(x=CAPTACION, y=gsub(" ", "",paste("buro/buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-CAPTACION2 %>% freq(Dias.de.mora)
  results_captacion_mora<- list.append(results_captacion_mora,aux2)
}

results_colocacion_mora=list() 
COLOCACION<-read.table(file="PLEXUS/PLEXUS_COLOCACION_ENC", header = TRUE, sep = ";", dec = ".")

for(aux in lista_mora){
  COLOCACION2<-merge(x=COLOCACION, y=gsub(" ", "",paste("buro/buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-COLOCACION2 %>% freq(Dias.de.mora)
  results_colocacion_mora<- list.append(results_colocacion_mora,aux2)
}



#Renombrar archivos
lista_ini<-list.files("./SUPERBASE",pattern="SUPERBASE*")
lista_fin<-gsub(" ", "_",list.files("./SUPERBASE",pattern="SUPERBASE*"))

largo<-length(lista_ini)

for (i in 1:largo){
  file.rename(from=lista_ini[i], to=lista_fin[i])
}


#Carga informacion buro
#lista_mora<-list("201612","201703","201706","20170509","201709","201712","201803")
lista_mora<-list("201703","201706","201709","201712","201803")
aux<-"201612"
consolidado<-read.csv(gsub(" ", "",paste("buro/buro_",aux,".csv")), header = T,sep=";")
consolidado$fecha<-paste(aux)
#write.table(names(consolidado), paste("buro/names_",format(today, format="%Y%m%d"),".txt", sep=""), sep="\t")

names(consolidado)<-c("ID_CLIENTE","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_EXPEDICION","NUM_OBLIG_ACTIV","NUM_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUM_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUM_CREDITOS_OCF","VALOR_INICIAL_OCF","VALOR_SALDO_OCF","VALOR_CUOTAS_OCF","VALOR_MORA_OCF","NUM_TDC","VALOR_CUPOS_TDC","VALOR_UTILIZADO_TDC","PORC_UTILIZACION_TDC","VALOR_CUOTAS_TDC","VALOR_MORA_TDC","FECHA_MAS_ANTIGUA_TDC","NUM_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUM_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUM_CREDITOS_COOP","VALOR_INICIAL_COOP","VALOR_SALDO_COOP","VALOR_CUOTAS_COOP","VALOR_MORA_COOP","NUM_CREDITOS_COD","VALOR_SALDO_COD","VALOR_CUOTAS_COD","VALOR_MORA_COD","NUM_OBLIG_AL_DIA","OBLIG_MORA_30","OBLIG_MORA_60","OBLIG_MORA_90","OBLIG_MORA_120","CARTERA_CASTIGADA","DUDOSO_RECAUDO","CUENTAS_EN_COBRADOR","NUM_MORAS_30_365","NUM_MORAS_60_365","NUM_MORAS_90_365","NUM_MORAS_120_365","NUM_CANCELADAS_MAL_MANEJO_365","NUM_CARTERA_RECUPERADA_365","ALTURA_MAX_MORA_TDC","ALTURA_MAX_MORA_CB","ALTURA_MAX_MORA_COOP","ALTURA_MAX_MORA_CV","PEOR_CALIFICACION_T1","PEOR_CALIFICACION_T2","CUENTAS_DE_AHORRO_ACTIVAS","CUENTAS_CORRIENTES_ACTIVAS","CUENTAS_EMBARGADAS","CANCELADAS_MAL_MANEJO","CUENTAS_SALDADAS","TOTAL_CONSULTAS_ULTIMOS_6_MESES","ESTADO_CONSULTA","ACIERTA_A_FINANCIERO","QUANTO3","fecha")

for(aux in lista_mora){
  nombre2<-read.csv(gsub(" ", "",paste("buro/buro_",aux,".csv")), header = T,sep=";")
  nombre2$fecha<-paste(aux)
  names(nombre2)<-c("ID_CLIENTE","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_EXPEDICION","NUM_OBLIG_ACTIV","NUM_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUM_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUM_CREDITOS_OCF","VALOR_INICIAL_OCF","VALOR_SALDO_OCF","VALOR_CUOTAS_OCF","VALOR_MORA_OCF","NUM_TDC","VALOR_CUPOS_TDC","VALOR_UTILIZADO_TDC","PORC_UTILIZACION_TDC","VALOR_CUOTAS_TDC","VALOR_MORA_TDC","FECHA_MAS_ANTIGUA_TDC","NUM_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUM_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUM_CREDITOS_COOP","VALOR_INICIAL_COOP","VALOR_SALDO_COOP","VALOR_CUOTAS_COOP","VALOR_MORA_COOP","NUM_CREDITOS_COD","VALOR_SALDO_COD","VALOR_CUOTAS_COD","VALOR_MORA_COD","NUM_OBLIG_AL_DIA","OBLIG_MORA_30","OBLIG_MORA_60","OBLIG_MORA_90","OBLIG_MORA_120","CARTERA_CASTIGADA","DUDOSO_RECAUDO","CUENTAS_EN_COBRADOR","NUM_MORAS_30_365","NUM_MORAS_60_365","NUM_MORAS_90_365","NUM_MORAS_120_365","NUM_CANCELADAS_MAL_MANEJO_365","NUM_CARTERA_RECUPERADA_365","ALTURA_MAX_MORA_TDC","ALTURA_MAX_MORA_CB","ALTURA_MAX_MORA_COOP","ALTURA_MAX_MORA_CV","PEOR_CALIFICACION_T1","PEOR_CALIFICACION_T2","CUENTAS_DE_AHORRO_ACTIVAS","CUENTAS_CORRIENTES_ACTIVAS","CUENTAS_EMBARGADAS","CANCELADAS_MAL_MANEJO","CUENTAS_SALDADAS","TOTAL_CONSULTAS_ULTIMOS_6_MESES","ESTADO_CONSULTA","ACIERTA_A_FINANCIERO","QUANTO3","fecha")
  consolidado<-rbind(consolidado,nombre2)
  print(aux)
}
