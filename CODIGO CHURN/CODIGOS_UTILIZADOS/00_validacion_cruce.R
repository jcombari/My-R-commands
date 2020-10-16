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
CLIENTES<-read.table(file="CLIENTES/BO-INFORMACION DE CLIENTES ENC.txt", header = T,sep=";")

#save(CLIENTES, file = "data.RData")
#load("data.RData")

#list.files("./BURO")
results_clientes_buro=list() 
lista_buro<-list("201612","201703","201706","201709","201712","201803")

for(aux in lista_buro){
  buro<-read.csv(gsub(" ", "",paste("buro/buro_",aux,".csv")), header = T,sep=";")
  buro2<-buro[!(duplicated(buro) | duplicated(buro, fromLast = TRUE)), ]
  names(buro2)<-c("ID_CLIENTE","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_EXPEDICION","NUM_OBLIG_ACTIV","NUM_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUM_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUM_CREDITOS_OCF","VALOR_INICIAL_OCF","VALOR_SALDO_OCF","VALOR_CUOTAS_OCF","VALOR_MORA_OCF","NUM_TDC","VALOR_CUPOS_TDC","VALOR_UTILIZADO_TDC","PORC_UTILIZACION_TDC","VALOR_CUOTAS_TDC","VALOR_MORA_TDC","FECHA_MAS_ANTIGUA_TDC","NUM_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUM_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUM_CREDITOS_COOP","VALOR_INICIAL_COOP","VALOR_SALDO_COOP","VALOR_CUOTAS_COOP","VALOR_MORA_COOP","NUM_CREDITOS_COD","VALOR_SALDO_COD","VALOR_CUOTAS_COD","VALOR_MORA_COD","NUM_OBLIG_AL_DIA","OBLIG_MORA_30","OBLIG_MORA_60","OBLIG_MORA_90","OBLIG_MORA_120","CARTERA_CASTIGADA","DUDOSO_RECAUDO","CUENTAS_EN_COBRADOR","NUM_MORAS_30_365","NUM_MORAS_60_365","NUM_MORAS_90_365","NUM_MORAS_120_365","NUM_CANCELADAS_MAL_MANEJO_365","NUM_CARTERA_RECUPERADA_365","ALTURA_MAX_MORA_TDC","ALTURA_MAX_MORA_CB","ALTURA_MAX_MORA_COOP","ALTURA_MAX_MORA_CV","PEOR_CALIFICACION_T1","PEOR_CALIFICACION_T2","CUENTAS_DE_AHORRO_ACTIVAS","CUENTAS_CORRIENTES_ACTIVAS","CUENTAS_EMBARGADAS","CANCELADAS_MAL_MANEJO","CUENTAS_SALDADAS","TOTAL_CONSULTAS_ULTIMOS_6_MESES","ESTADO_CONSULTA","ACIERTA_A_FINANCIERO","QUANTO3")
  assign(paste("buro_", aux, sep = ""), buro2)  
  CLIENTES2<-merge(x=CLIENTES, y=buro2, by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-CLIENTES2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_clientes_buro<- list.append(results_clientes_buro,aux2)
}

#save(CLIENTES, buro_201612, buro_201703, buro_201706, buro_201709,buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

capture.output(print(results_clientes_buro), file=paste("SALIDA/results_clientes_buro_",format(today, format="%Y%m%d"),".txt", sep=""))

results_prepagos_buro=list() 

PREPAGOS<-read.table(file="PLEXUS/PLEXUS_PREPAGOS_ENC", header = TRUE, sep = ";")

for(aux in lista_buro){
  PREPAGOS2<-merge(x=PREPAGOS, y=get(paste0("buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-PREPAGOS2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_prepagos_buro<- list.append(results_prepagos_buro,aux2)
  print(aux)
  }

capture.output(print(results_prepagos_buro), file=paste("SALIDA/results_prepagos_buro_",format(today, format="%Y%m%d"),".txt", sep=""))


COLOCACION<-read.table(file="PLEXUS/PLEXUS_COLOCACION_ENC", header = TRUE,  sep = ";")
#save(CLIENTES,PREPAGOS, COLOCACION, buro_201612, buro_201703, buro_201706, buro_201709, buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

results_colocacion_buro=list() 
for(aux in lista_buro){
  COLOCACION2<-merge(x=COLOCACION, y=get(paste0("buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-COLOCACION2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_colocacion_buro<- list.append(results_colocacion_buro,aux2)
  print(aux)
}

capture.output(print(results_colocacion_buro), file=paste("SALIDA/results_colocacion_buro_",format(today, format="%Y%m%d"),".txt", sep=""))


CAPTACION<-read.table(file="PLEXUS/PLEXUS_ CAPTACION_ENC", header = TRUE,  sep = ";")
#save(CLIENTES,PREPAGOS, COLOCACION, CAPTACION, buro_201612, buro_201703, buro_201706, buro_201709, buro_201712, buro_201803,  file = "data.RData")
#load("data.RData")

results_captacion_buro=list() 

for(aux in lista_buro){
  CAPTACION2<-merge(x=CAPTACION, y=get(paste0("buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-CAPTACION2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_captacion_buro<- list.append(results_captacion_buro,aux2)
  print(aux)
}

capture.output(print(results_captacion_buro), file=paste("SALIDA/results_captacion_buro_",format(today, format="%Y%m%d"),".txt", sep=""))


#************************ SUPER BASE ************************
setwd(paste0(ruta,"/superbase"))
old_names<- list.files()
new_names<-gsub(" ", "_",file_names)
file.rename(from=old_names, to=new_names) 
list.files() #to check

for(aux in new_names){
  faux<-read.csv(gsub(" ", "",paste("SUPERBASE/",aux)), header = T,sep=";")
  assign(paste(aux), faux)  
} 

save( "SUPERBASE_ENE17_ENC" ,"SUPERBASE_ENE18_ENC" ,"SUPERBASE_FEB17_ENC" ,"SUPERBASE_FEB18_ENC",
      "SUPERBASE_JUL17_ENC" ,"SUPERBASE_JUL18_ENC" ,"SUPERBASE_JUN17_ENC" ,"SUPERBASE_JUN18_ENC",
      "SUPERBASE_MAR17_ENC" ,"SUPERBASE_MAR18_ENC" ,"SUPERBASE_MAY17_ENC" ,"SUPERBASE_MAY18_ENC",
      "SUPERBASE_NOV16_ENC" ,"SUPERBASE_NOV17_ENC" ,"SUPERBASE_OCT16_ENC" ,"SUPERBASE_OCT17_ENC",
      "SUPERBASE_SEP17_ENC" ,"SUPERBASE_ABR17_ENC" ,"SUPERBASE_ABR18_ENC" ,"SUPERBASE_AGO17_ENC",
      "SUPERBASE_AGO18_ENC" ,"SUPERBASE_DIC16_ENC" ,"SUPERBASE_DIC17_ENC",  file = "superbase.RData")

results_superbase_buro=list() 

for(j in new_names){
  for(aux in lista_buro){
    SUPERBASE2<-merge(x=get(j), y=get(paste0("buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
    aux2<-SUPERBASE2 %>% freq(RANGO_APROXIMADO_EDAD )
    results_superbase_buro<- list.append(results_superbase_buro,aux2)
    print(aux)
  }
}

capture.output(print(results_superbase_buro), file=paste("SALIDA/results_superbase_buro_",format(today, format="%Y%m%d"),".txt", sep=""))

buro_201612 %>% freq(RANGO_APROXIMADO_EDAD )
buro_201703 %>% freq(RANGO_APROXIMADO_EDAD )

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

for(aux in lista_buro){
  CAPTACION2<-merge(x=CAPTACION, y=gsub(" ", "",paste("buro/buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-CAPTACION2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_captacion_buro<- list.append(results_captacion_buro,aux2)
}

results_colocacion_buro=list() 
COLOCACION<-read.table(file="PLEXUS/PLEXUS_COLOCACION_ENC", header = TRUE, sep = ";", dec = ".")

for(aux in lista_buro){
  COLOCACION2<-merge(x=COLOCACION, y=gsub(" ", "",paste("buro/buro_",aux)), by.x="CEDULAENC", by.y="ID_CLIENTE", all.x=TRUE, .y=c("RANGO_APROXIMADO_EDAD") )
  aux2<-COLOCACION2 %>% freq(RANGO_APROXIMADO_EDAD )
  results_colocacion_buro<- list.append(results_colocacion_buro,aux2)
}



#Renombrar archivos
lista_ini<-list.files("./SUPERBASE",pattern="SUPERBASE*")
lista_fin<-gsub(" ", "_",list.files("./SUPERBASE",pattern="SUPERBASE*"))

largo<-length(lista_ini)

for (i in 1:largo){
  file.rename(from=lista_ini[i], to=lista_fin[i])
}



#Carga informacion buro
#lista_buro<-list("201612","201703","201706","20170509","201709","201712","201803")
lista_buro<-list("201703","201706","201709","201712","201803")
aux<-"201612"
consolidado<-read.csv(gsub(" ", "",paste("buro/buro_",aux,".csv")), header = T,sep=";")
consolidado$fecha<-paste(aux)
#write.table(names(consolidado), paste("buro/names_",format(today, format="%Y%m%d"),".txt", sep=""), sep="\t")

names(consolidado)<-c("ID_CLIENTE","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_EXPEDICION","NUM_OBLIG_ACTIV","NUM_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUM_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUM_CREDITOS_OCF","VALOR_INICIAL_OCF","VALOR_SALDO_OCF","VALOR_CUOTAS_OCF","VALOR_MORA_OCF","NUM_TDC","VALOR_CUPOS_TDC","VALOR_UTILIZADO_TDC","PORC_UTILIZACION_TDC","VALOR_CUOTAS_TDC","VALOR_MORA_TDC","FECHA_MAS_ANTIGUA_TDC","NUM_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUM_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUM_CREDITOS_COOP","VALOR_INICIAL_COOP","VALOR_SALDO_COOP","VALOR_CUOTAS_COOP","VALOR_MORA_COOP","NUM_CREDITOS_COD","VALOR_SALDO_COD","VALOR_CUOTAS_COD","VALOR_MORA_COD","NUM_OBLIG_AL_DIA","OBLIG_MORA_30","OBLIG_MORA_60","OBLIG_MORA_90","OBLIG_MORA_120","CARTERA_CASTIGADA","DUDOSO_RECAUDO","CUENTAS_EN_COBRADOR","NUM_MORAS_30_365","NUM_MORAS_60_365","NUM_MORAS_90_365","NUM_MORAS_120_365","NUM_CANCELADAS_MAL_MANEJO_365","NUM_CARTERA_RECUPERADA_365","ALTURA_MAX_MORA_TDC","ALTURA_MAX_MORA_CB","ALTURA_MAX_MORA_COOP","ALTURA_MAX_MORA_CV","PEOR_CALIFICACION_T1","PEOR_CALIFICACION_T2","CUENTAS_DE_AHORRO_ACTIVAS","CUENTAS_CORRIENTES_ACTIVAS","CUENTAS_EMBARGADAS","CANCELADAS_MAL_MANEJO","CUENTAS_SALDADAS","TOTAL_CONSULTAS_ULTIMOS_6_MESES","ESTADO_CONSULTA","ACIERTA_A_FINANCIERO","QUANTO3","fecha")

for(aux in lista_buro){
  nombre2<-read.csv(gsub(" ", "",paste("buro/buro_",aux,".csv")), header = T,sep=";")
  nombre2$fecha<-paste(aux)
  names(nombre2)<-c("ID_CLIENTE","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_EXPEDICION","NUM_OBLIG_ACTIV","NUM_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUM_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUM_CREDITOS_OCF","VALOR_INICIAL_OCF","VALOR_SALDO_OCF","VALOR_CUOTAS_OCF","VALOR_MORA_OCF","NUM_TDC","VALOR_CUPOS_TDC","VALOR_UTILIZADO_TDC","PORC_UTILIZACION_TDC","VALOR_CUOTAS_TDC","VALOR_MORA_TDC","FECHA_MAS_ANTIGUA_TDC","NUM_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUM_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUM_CREDITOS_COOP","VALOR_INICIAL_COOP","VALOR_SALDO_COOP","VALOR_CUOTAS_COOP","VALOR_MORA_COOP","NUM_CREDITOS_COD","VALOR_SALDO_COD","VALOR_CUOTAS_COD","VALOR_MORA_COD","NUM_OBLIG_AL_DIA","OBLIG_MORA_30","OBLIG_MORA_60","OBLIG_MORA_90","OBLIG_MORA_120","CARTERA_CASTIGADA","DUDOSO_RECAUDO","CUENTAS_EN_COBRADOR","NUM_MORAS_30_365","NUM_MORAS_60_365","NUM_MORAS_90_365","NUM_MORAS_120_365","NUM_CANCELADAS_MAL_MANEJO_365","NUM_CARTERA_RECUPERADA_365","ALTURA_MAX_MORA_TDC","ALTURA_MAX_MORA_CB","ALTURA_MAX_MORA_COOP","ALTURA_MAX_MORA_CV","PEOR_CALIFICACION_T1","PEOR_CALIFICACION_T2","CUENTAS_DE_AHORRO_ACTIVAS","CUENTAS_CORRIENTES_ACTIVAS","CUENTAS_EMBARGADAS","CANCELADAS_MAL_MANEJO","CUENTAS_SALDADAS","TOTAL_CONSULTAS_ULTIMOS_6_MESES","ESTADO_CONSULTA","ACIERTA_A_FINANCIERO","QUANTO3","fecha")
  consolidado<-rbind(consolidado,nombre2)
  print(aux)
}
