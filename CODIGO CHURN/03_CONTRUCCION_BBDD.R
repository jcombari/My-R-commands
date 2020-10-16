#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cargamos la información de Buro
buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")

names(buro)<-gsub(" ", "",names(buro))
write.xlsx(unlist(names(buro)),file=paste0("SALIDA/buro_nombre_variables_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Seleccionamos solo las variables que se pueden utilizar
buro<-buro[, c("TIPO_ID","RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_DE_EXPEDICION","ACIERTA_A_FINANCIERO","QUANTO","NUMERO_OBLIGACIONES_ACTIVAS","NUMERO_CREDITOS_CB","VALOR_INICIAL_CB","VALOR_SALDO_CB","VALOR_CUOTAS_CB","VALOR_MORA_CB","NUMERO_CREDITOS_CV","VALOR_INICIAL_CV","VALOR_SALDO_CV","VALOR_CUOTAS_CV","VALOR_MORA_CV","NUMERO_CREDITOS_CF","VALOR_INICIAL_CF","VALOR_SALDO_CF","VALOR_CUOTAS_CF","VALOR_MORA_CF","NUMERO_TDC","VALOR_CUPOS","VALOR_UTILIZADO","PORCENTAJE_UTILIZACION","VALOR_CUOTAS","VALOR_MORA","FECHA_MAS_ANTIGUA_APERTURA","NUMERO_CREDITOS_SR","VALOR_INICIAL_SR","VALOR_SALDO_SR","VALOR_CUOTAS_SR","VALOR_MORA_SR","NUMERO_CELULARES_TELCOS","VALOR_CUOTAS_CELULARES_TELCOS","VALOR_MORA_TELCOS","NUMERO_CREDITOS_COOPERATIVAS","VALOR_INICIAL_COOPERATIVAS","VALOR_SALDO_COOPERATIVAS","VALOR_CUOTAS_COOPERATIVAS","VALOR_MORA_COOPERATIVAS","NUMERO_CREDITOS_CODEUDORES","VALOR_SALDO_CODEUDORES","VALOR_CUOTAS_CODEUDORES","VALOR_MORA_CODEUDORES","OBLIGA_AL_DIA_CARTERA_ACTUAL","OBLIGA_MORA_30_CARTERA_ACTUAL","OBLIGA_MORA_60_CARTERA_ACTUAL","OBLIGA_MORA_90_CARTERA_ACTUAL","OBLIGA_MORA_120_CARTERA_ACTUAL","CARTERA_CASTIG_CARTERA_ACTUAL","DUDOSO_RECAUDO_CARTERA_ACTUAL","CTAS_EN_COBRADOR_CARTERA_ACT","ULT_AÑO_MORAS_30_CARTERA_HIST","ULT_AÑO_MORAS_60_CARTERA_HIST","ULT_AÑO_MORAS_90_CARTERA_HIST","ULT_AÑO_MORAS_120_CARTERA_HIST","CANCEL_MAL_MANEJO_CARTERA_HIST","CARTERA_RECUPE_CARTERA_HIST","TDC_ALTURA_MAXIMA_DE_MORA","CARTERA_BANCA_ALT_MAX_DE_MORA","CARTERA_COOPE_ALT_MAX_DE_MORA","CARTERA_HIPOTE_ALT_MAX_DE_MORA","PEOR_CALIFI_TRIM_1_ENDEUD","PEOR_CALIF_TRIM_2_ENDEUD","CTAS_DE_AHORRO_ACT_CTAS_BANCA","CTAS_CTES_ACT_CTA_BANCA","CTAS_EMBARGADAS_CTAS_BANCARIAS","CANCEL_MAL_MANEJO_CTAS_BANCA","CTAS_SALDADAS_CTAS_BANCA","TOTAL_CONSULTAS_ULT_6_MESES","ENDEUDAMIENTO","NUM_TDC_VIGENTES_SIN_POPULAR","CUPO_SIN_POPULAR","MAX_CUPO_TDC_SIN_POPULAR","PROMEDIO_CUPO_TDC_SIN_POPULAR","FEC_MAS_ANTI_APER_TDC_SIN_POPU","VALOR_UTILIZADO_SIN_POPULAR","UTILIZACION_SIN_POPULAR","VALOR_CUOTAS_SIN_POPULAR","VALOR_EN_MORA_SIN_POPULAR","NUMOBLVIGENSECTORBANCASIN_POPU","CUPOSECTORBANCARIO_SIN_POPULAR","MAXCUPOSECTORBANCASIN_POPULAR","PROMCUPOSECTORBANCASIN_POPULAR","FECANTIAPERSECTORBANCASINPOPU","VALOR_UTILISECTORBANCASIN_POPU","UTIL_SECTOR_BANCA_SIN_POPULAR","VAL_CUO_SECTOR_BANCA_SIN_POPU","VAL_MORASECTORBANCASIN_POPULAR","NUMOBLVIGENSECTOR_HIP_SIN_POPU","CUPO_SECTOR_HIP_SIN_POPULAR","MAX_CUPO_SECTOR_HIP_SIN_POPU","PROM_CUPO_SECTOR_HIP_SIN_POPU","FEC_MASANTAPERSECTORHIPSIN_POP","VAL_UTIL_SECTOR_HIP_SIN_POP","UTIL_SECTOR_HIP_SIN_POPULAR","VAL_CUOTAS_SECTOR_HIP_SIN_POPU","VAL_MORA_SECTOR_HIP_SIN_POPU","FECHA_ENVIO","FECHA_DATA","CEDULAENC")]
write.table(summary.default(buro),file=paste("SALIDA/buro_descn_variables_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Arreglamos fecha en el archivo buro
freq(buro$FECHA_DATA)
buro$FECHA<-as.numeric(paste0(str_sub(buro$FECHA_DATA,1,4),str_sub(buro$FECHA_DATA,6,7)))
buro$FECHA_ENVIO2<-as.numeric(paste0(str_sub(buro$FECHA_ENVIO,1,4),str_sub(buro$FECHA_ENVIO,6,7)))
freq(buro$FECHA) 
freq(buro$FECHA_ENVIO2)

#Para grabar
#save(buro, file = "buro.RData")

#Cargamos la población objetivo
load(file="buro.Rdata")
load(file="POBLACION_OBJETIVO.Rdata")

buro$clave<-paste0(buro$CEDULAENC,buro$FECHA)
buro<-buro[order(buro$CEDULAENC, -buro$FECHA),] #ordeno por clave=cedula+fecha
buro<-buro[!duplicated(buro$clave), ] #luego de ordenamos, elimino dupicados

names(OBJETIVO)<-gsub("\\.super", "",names(OBJETIVO))

#aux<-data.frame(unclass(summary.default(buro)), check.names = FALSE, stringsAsFactors = FALSE)
#save(buro, file="buro.Rdata")
#load(file="buro.Rdata")

#Importamos fecha para saber el cruce
fecha_buro<-read.csv(gsub(" ", "",paste("Buro/","buro_fecha.csv")), header = T,sep=";")
OBJETIVO<-merge(x=OBJETIVO, y=fecha_buro, by.x="fecha", by.y="fecha", x.all=TRUE)
OBJETIVO$fecha_buro<-as.numeric(OBJETIVO$fecha_buro)
#table(OBJETIVO$fecha,OBJETIVO$fecha_buro, useNA="ifany")
OBJETIVO$clave<-paste0(OBJETIVO$CEDULAENC, OBJETIVO$fecha_buro)

#Cruce de la poblacion objetivo con la base de datos de buro
#Creamos clave en la población objetivo
#memory.limit(size=65000)
#OBJETIVO_BURO<-transform(merge(
#  x=cbind(OBJETIVO,source="x"),
#  y=cbind(buro,source="y"),
#  by.x="clave", by.y="clave",all.x=TRUE),
#  source=ifelse(!is.na(source.x) & !is.na(source.y), "both", 
#                ifelse(!is.na(source.x), "x", "y")),
#  source.x=NULL,
#  source.y=NULL
#)

names(OBJETIVO)<-paste0(names(OBJETIVO),"_x")
names(buro)<-paste0(names(buro),"_y")

OBJETIVO_BURO<-merge(
  x=OBJETIVO,
  y=buro,
  by.x="clave_x", by.y="clave_y",all.x=TRUE)

names(OBJETIVO_BURO)<-gsub(pattern="_x", "_objetivo",names(OBJETIVO_BURO))
names(OBJETIVO_BURO)<-gsub(pattern="_y", "_buro",names(OBJETIVO_BURO))

#Grabamos el archivo

save(OBJETIVO_BURO,file="objetivo_buro.Rdata")

#Cargamos el archivo
load(file="objetivo_buro.Rdata")

BBDD<-OBJETIVO_BURO

BBDD$VLR_DESEMB_objetivo_tram<-ifelse(BBDD$VLR_DESEMB_objetivo<=17000000,"01_[ 0 ; 17000000]",ifelse(BBDD$VLR_DESEMB_objetivo<=29100000,"02_( 17,000,000 ; 29,100,000]","03_>29,100,000"))
BBDD$SAL_CAPITA_objetivo_tram<-ifelse(BBDD$SAL_CAPITA_objetivo<=16585327,"01_[ 0 ; 16,585,327]","02_>16,585,327")
BBDD$VLR_CUOTA_objetivo_tram<-ifelse(BBDD$VLR_CUOTA_objetivo<=510486,"[ 0 ; 510,486]",">510,486")
BBDD$VLR_PAGO_objetivo_tram<-ifelse(is.na(BBDD$VLR_PAGO_objetivo),"SIN_DATO",ifelse(BBDD$VLR_PAGO_objetivo<=11548993,"[ 0 ; 11,548,993]",">11,548,993"))
BBDD$CUOTA_PAGA2_objetivo_tram<-ifelse(is.na(BBDD$CUOTA_PAGA2_objetivo),">24",ifelse(BBDD$CUOTA_PAGA2_objetivo<=24,"[ 0 ; 24]",">24"))
BBDD$CUOTA_PENDIENTE_objetivo_tram<-ifelse(is.na(BBDD$CUOTA_PENDIENTE_objetivo),">55",ifelse(BBDD$CUOTA_PENDIENTE_objetivo<=55,"[ 0 ; 55]",">55"))
BBDD$ACIERTA_A_FINANCIERO_buro_tram<-ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=699 |is.na(BBDD$ACIERTA_A_FINANCIERO_buro),"01_[ 0 ; 699]",ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=829,"02_( 699 ; 829]","03_>829"))
BBDD$QUANTO_buro_tram<-ifelse(is.na(BBDD$QUANTO_buro),"03_>5,177,000",ifelse(BBDD$QUANTO_buro<=3517000,"01_[ 0 ; 3,517,000]",ifelse(BBDD$QUANTO_buro<=5177000,"02_( 3,517,000 ; 5,177,000]","03_>5,177,000")))
BBDD$VALOR_INICIAL_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CB_buro),"03_>44,000,000",ifelse(BBDD$VALOR_INICIAL_CB_buro<=21300000,"01_[ 0 ; 21,300,000]",ifelse(BBDD$VALOR_INICIAL_CB_buro<=44000000,"02_( 21,300,000 ; 44,000,000]","03_>44,000,000")))
BBDD$VALOR_SALDO_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CB_buro),"03_>35,167,000",ifelse(BBDD$VALOR_SALDO_CB_buro<=14559000,"01_[ 0 ; 14,559,000]",ifelse(BBDD$VALOR_SALDO_CB_buro<=35167000,"02_( 14,559,000 ; 35,167,000]","03_>35,167,000")))
BBDD$VALOR_CUOTAS_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CB_buro),"03_>866,000",ifelse(BBDD$VALOR_CUOTAS_CB_buro<=460000,"01_[ 0 ; 460,000]",ifelse(BBDD$VALOR_CUOTAS_CB_buro<=866000,"02_( 460,000 ; 866,000]","03_>866,000")))
BBDD$VALOR_MORA_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_CB_buro),"0",ifelse(BBDD$VALOR_MORA_CB_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CV_buro),"0",ifelse(BBDD$VALOR_INICIAL_CV_buro==0,"0",">0"))
BBDD$VALOR_SALDO_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CV_buro),"0",ifelse(BBDD$VALOR_SALDO_CV_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CV_buro),"0",ifelse(BBDD$VALOR_CUOTAS_CV_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CF_buro),">2,400,000",ifelse(BBDD$VALOR_INICIAL_CF_buro<=2400000,"[ 0 ; 2,400,000]",">2,400,000"))
BBDD$VALOR_SALDO_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CF_buro),">748,000",ifelse(BBDD$VALOR_SALDO_CF_buro<=748000,"[ 0 ; 748,000]",">748,000"))
BBDD$VALOR_CUOTAS_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CF_buro),">43,000",ifelse(BBDD$VALOR_CUOTAS_CF_buro<=43000,"[ 0 ; 43,000]",">43,000"))
BBDD$VALOR_CUPOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUPOS_buro),">6,290,000",ifelse(BBDD$VALOR_CUPOS_buro<=500000,"01_[ 0 ; 500,000]",ifelse(BBDD$VALOR_CUPOS_buro<=6290000,"( 500,000 ; 6,290,000]",">6,290,000")))
BBDD$VALOR_UTILIZADO_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILIZADO_buro),">273,000",ifelse(BBDD$VALOR_UTILIZADO_buro<=273000,"[ 0 ; 273,000]",">273,000"))
BBDD$PORCENTAJE_UTILIZACION_buro_tram<-ifelse(is.na(BBDD$PORCENTAJE_UTILIZACION_buro),"[ 0 ; 0.07]",ifelse(BBDD$PORCENTAJE_UTILIZACION_buro<=0.07,"[ 0 ; 0.07]",">0.07"))
BBDD$VALOR_CUOTAS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_buro),"[ 0 ; 15,000]",ifelse(BBDD$VALOR_CUOTAS_buro<=15000,"[ 0 ; 15,000]",">15,000"))
BBDD$VALOR_MORA_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_buro),"0",ifelse(BBDD$VALOR_MORA_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_SR_buro),"0",ifelse(BBDD$VALOR_INICIAL_SR_buro==0,"0",">0"))
BBDD$VALOR_SALDO_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_SR_buro),"0",ifelse(BBDD$VALOR_SALDO_SR_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_SR_buro),"0",ifelse(BBDD$VALOR_CUOTAS_SR_buro==0,"0",">0"))
BBDD$VALOR_MORA_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_SR_buro),"0",ifelse(BBDD$VALOR_MORA_SR_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro),"0",ifelse(BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro==0,"0",">0"))
BBDD$VALOR_MORA_TELCOS_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_TELCOS_buro),"0",ifelse(BBDD$VALOR_MORA_TELCOS_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_INICIAL_COOPERATIVAS_buro==0,"0",">0"))
BBDD$VALOR_SALDO_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_SALDO_COOPERATIVAS_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_CUOTAS_COOPERATIVAS_buro==0,"0",">0"))
BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro),">9",ifelse(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro<=4,"[ 0 ; 4]",ifelse(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro<=9,"( 4 ; 9]",">9")))
BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro==0,"0",">0"))
BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram<-ifelse(is.na(BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro),"0",ifelse(BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro==0,"0",">0"))
BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro),">1",ifelse(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro==0,"0",ifelse(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro==1,"1","3_>1"))) 
BBDD$CTAS_CTES_ACT_CTA_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_CTES_ACT_CTA_BANCA_buro),"0",ifelse(BBDD$CTAS_CTES_ACT_CTA_BANCA_buro==0,"0",">0"))
BBDD$CTAS_SALDADAS_CTAS_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_SALDADAS_CTAS_BANCA_buro),"0",ifelse(BBDD$CTAS_SALDADAS_CTAS_BANCA_buro==0,"0",">0"))
BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram<-ifelse(is.na(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro),"2_>1",ifelse(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro==0,"0_0",ifelse(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro==1,"1_1","2_>1")))
BBDD$ENDEUDAMIENTO_buro_tram<-ifelse(is.na(BBDD$ENDEUDAMIENTO_buro),"03_>37",ifelse(BBDD$ENDEUDAMIENTO_buro<=14,"00_[ 0 ; 14]",ifelse(BBDD$ENDEUDAMIENTO_buro<=23,"01_(14 ; 23]",ifelse(BBDD$ENDEUDAMIENTO_buro<=37,"02_[ 23 ; 37]","03_>37"))))
BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro),">0",ifelse(BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro==0,"0",">0"))
BBDD$CUPO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$CUPO_SIN_POPULAR_buro),">50,000",ifelse(BBDD$CUPO_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro),">50,000",ifelse(BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro),">50,000",ifelse(BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro),">169,000",ifelse(BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro<=169000,"[ 0 ; 169,000]",">169,000"))
BBDD$UTILIZACION_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$UTILIZACION_SIN_POPULAR_buro),">2",ifelse(BBDD$UTILIZACION_SIN_POPULAR_buro<=2,"[ 0 ; 2]",">2"))
BBDD$VALOR_CUOTAS_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_SIN_POPULAR_buro),">2,000",ifelse(BBDD$VALOR_CUOTAS_SIN_POPULAR_buro<=2000,"[ 0 ; 2,000]",">2,000"))
BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram<-ifelse(is.na(BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro),">0",ifelse(BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro==0,"0",">0"))
BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro),"0",ifelse(BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro==0,"0",">0"))
BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro),"0",ifelse(BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro==0,"0",">0"))
BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro),"0",ifelse(BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro==0,"0",">0"))
BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro),">0",ifelse(BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro==0,"0",">0"))
BBDD$RANGO_APROXIMADO_EDAD_buro<-is.numeric(BBDD$RANGO_APROXIMADO_EDAD_buro)
BBDD$RANGO_APROXIMADO_EDAD_buro_tram<-ifelse(is.na(BBDD$RANGO_APROXIMADO_EDAD_buro),"<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=35,"<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=55, "(35 ; 55]", ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=65, "(55 ; 65]", ">65" ))))
a<-unique(BBDD$RANGO_APROXIMADO_EDAD_buro)
BBDD$RANGO_APROXIMADO_EDAD_buro_tram<-ifelse(is.na(BBDD$RANGO_APROXIMADO_EDAD_buro),"1_<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (a[1:5]),"1_<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (c('36-45', '46-55')),"2_(35 ; 55]",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (c('56-65')),"3_56-65","4_66+"))))
BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro_tram<-ifelse(is.na(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro),"3_>8",ifelse(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro<=4,"1_<=4",ifelse(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro<=8,"2_( 4 ; 8]","3_>8" )))
BBDD$maduracion<-BBDD$CUOTA_PAGA2_objetivo/(BBDD$CUOTA_PAGA2_objetivo+ BBDD$CUOTA_PENDIENTE_objetivo)
BBDD$maduracion_tram<-ifelse(is.na(BBDD$maduracion),"02_>30%",ifelse(BBDD$maduracion<=0.30,"01_<=30%","02_>30%"))

load(file="BBDD_mora.Rdata")

BBDD$clave_mora<-paste0(BBDD$OBLIGACIONENC_objetivo,"_",BBDD$fecha_objetivo)
super_bbdd_mora$clave_mora<-paste0(super_bbdd_mora$OBLIGACIONENC_objetivo,"_",super_bbdd_mora$fecha_objetivo)
super_bbdd_mora<-super_bbdd_mora[order(super_bbdd_mora$OBLIGACIONENC_objetivo -super_bbdd_mora$fecha_objetivo),] #ordeno por clave=cedula+fecha
super_bbdd_mora2<-super_bbdd_mora[!duplicated(super_bbdd_mora$clave_mora), ] #luego de ordenamos, elimino dupicados

drops <- c("fecha_objetivo","TARGET_objetivo","CEDULAENC_objetivo","OBLIGACIONENC_objetivo")
super_bbdd_mora2<-super_bbdd_mora2[ , !(names(super_bbdd_mora2) %in% drops)]

BBDD2<-merge(x=BBDD, y=super_bbdd_mora2, by.x="clave_mora", by.y = "clave_mora", all.x = TRUE)

BBDD2$mora_max_actual_tram<-ifelse(is.na(BBDD2$mora_max_actual),"01_<=6",ifelse(BBDD2$mora_max_actual<=6,"01_<=6","02_>6"))
BBDD2$mora_max_actual_tram<-ifelse(is.na(BBDD2$mora_max_actual),"01_<=6",ifelse(BBDD2$mora_max_actual<=6,"01_<=6","02_>6"))


#Información de fidelizacion
#base_novada<-read.csv(gsub(" ", "",paste("Novacion/QUINTA_ENTREGA/","base_R.csv")) , header = T,sep=";")
#base_novada<-sqldf("select * from base_novada where C20IDTERCERO<> '' ")

#old_names<-names(base_novada)
#names(base_novada)<-gsub("V.", "",old_names) 

#base_novada$MODALIDAD<-  gsub("Ó", "O",base_novada$MODALIDAD) 
#base_novada$MOD_DEF<-ifelse(is.na(base_novada$C69OBLIGACIONNODA)!=TRUE & base_novada$MODALIDAD=="FIDELIZACION", "FIDELIZACION", ifelse(is.na(base_novada$C69OBLIGACIONNODA)!=TRUE & base_novada$MODALIDAD!="FIDELIZACION" ,"NOVACION", "DESEMBOLSO" ))
#freq(base_novada$MOD_DEF)

#base_novada<-sqldf("select * from base_novada where MOD_DEF='FIDELIZACION'")
#base_novada2<-base_novada[c("C69OBLIGACIONNODA","MOD_DEF","C70FECHADESEM")]
#Pegamos la información de novación
#BBDD2<-merge(x=BBDD2, y=base_novada2, by.x="OBLIGACIONENC_objetivo", by.y = "C69OBLIGACIONNODA", all.x = TRUE)

#freq(BBDD2$MOD_DEF)
#freq(BBDD2$TARGET_objetivo, useNA="ifany")

#table(BBDD2$MOD_DEF_objetivo, BBDD2$TARGET_objetivo, useNA="ifany")

#Limpieza target 

BBDD2$TARGET_objetivo<-ifelse(BBDD2$TARGET_objetivo==1 & BBDD2$MOD_DEF_objetivo=="FIDELIZACION", 0 , BBDD2$TARGET_objetivo)

freq(BBDD2$TARGET_objetivo, useNA="ifany")

table(BBDD2$MOD_DEF_objetivo, BBDD2$TARGET_objetivo, useNA="ifany")

table(BBDD2$MOD_DEF, BBDD2$TARGET_objetivo, useNA="ifany")

BBDD2$TARGET_objetivo[is.na(BBDD2$TARGET_objetivo)] <- 1

table(BBDD2$MOD_DEF, BBDD2$TARGET_objetivo, useNA="ifany")

#Creamos MARCA DE FIDELIZACION DENTRO DE LA VENTANA

BBDD2$marca_fid_ventana<-ifelse(BBDD2$MOD_DEF=="FIDELIZACION" & as.numeric(BBDD2$FECDES_NF_objetivo)<=as.numeric(BBDD2$lista_fin_objetivo) ,1,BBDD2$TARGET_objetivo)
freq(BBDD2$marca_fid_ventana)
BBDD2$marca_fid_ventana[is.na(BBDD2$marca_fid_ventana)] <- 0
freq(BBDD2$marca_fid_ventana)

BBDD2$TARGET_objetivo2<-ifelse(BBDD2$MOD_DEF=="FIDELIZACION" & BBDD2$marca_fid_ventana==1,1,BBDD2$TARGET_objetivo)

table(BBDD2$MOD_DEF, BBDD2$TARGET_objetivo2, useNA="ifany")

aux<-freq(BBDD2$TARGET_objetivo, useNA="ifany")
tasa_churn<-(aux[2]/aux[1])*100

aux2<-freq(BBDD2$TARGET_objetivo2, useNA="ifany")
tasa_churn_fid<-(aux2[2]/aux2[1])*100


#Lista de fechas
#lista_ini<-unique(BBDD$fecha_objetivo)
#ventana_back<-2
#Para saber de esas cuantas se han ido K meses despues
#lista_fin<-list()
#largo<-length(lista_ini)
#for(i in (1:largo)){
#    lista_fin[i]<- sub("-","", format(seq(as.Date(paste0(lista_ini[i],"01"), "%Y%m%d"), length = 2, by = "+2 months"), "%Y-%m"))[2]
#}

#lista_fecha<-as.data.frame(cbind(lista_ini, lista_fin))
#BBDD2<-merge(x=BBDD2, y=lista_fecha, by.x="fecha_objetivo", by.y = "lista_ini", all.x = TRUE)

#BBDD3<- sqldf("select * from BBDD2 where CUOTA_PENDIENTE_objetivo>=6")
#freq(BBDD3$CUOTA_PENDIENTE_objetivo)

save(BBDD2, BBDD3, file = "base_objetivo_fid.RData")

