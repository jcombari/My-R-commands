#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","pastecs")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cargamos la base a analizar

load(file="objetivo_buro.Rdata")
BBDD<-OBJETIVO_BURO

#Clasificamos las variables en dos tipos: cuali y cuanti

#write.table(summary.default(BBDD),file=paste("SALIDA/BBDD_descn_variables_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

discreta<-c("fecha_objetivo","TARGET_objetivo" ,"CALIF_CART_objetivo","COD_OFIC_objetivo","PLAZO_objetivo","COD_PAG_objetivo","FECDES2_objetivo","FECVEN2_objetivo","MOD_DEF_objetivo","COD_PAGADURIA_objetivo","COD_SUCURSAL_objetivo","COD_OFIADMIN_objetivo","COD_OFIPAGO_objetivo","COD_SECTOR_objetivo","COD_SUBSECTOR_objetivo","COD_OFICINA_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","NUMERO_CREDITOS_CB_buro","NUMERO_CREDITOS_CV_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","ESTADO_CONSULTA_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","FECHA_buro","FECHA_ENVIO2_buro")
continua<-c("fecha_objetivo","TARGET_objetivo", "TASINT_CTE_objetivo","TASINT_MOR_objetivo","VLR_DESEMB_objetivo","SAL_CAPITA_objetivo","VLR_CUOTA_objetivo","VLR_MORA_objetivo","DIASMORA_I_objetivo","VLR_PAGO_objetivo","VLR_SALDOOBLIG_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","ACIERTA_A_FINANCIERO_buro","QUANTO_buro","VALOR_INICIAL_CB_buro","VALOR_SALDO_CB_buro","VALOR_CUOTAS_CB_buro","VALOR_MORA_CB_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro", "PLAZO_objetivo")

borrar <- c("fecha_objetivo","TARGET_objetivo")
BBDD_continua<-BBDD[continua]
BBDD_discreta<-BBDD[discreta]


#Tramos definitivos

BBDD$VLR_DESEMB_objetivo_tram<-ifelse(VLR_DESEMB_objetivo_tram)

BBDD$VLR_DESEMB_objetivo_tram<- ifelse(BBDD$VLR_DESEMB_objetivo<=17000000,"[ 0 ; 17000000]",ifelse(BBDD$VLR_DESEMB_objetivo<=29100000,"[ 17000000 ; 29100000]",">29100000" ))
freq(BBDD$VLR_DESEMB_objetivo_tram,useNA="ifany")
BBDD$SAL_CAPITA_objetivo_tram<-ifelse(BBDD$SAL_CAPITA_objetivo<=16585327,"[ 0 ; 16585327]",">21184898")
freq(BBDD$SAL_CAPITA_objetivo_tram,useNA="ifany")
                                      
sum(is.na(BBDD$VLR_PAGO_objetivo))

BBDD$VLR_PAGO_objetivo_tram<-ifelse(is.na(BBDD$VLR_PAGO_objetivo)==TRUE,NA,ifelse(BBDD$VLR_PAGO_objetivo<=11548993,"[ 0 ; 11548993]",">11548993"))

freq(BBDD$VLR_PAGO_objetivo_tram,useNA="ifany")

BBDD$ACIERTA_A_FINANCIERO_buro_tram<-ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=699 |is.na(BBDD$ACIERTA_A_FINANCIERO_buro),"[ 0 ; 699]",ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=829,"[ 699 ; 829]",">829")) 
freq(BBDD$ACIERTA_A_FINANCIERO_buro_tram,useNA="ifany")

BBDD$QUANTO_buro_tram<-ifelse(is.na(BBDD$QUANTO_buro),">5177000",ifelse(BBDD$QUANTO_buro<=3517000,"[ 0 ; 3517000]",ifelse(BBDD$QUANTO_buro<=5177000,"[ 3517000 ; 5177000]",">5177000"))) 
freq(BBDD$QUANTO_buro_tram,useNA="ifany")



BBDD$VALOR_INICIAL_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CB_buro),">44000000",ifelse(BBDD$VALOR_INICIAL_CB_buro<=21300000,"[ 0 ; 21300000]",ifelse(BBDD$VALOR_INICIAL_CB_buro<=44000000,"[ 21300000 ; 44000000]",">44000000"))) 
freq(BBDD$VALOR_INICIAL_CB_buro_tram,useNA="ifany")

BBDD$VALOR_SALDO_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CB_buro),">35167000",ifelse(BBDD$VALOR_SALDO_CB_buro<=14559000,"[ 0 ; 14559000]",ifelse(BBDD$VALOR_SALDO_CB_buro<=35167000,"[ 14559000 ; 35167000]",">35167000"))) 
freq(BBDD$VALOR_SALDO_CB_buro_tram,useNA="ifany")

BBDD$VALOR_MORA_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_CB_buro),"mora_0",ifelse(BBDD$VALOR_MORA_CB_buro==0,"mora_0","mora_1")) 
                                     
freq(BBDD$VALOR_MORA_CB_buro_tram,useNA="ifany")

BBDD$VALOR_INICIAL_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CF_buro),">2400000",ifelse(BBDD$VALOR_INICIAL_CF_buro<=2400000,"[ 0 ; 2400000]",">2400000"))
freq(BBDD$VALOR_INICIAL_CF_buro_tram,useNA="ifany")

BBDD$VALOR_CUPOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUPOS_buro),">6290000",ifelse(BBDD$VALOR_CUPOS_buro<=6290000,"[ 6290000 ; 6290000]",">6290000")) 
freq(BBDD$VALOR_CUPOS_buro_tram,useNA="ifany")

BBDD$VALOR_CUPOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUPOS_buro),">6290000",ifelse(BBDD$VALOR_CUPOS_buro<=500000,"[ 0 ; 500000]",ifelse(BBDD$VALOR_CUPOS_buro<=6290000,"[ 500000 ; 6290000]",">6290000"))) 
freq(BBDD$VALOR_CUPOS_buro_tram,useNA="ifany")

BBDD$PORCENTAJE_UTILIZACION_buro_tram<-ifelse(is.na(BBDD$PORCENTAJE_UTILIZACION_buro),"[ 0 ; 0.07]",ifelse(BBDD$PORCENTAJE_UTILIZACION_buro<=0.07,"[ 0 ; 0.07]",">0.07")) 

freq(BBDD$PORCENTAJE_UTILIZACION_buro_tram,useNA="ifany")


BBDD$PORCENTAJE_UTILIZACION_buro_tram<-ifelse(is.na(BBDD$PORCENTAJE_UTILIZACION_buro),"[ 0 ; 0.07]",ifelse(BBDD$PORCENTAJE_UTILIZACION_buro<=0.07,"[ 0 ; 0.07]",">0.07")) 
freq(BBDD$PORCENTAJE_UTILIZACION_buro_tram,useNA="ifany")


BBDD$VALOR_MORA_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_buro),"mora_0",ifelse(BBDD$VALOR_MORA_buro==0,"mora_0","mora_1")) 
freq(BBDD$VALOR_MORA_buro_tram,useNA="ifany")

BBDD$VALOR_INICIAL_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_SR_buro),"mora_0",ifelse(BBDD$VALOR_INICIAL_SR_buro==0,"mora_0","mora_1")) 
freq(BBDD$VALOR_INICIAL_SR_buro_tram,useNA="ifany")


BBDD$VALOR_SALDO_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_SR_buro),"mora_0",ifelse(BBDD$VALOR_SALDO_SR_buro==0,"mora_0","mora_1")) 
freq(BBDD$VALOR_SALDO_SR_buro_tram,useNA="ifany")

BBDD$VALOR_CUOTAS_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_SR_buro),"mora_0",ifelse(BBDD$VALOR_CUOTAS_SR_buro==0,"mora_0","mora_1")) 
freq(BBDD$VALOR_CUOTAS_SR_buro_tram,useNA="ifany")

BBDD$VALOR_MORA_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_SR_buro),"mora_0",ifelse(BBDD$VALOR_MORA_SR_buro==0,"mora_0","mora_1")) 
freq(BBDD$VALOR_MORA_SR_buro_tram,useNA="ifany")

puntos<-c(2,4,6,8,10)
BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram<-cut(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro, breaks=puntos,include.lowest=TRUE,dig.lab=10)    
freq(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram)

BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro),"ULT_ANIO_MORAS_30_CARTERA_0",ifelse(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro==0,"ULT_ANIO_MORAS_30_CARTERA_0","ULT_ANIO_MORAS_30_CARTERA_0_1")) 

freq(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram,useNA="ifany")

BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro),"ULT_ANIO_MORAS_30_CARTERA_0",ifelse(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro==0,"ULT_ANIO_MORAS_30_CARTERA_0","ULT_ANIO_MORAS_30_CARTERA_0_1")) 
freq(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram,useNA="ifany")






