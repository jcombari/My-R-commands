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

tipo_variable<-data.frame(unclass(summary.default(BBDD)), check.names = FALSE, stringsAsFactors = FALSE)
freq(tipo_variable$Class)


discreta<-c("fecha_objetivo", "TARGET_objetivo", "clave_objetivo","clave_objetivo","fecha_objetivo","CALIF_CART_objetivo","COD_OFIC_objetivo","FECDES_objetivo","FECVEN_objetivo","PLAZO_objetivo","COD_PAG_objetivo","FECDES2_objetivo","FECVEN2_objetivo","segmentacion_objetivo","MOD_DEF_objetivo","FECDES_NF_objetivo","COD_PAGADURIA_objetivo","COD_SUCURSAL_objetivo","COD_OFIADMIN_objetivo","FEC_PAGO_objetivo","COD_OFIPAGO_objetivo","NOM_PAGADURIA_objetivo","COD_SECTOR_objetivo","NOM_SECTOR_objetivo","COD_SUBSECTOR_objetivo","NOM_SUBSECTOR_objetivo","COD_OFICINA_objetivo","NOM_SECTORANT_objetivo","NOM_UNIDAD_objetivo","FEC_PAGO2_objetivo","FEC_PAGO_marca_objetivo","TIPO_DE_PAGO_objetivo","marca_prepago_objetivo","FEC_PAGO_SINIESTRO_objetivo","MARCA_SINIESTRO_RANGO_objetivo","TARGET_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","NUMERO_CREDITOS_CB_buro","NUMERO_CREDITOS_CV_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro")
continua<-c("fecha_objetivo", "TARGET_objetivo","TASINT_CTE_objetivo","TASINT_MOR_objetivo","VLR_DESEMB_objetivo","SAL_CAPITA_objetivo","VLR_CUOTA_objetivo","VLR_MORA_objetivo","DIASMORA_I_objetivo","VLR_PAGO_objetivo","VLR_SALDOOBLIG_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","ACIERTA_A_FINANCIERO_buro","QUANTO_buro","VALOR_INICIAL_CB_buro","VALOR_SALDO_CB_buro","VALOR_CUOTAS_CB_buro","VALOR_MORA_CB_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro")

BBDD_continua<-BBDD[continua]
BBDD_discreta<-BBDD[discreta]


largo_disc<-size(BBDD_discreta)[2]

loop.vector_disc <- 5:largo_disc

#Conjunto de datos con predictores que presencia muy significativa de un único valor o "varianza casi cero
super_frecuencia=list()

for (i in loop.vector_disc){ # Loop over loop.vector
  # store data in column.i as x
  x <- BBDD_discreta[,i]
  
  # Plot histogram of x
  aux<-  table( x, BBDD_discreta$TARGET_objetivo,useNA="ifany")
  super_frecuencia<-rbind(super_frecuencia,aux)
}

write.table(aux,file=paste("SALIDA/Variables_discretas_frecuencia_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

