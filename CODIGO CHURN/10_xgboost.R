##Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR","zoo", "ROCR")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Inicio: indispensable
load(file= "base_objetivo_fid2.RData")

churn<-BBDD
#Creación del Training/Testing
drops <- c("TIPO_ID_buro")
churn<-BBDD3[ , !(names(BBDD3) %in% drops)]
names(churn)<-gsub("AÃ'O","ANIO",names(churn))
names(churn)<-gsub("Ã???","É",names(churn))

#names(churn)<-gsub("Ã???","E",names(churn))
#CUOTAS_CALCULADAS_CRÃ???DITOS_buro 
names(churn)<-gsub("É","E",names(churn))
load(file="indice_particion.Rdata")
#Fin: indispensable
#Semilla
set.seed(1976)

eliminar<-c("QUANTO_buro","QUANTO_MOD_buro","PORCEN_DE_CUOTAS_VS_INGRESO_buro","RESPUESTA_buro","VALOR_SALDO_CB_buro","VALOR_MORA_CB_buro","NUMERO_CREDITOS_CV_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","RANGO_0_buro","RANGO_5_buro","RANGO_6_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","SAL_CAPITA_objetivo","max_MADURACION_CUOTA_objetivo","max_MADURACION_SALDO_objetivo","NUMERO_CREDITOS_CB_buro","VALOR_INICIAL_CB_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","mora_max_10ant","mora_max_1ant","mora_max_6ant","f_ult_huella")
churn<-churn[ , !(names(churn) %in% eliminar)]  

#Limpieza
churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro<-ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="A","A",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="B","B",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="C","C",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="D","D",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="E","E",NA)))))

freq(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro, useNA="ifany")

churn$PEOR_CALIF_TRIM_2_ENDEUD_buro<-ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="A","A",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="B","B",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="C","C",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="D","D",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="E","E",NA)))))

freq(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro, useNA="ifany")

#Segmentación de variables

var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
var_continua<-c("VLR_CUOTA_objetivo","DIASMORA_I_objetivo","VLR_MORA_objetivo","min_MADURACION_CUOTA_objetivo","min_MADURACION_SALDO_objetivo","MAX_SAL_CAPITA_objetivo","ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_ANIO_MORAS_30_CARTERA_HIST_buro","ULT_ANIO_MORAS_60_CARTERA_HIST_buro","ULT_ANIO_MORAS_90_CARTERA_HIST_buro","ULT_ANIO_MORAS_120_CARTERA_HIST_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","mora_max_actual","mora_max_3ant","min_RESULTADO_SCORE_huellas")
var_discreta<-c("max_CALIF_CART_objetivo","num_cred_libranza_objetivo","COD_PAG_objetivo","TARGET_objetivo","TARGET2_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","ESTADO_CONSULTA_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","num_consultas_huellas","Huellas_BANCOLOMBIA","Huellas_AV_VILLAS","Huellas_BBVA","Huellas_BOGOTA","Huellas_SUDAMERIS","Huellas_OTROS","mes_huella","dia_huella","semana_mes_huella","semana_anio_huella")
var_eliminar<-c("RANGO_1_buro","RANGO_2_buro","RANGO_3_buro","RANGO_4_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","CUOTAS_CALCULADAS_TDC_buro","CUOTAS_CALCULADAS_CREDITOS_buro","CUOTAS_CALCULADAS_HIPOTECARIAS_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","CUPO_SUGERIDO_1_buro","CUPO_SUGERIDO_2_buro","CUPO_SUGERIDO_3_buro","CUPO_SEGUN_MERCADO_buro","CUPO_AJUSTADI_buro","CUPO_EXP_FIN_buro","CUPO_FINAL_buro","EXPERIENCIA_FINANCIERA_buro","FECHA_DATA_buro", "FECHA_ENVIO_buro", "FECHA_DATA_buro","CEDULAENC_buro", "FECHA_buro","FECHA_ENVIO2_buro")

#Verificar que todas las variables se hayan segmentado
size(var_continua)[2] + size(var_discreta)[2] + size(var_identificacion)[2] +size(var_eliminar)[2] 
size(churn)
#Tratamiento variables discretas

for(variable in var_discreta){
  churn[[variable]]<-as.factor(churn[[variable]])
  print(variable)
}

for(variable in var_continua){
  churn[[variable]]<-as.numeric(churn[[variable]])
  print(variable)
}

huellas_na<-c("num_consultas_huellas","Huellas_BANCOLOMBIA","Huellas_AV_VILLAS","Huellas_BBVA","Huellas_BOGOTA","Huellas_SUDAMERIS","Huellas_OTROS")

for(variable in huellas_na){
  churn[[variable]]<-as.factor(ifelse(is.na(churn[[variable]]), 0, churn[[variable]]))
  print(variable)
}

huellas_na2<-c("mes_huella","dia_huella","semana_mes_huella","semana_anio_huella")
for(variable in huellas_na2){
  churn[[variable]]<-as.factor(ifelse(is.na(churn[[variable]]), 0, churn[[variable]]))
  print(variable)
}

moras<-c("mora_max_actual","mora_max_3ant")

for(variable in moras){
  churn[[variable]]<-ifelse(is.na(churn[[variable]]), 0, churn[[variable]])
  print(variable)
}

otros<-c("RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_ANIO_MORAS_30_CARTERA_HIST_buro","ULT_ANIO_MORAS_60_CARTERA_HIST_buro","ULT_ANIO_MORAS_90_CARTERA_HIST_buro","ULT_ANIO_MORAS_120_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ESTADO_CONSULTA_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","min_RESULTADO_SCORE_huellas")

for(variable in otros){
  churn[[variable]]<-as.numeric(ifelse(is.na(churn[[variable]]), 0, churn[[variable]]))
  print(variable)
}

#Var_eliminar
churn<-churn[ , !(names(churn) %in% c(var_eliminar,"CUOTAS_CALCULADAS_CRÃ???DITOS_buro"))]

a<-sapply(churn, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero

factor<-sapply(churn, is.factor)


BF<-churn[,factor]

nums<-sapply(churn, is.numeric)
churn<-churn[,nums]


train<-churn[indice_particion,]
test<-churn[-indice_particion,]

#Validemos tamANIO de la muestra
rbind(c("train","test"),cbind((dim(train)[1]/dim(churn)[1])*100 , (dim(test)[1]/dim(churn)[1])*100))

#Validemos la propocion del target en cada muestra
rbind(c("train","test"),cbind(prop.table(table(train$TARGET_objetivo))*100, prop.table(table(test$TARGET_objetivo))*100))

train<-train[ , !(names(train) %in% eliminar)]
test<-test[ , !(names(test) %in% eliminar)]

# write.xlsx(names(train),file=paste0("SALIDA/05_modelamiento_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
# write.xlsx(summary(train),file=paste0("SALIDA/05_modelamiento_resumen_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Var_eliminar
train<-train[ , !(names(train) %in% var_eliminar)]
test<-test[ , !(names(test) %in% var_eliminar)]

library(xgboost)

bst <- xgboost(data = training, label = TARGET_objetivo, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")