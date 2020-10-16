#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate", "ROSE", "DMwR")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Inicio: indispensable
load(file= "base_objetivo_fid2.RData")

freq(str_length(BBDD3$CEDULAENC_objetivo), useNA="ifany")
#NA

drops <- c("TIPO_ID_buro")
churn<-BBDD3[ , !(names(BBDD3) %in% drops)]
churn_target<-churn[c( "CEDULAENC_objetivo", "fecha_objetivo")]

#Fin: indispensable

variables_na<-sapply(churn, function(x) sum(is.na(x)))
write.xlsx(variables_na,file=paste0("SALIDA/variables_na_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

glimpse(churn)

#Creación del Training/Testing

#Semilla
set.seed(1976)

indice_particion<-createDataPartition(churn$TARGET_objetivo, p=0.7, list =F)
train<-churn[indice_particion,]
test<-churn[-indice_particion,]

#Validemos tamaño de la muestra
rbind(c("train","test"),cbind((dim(train)[1]/dim(churn)[1])*100 , (dim(test)[1]/dim(churn)[1])*100))

#Validemos la propocion del target en cada muestra
rbind(c("train","test"),cbind(prop.table(table(train$TARGET_objetivo))*100, prop.table(table(test$TARGET_objetivo))*100))

#Reducción del conjunto de variables: Varianza constante
load(file="Ind_Varianza_constante")
#Ind_Varianza_constante<-nearZeroVar(train,freqCut = 99/9, uniqueCut = 10)
#save(Ind_Varianza_constante,file="Ind_Varianza_constante")
#which( colnames(train)=="TARGET_objetivo" )
#Se debe exportar estas variables y examinar el impacto en negocio
write.table(names(churn[,Ind_Varianza_constante]),file=paste("SALIDA/05_MODELAMIENTO_nearZeroVar",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

# eliminar<-c("QUANTO_buro","QUANTO_MOD_buro","PORCEN_DE_CUOTAS_VS_INGRESO_buro","RESPUESTA_buro","VALOR_SALDO_CB_buro","VALOR_MORA_CB_buro","NUMERO_CREDITOS_CV_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","RANGO_0_buro","RANGO_5_buro","RANGO_6_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","ESTADO_CONSULTA_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","FECANTIAPERSECTORBANCASINPOPU_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","CUOTAS_CALCULADAS_CRÉDITOS_buro","CUOTAS_CALCULADAS_HIPOTECARIAS_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","CUPO_SUGERIDO_1_buro","CUPO_SUGERIDO_2_buro","CUPO_SUGERIDO_3_buro","CUPO_SEGUN_MERCADO_buro","CUPO_AJUSTADI_buro","CUPO_EXP_FIN_buro","CUPO_FINAL_buro","EXPERIENCIA_FINANCIERA_buro")
# 
# train<-train[ , !(names(train) %in% eliminar)]
# test<-test[ , !(names(test) %in% eliminar)]

train<-train[ , -Ind_Varianza_constante]
test<-test[ , -Ind_Varianza_constante]

#write.xlsx(unlist(names(train)),file=paste0("SALIDA/train_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#La función nearZerovar del paquete caret nos permite identificar variables con varianza cercana a cero.

#Reducción del conjunto de variables: correlación
input.list<-names(train)
input.list_Fecha<-input.list[grep("*FECHA*", input.list)]
input.list_clave<-input.list[grep("*clave*", input.list)]

eliminar<-c(input.list_Fecha,input.list_clave, c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","TARGET_objetivo","TARGET2_objetivo", "fecha_buro_objetivo", "clave_objetivo","TIPO_ID_buro","RANGO_1_buro","RANGO_2_buro", "RANGO_3_buro","RANGO_4_buro"))

train_corr<-train[ , !(names(train) %in% eliminar)]

nums<-sapply(train_corr, is.numeric)

train_corr<-na.omit(train_corr[,nums])
corr_train<-cor(train_corr,use="complete.obs")

write.xlsx(corr_train,file=paste0("SALIDA/05_modelamiento_correlacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = TRUE, col.names = TRUE)

#Contrucción de data set

eliminar<-c("QUANTO_buro","QUANTO_MOD_buro","PORCEN_DE_CUOTAS_VS_INGRESO_buro","RESPUESTA_buro","VALOR_SALDO_CB_buro","VALOR_MORA_CB_buro","NUMERO_CREDITOS_CV_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","RANGO_0_buro","RANGO_5_buro","RANGO_6_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","SAL_CAPITA_objetivo","max_MADURACION_CUOTA_objetivo","max_MADURACION_SALDO_objetivo","NUMERO_CREDITOS_CB_buro","VALOR_INICIAL_CB_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","mora_max_10ant","mora_max_1ant","mora_max_6ant","f_ult_huella")

train<-churn[indice_particion,]
test<-churn[-indice_particion,]

#Validemos tamaño de la muestra
rbind(c("train","test"),cbind((dim(train)[1]/dim(churn)[1])*100 , (dim(test)[1]/dim(churn)[1])*100))

#Validemos la propocion del target en cada muestra
rbind(c("train","test"),cbind(prop.table(table(train$TARGET_objetivo))*100, prop.table(table(test$TARGET_objetivo))*100))

train<-train[ , !(names(train) %in% eliminar)]
test<-test[ , !(names(test) %in% eliminar)]

write.xlsx(names(train),file=paste0("SALIDA/05_modelamiento_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
write.xlsx(summary(train),file=paste0("SALIDA/05_modelamiento_resumen_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Limpieza
train$PEOR_CALIFI_TRIM_1_ENDEUD_buro<-ifelse(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="A","A",ifelse(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="B","B",ifelse(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="C","C",ifelse(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="D","D",ifelse(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="E","E",NA)))))
freq(train$PEOR_CALIFI_TRIM_1_ENDEUD_buro, useNA="ifany")
train$PEOR_CALIF_TRIM_2_ENDEUD_buro<-ifelse(train$PEOR_CALIF_TRIM_2_ENDEUD_buro=="A","A",ifelse(train$PEOR_CALIF_TRIM_2_ENDEUD_buro=="B","B",ifelse(train$PEOR_CALIF_TRIM_2_ENDEUD_buro=="C","C",ifelse(train$PEOR_CALIF_TRIM_2_ENDEUD_buro=="D","D",ifelse(train$PEOR_CALIF_TRIM_2_ENDEUD_buro=="E","E",NA)))))
freq(train$PEOR_CALIF_TRIM_2_ENDEUD_buro, useNA="ifany")

#Segmentación de variables

var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
var_continua<-c("VLR_CUOTA_objetivo","DIASMORA_I_objetivo","VLR_MORA_objetivo","min_MADURACION_CUOTA_objetivo","min_MADURACION_SALDO_objetivo","MAX_SAL_CAPITA_objetivo","ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","mora_max_actual","mora_max_3ant","min_RESULTADO_SCORE_huellas")
var_discreta<-c("max_CALIF_CART_objetivo","num_cred_libranza_objetivo","COD_PAG_objetivo","TARGET_objetivo","TARGET2_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","ESTADO_CONSULTA_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","num_consultas_huellas","Huellas_BANCOLOMBIA","Huellas_AV_VILLAS","Huellas_BBVA","Huellas_BOGOTA","Huellas_SUDAMERIS","Huellas_OTROS","mes_huella","dia_huella","semana_mes_huella","semana_anio_huella")
var_eliminar<-c("RANGO_1_buro","RANGO_2_buro","RANGO_3_buro","RANGO_4_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","CUOTAS_CALCULADAS_TDC_buro","CUOTAS_CALCULADAS_CRÉDITOS_buro","CUOTAS_CALCULADAS_HIPOTECARIAS_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","CUPO_SUGERIDO_1_buro","CUPO_SUGERIDO_2_buro","CUPO_SUGERIDO_3_buro","CUPO_SEGUN_MERCADO_buro","CUPO_AJUSTADI_buro","CUPO_EXP_FIN_buro","CUPO_FINAL_buro","EXPERIENCIA_FINANCIERA_buro")

#Verificar que todas las variables se hayan segmentado
size(var_continua)[2] + size(var_discreta)[2] + size(var_identificacion)[2] +size(var_eliminar)[2] 
size(train) #La suma anterior debe ser igual a esto

#Tratamiento variables discretas

for(variable in var_discreta){
  train[[variable]]<-as.factor(train[[variable]])
  test[[variable]]<-as.factor(test[[variable]])
  print(variable)
}

for(variable in var_continua){
  train[[variable]]<-as.numeric(train[[variable]])
  test[[variable]]<-as.numeric(test[[variable]])
  print(variable)
}

#Var_eliminar
train<-train[ , !(names(train) %in% var_eliminar)]
test<-test[ , !(names(test) %in% var_eliminar)]

#save(train,test, file="pre_balanceo.Rdata")

#Balanceo de clases  
load(file="pre_balanceo.Rdata")

#Ultima limpieza

# Estudio_NA<-sapply(train, function(x) sum(is.na(x)))
# write.xlsx(Estudio_NA,file=paste0("SALIDA/variables_na_",format(today, format="%Y%m%d"),".xlsx"),row.names = TRUE)

huellas_na<-c("num_consultas_huellas","Huellas_BANCOLOMBIA","Huellas_AV_VILLAS","Huellas_BBVA","Huellas_BOGOTA","Huellas_SUDAMERIS","Huellas_OTROS")

for(variable in huellas_na){
  train[[variable]]<-as.factor(ifelse(is.na(train[[variable]]), 0, train[[variable]]))
  test[[variable]]<-as.factor(ifelse(is.na(test[[variable]]), 0, test[[variable]]))
  print(variable)
}

huellas_na2<-c("mes_huella","dia_huella","semana_mes_huella","semana_anio_huella")
for(variable in huellas_na2){
  train[[variable]]<-as.factor(ifelse(is.na(train[[variable]]), 0, train[[variable]]))
  test[[variable]]<-as.factor(ifelse(is.na(test[[variable]]), 0, test[[variable]]))
  print(variable)
}

moras<-c("mora_max_actual","mora_max_3ant")

for(variable in moras){
  train[[variable]]<-ifelse(is.na(train[[variable]]), 0, train[[variable]])
  test[[variable]]<-ifelse(is.na(test[[variable]]), 0, test[[variable]])
  print(variable)
}

otros<-c("RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ESTADO_CONSULTA_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","min_RESULTADO_SCORE_huellas")

for(variable in otros){
  train[[variable]]<-as.numeric(ifelse(is.na(train[[variable]]), 0, train[[variable]]))
  test[[variable]]<-as.numeric(ifelse(is.na(test[[variable]]), 0, test[[variable]]))
  print(variable)
}

Estudio_NA<-sapply(train, function(x) sum(is.na(x)))
write.xlsx(Estudio_NA,file=paste0("SALIDA/variables_na2_",format(today, format="%Y%m%d"),".xlsx"),row.names = TRUE)

training <-train[ , !(names(train) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo") )]
training2 <-train[ , !(names(train) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo") )]

#downSample
down_training<-downSample(x=training, y=training$TARGET_objetivo)
sapply(down_training, function(x) sum(is.na(x)))


freq(down_training$TARGET_objetivo, useNA="ifany")
table(down_training$Class)
prop.table((table(down_training$Class)))

down_training2<-downSample(x=training2, y=training2$TARGET2_objetivo)
freq(down_training2$TARGET2_objetivo, useNA="ifany")
table(down_training2$Class)
prop.table((table(down_training2$Class)))

#UpSample
up_training<-upSample(x=training, y=training$TARGET_objetivo)
table(up_training$Class)
prop.table((table(up_training$Class)))
freq(up_training$TARGET_objetivo, useNA="ifany")

up_training2<-upSample(x=training2, y=training2$TARGET2_objetivo)
table(up_training2$Class)
prop.table((table(up_training2$Class)))
freq(up_training2$TARGET2_objetivo, useNA="ifany")

#Rose
rose_training<-ROSE(TARGET_objetivo ~., data =training)$data 
table(rose_training$TARGET_objetivo)

rose_training2<-ROSE(TARGET2_objetivo ~., data =training2)$data 
table(rose_training2$TARGET2_objetivo)

#SMOTE
smote_training<-SMOTE(TARGET_objetivo ~., training, k=5, perc.over = 100, perc.under = 200)
table(smote_training$TARGET_objetivo)

smote_training2<-SMOTE(TARGET2_objetivo ~., training2, k=5, perc.over = 100, perc.under = 200)
table(smote_training$TARGET_objetivo)

save(training, training2, down_training, down_training2, up_training, up_training2, rose_training, rose_training2,smote_training, smote_training2, file="balanceo.Rdata")

#Modelamiento


library(randomForest)
set.seed(1976)
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]
modelo_randomForest11<-randomForest(TARGET_objetivo ~., data=down_training,  ntree=5, nsplit=2)
pred_rf <- predict(modelo_randomForest11, test)
caret::confusionMatrix(pred_rf, testing$Churn)


varImpPlot(modelo_randomForest11, sort = T, main="Variable Importance")

forest.pred      <- predict(modelo_randomForest11, newdata = test, type = "class")  
forest.result    <- confusionMatrix(data = forest.pred, test.df$churn)  
forest.precision <- forest.result$byClass['Pos Pred Value']  
forest.recall    <- forest.result$byClass['Sensitivity']  
forest.F1        <- forest.result$byClass['F1']


#Curva roc
#probabilidad modelo
probs<-predict(modelo_randomForest11,test, type="prob")[,2]
plot(modelo_randomForest11)
print(modelo_randomForest11)



modelo_randomForest12<-randomForest(TARGET2_objetivo ~., data=down_training2, ntree=500, nsplit=10)
plot(modelo_randomForest12)

modelo_randomForest21<-randomForest(TARGET_objetivo ~., data=up_training, ntree=500, nsplit=10)
plot(modelo_randomForest21)

modelo_randomForest22<-randomForest(TARGET2_objetivo ~., data=up_training2, ntree=500, nsplit=10)
plot(modelo_randomForest22)

modelo_randomForest31<-randomForest(TARGET_objetivo ~., data=rose_training, ntree=500, nsplit=10)
plot(modelo_randomForest31)
modelo_randomForest32<-randomForest(TARGET2_objetivo ~., data=rose_training2, ntree=500, nsplit=10)

modelo_randomForest41<-randomForest(TARGET_objetivo ~., data=smote_training, ntree=500, nsplit=10)
modelo_randomForest42<-randomForest(TARGET2_objetivo ~., data=smote_training2, ntree=500, nsplit=10)


#Logistic regresion

modelo_log_reg_11<-glm(TARGET_objetivo ~., family=binomial(link = 'logit'), data=down_training)

sapply(smote_training2, function(x) sum(is.na(x)))