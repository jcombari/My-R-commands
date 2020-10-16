#Limpieza 
rm(list=ls())
.rs.restartR() #restar
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

# freq(str_length(BBDD3$CEDULAENC_objetivo), useNA="ifany")
# 
# 
# variables_na<-sapply(churn, function(x) sum(is.na(x)))
# write.xlsx(variables_na,file=paste0("SALIDA/variables_na_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
# 
# glimpse(churn)

#Creación del Training/Testing
drops <- c("TIPO_ID_buro")
churn<-BBDD3[ , !(names(BBDD3) %in% drops)]
names(churn)<-gsub("AÃ‘O","ANIO",names(churn))
names(churn)<-gsub("Ã‰","E",names(churn))
#CUOTAS_CALCULADAS_CRÃ???DITOS_buro 
names(churn)<-gsub("É","E",names(churn))
#Fin: indispensable

#Semilla
set.seed(1234)

#indice_particion<-createDataPartition(churn$TARGET_objetivo, p=0.7, list =F)
#save(indice_particion,file="indice_particion.Rdata")

#train<-churn[indice_particion,]
#test<-churn[-indice_particion,]

#Validemos tamANIO de la muestra
# rbind(c("train","test"),cbind((dim(train)[1]/dim(churn)[1])*100 , (dim(test)[1]/dim(churn)[1])*100))

#Validemos la propocion del target en cada muestra
# rbind(c("train","test"),cbind(prop.table(table(train$TARGET_objetivo))*100, prop.table(table(test$TARGET_objetivo))*100))

#Reducción del conjunto de variables: Varianza constante
# load(file="Ind_Varianza_constante")
#Ind_Varianza_constante<-nearZeroVar(train,freqCut = 99/9, uniqueCut = 10)
#save(Ind_Varianza_constante,file="Ind_Varianza_constante")
#which( colnames(train)=="TARGET_objetivo" )
#Se debe exportar estas variables y examinar el impacto en negocio
# write.table(names(churn[,Ind_Varianza_constante]),file=paste("SALIDA/05_MODELAMIENTO_nearZeroVar",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

# eliminar<-c("QUANTO_buro","QUANTO_MOD_buro","PORCEN_DE_CUOTAS_VS_INGRESO_buro","RESPUESTA_buro","VALOR_SALDO_CB_buro","VALOR_MORA_CB_buro","NUMERO_CREDITOS_CV_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","RANGO_0_buro","RANGO_5_buro","RANGO_6_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_ANIO_MORAS_60_CARTERA_HIST_buro","ULT_ANIO_MORAS_90_CARTERA_HIST_buro","ULT_ANIO_MORAS_120_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","ESTADO_CONSULTA_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","FECANTIAPERSECTORBANCASINPOPU_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","CUOTAS_CALCULADAS_CREDITOS_buro","CUOTAS_CALCULADAS_HIPOTECARIAS_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","CUPO_SUGERIDO_1_buro","CUPO_SUGERIDO_2_buro","CUPO_SUGERIDO_3_buro","CUPO_SEGUN_MERCADO_buro","CUPO_AJUSTADI_buro","CUPO_EXP_FIN_buro","CUPO_FINAL_buro","EXPERIENCIA_FINANCIERA_buro")
# 
# train<-train[ , !(names(train) %in% eliminar)]
# test<-test[ , !(names(test) %in% eliminar)]

# train<-train[ , -Ind_Varianza_constante]
# test<-test[ , -Ind_Varianza_constante]

#write.xlsx(unlist(names(train)),file=paste0("SALIDA/train_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#La función nearZerovar del paquete caret nos permite identificar variables con varianza cercana a cero.

#Reducción del conjunto de variables: correlación
# input.list<-names(train)
# input.list_Fecha<-input.list[grep("*FECHA*", input.list)]
# input.list_clave<-input.list[grep("*clave*", input.list)]
# 
# eliminar<-c(input.list_Fecha,input.list_clave, c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","TARGET_objetivo","TARGET2_objetivo", "fecha_buro_objetivo", "clave_objetivo","TIPO_ID_buro","RANGO_1_buro","RANGO_2_buro", "RANGO_3_buro","RANGO_4_buro"))
# 
# train_corr<-train[ , !(names(train) %in% eliminar)]
# 
# nums<-sapply(train_corr, is.numeric)
# 
# train_corr<-na.omit(train_corr[,nums])
# corr_train<-cor(train_corr,use="complete.obs")
# 
# write.xlsx(corr_train,file=paste0("SALIDA/05_modelamiento_correlacion_",format(today, format="%Y%m%d"),".xlsx"),row.names = TRUE, col.names = TRUE)
# 
#Contrucción de data set

eliminar<-c("QUANTO_buro","QUANTO_MOD_buro","PORCEN_DE_CUOTAS_VS_INGRESO_buro","RESPUESTA_buro","VALOR_SALDO_CB_buro","VALOR_MORA_CB_buro","NUMERO_CREDITOS_CV_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","RANGO_0_buro","RANGO_5_buro","RANGO_6_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","VALOR_SALDO_CODEUDORES_buro","VALOR_CUOTAS_CODEUDORES_buro","VALOR_MORA_CODEUDORES_buro","SAL_CAPITA_objetivo","max_MADURACION_CUOTA_objetivo","max_MADURACION_SALDO_objetivo","NUMERO_CREDITOS_CB_buro","VALOR_INICIAL_CB_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","mora_max_10ant","mora_max_1ant","mora_max_6ant","f_ult_huella")
churn<-churn[ , !(names(churn) %in% eliminar)]  

#Limpieza
churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro<-ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="A","A",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="B","B",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="C","C",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="D","D",ifelse(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro=="E","E",NA)))))
freq(churn$PEOR_CALIFI_TRIM_1_ENDEUD_buro, useNA="ifany")
churn$PEOR_CALIF_TRIM_2_ENDEUD_buro<-ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="A","A",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="B","B",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="C","C",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="D","D",ifelse(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro=="E","E",NA)))))
freq(churn$PEOR_CALIF_TRIM_2_ENDEUD_buro, useNA="ifany")
freq(churn$RANGO_APROXIMADO_EDAD_buro)
churn$RANGO_APROXIMADO_EDAD_buro[is.na(churn$RANGO_APROXIMADO_EDAD_buro)]<-'18-21' 
churn$RANGO_APROXIMADO_EDAD_buro[churn$RANGO_APROXIMADO_EDAD_buro==""]<-'18-21'
churn$RANGO_APROXIMADO_EDAD_buro[churn$RANGO_APROXIMADO_EDAD_buro=="0"]<-'18-21'
freq(churn$GENERO_buro)
churn$GENERO_buro[churn$GENERO_buro==""]<-'0'
freq(churn$GENERO_buro)
freq(churn$CIUDAD_DE_EXPEDICION_buro)
churn$CIUDAD_DE_EXPEDICION_buro[churn$CIUDAD_DE_EXPEDICION_buro==""]<-'0'
freq(churn$CIUDAD_DE_EXPEDICION_buro)

freq(churn$GENERO_buro)
churn$GENERO_buro[is.na(churn$GENERO_buro)]<-'0' 
sum(is.na(churn$GENERO_buro))
freq(churn$GENERO_buro)

freq(churn$CIUDAD_DE_EXPEDICION_buro)
churn$CIUDAD_DE_EXPEDICION_buro[is.na(churn$CIUDAD_DE_EXPEDICION_buro)]<-'0' 
freq(churn$CIUDAD_DE_EXPEDICION_buro)

#Segmentación de variables
var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
var_continua<-c("VLR_CUOTA_objetivo","DIASMORA_I_objetivo","VLR_MORA_objetivo","min_MADURACION_CUOTA_objetivo","min_MADURACION_SALDO_objetivo","MAX_SAL_CAPITA_objetivo","ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_ANIO_MORAS_30_CARTERA_HIST_buro","ULT_ANIO_MORAS_60_CARTERA_HIST_buro","ULT_ANIO_MORAS_90_CARTERA_HIST_buro","ULT_ANIO_MORAS_120_CARTERA_HIST_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","mora_max_actual","mora_max_3ant","min_RESULTADO_SCORE_huellas")
var_discreta<-c("RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","max_CALIF_CART_objetivo","num_cred_libranza_objetivo","COD_PAG_objetivo","TARGET_objetivo","TARGET2_objetivo","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","ESTADO_CONSULTA_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","num_consultas_huellas","Huellas_BANCOLOMBIA","Huellas_AV_VILLAS","Huellas_BBVA","Huellas_BOGOTA","Huellas_SUDAMERIS","Huellas_OTROS","mes_huella","dia_huella","semana_mes_huella","semana_anio_huella")
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

otros<-c("ACIERTA_A_FINANCIERO_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","VALOR_CUOTAS_CB_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_ANIO_MORAS_30_CARTERA_HIST_buro","ULT_ANIO_MORAS_60_CARTERA_HIST_buro","ULT_ANIO_MORAS_90_CARTERA_HIST_buro","ULT_ANIO_MORAS_120_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ESTADO_CONSULTA_buro","ENDEUDAMIENTO_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro","QUANTO2_buro","min_RESULTADO_SCORE_huellas")

for(variable in otros){
  churn[[variable]]<-as.numeric(ifelse(is.na(churn[[variable]]), 0, churn[[variable]]))
  print(variable)
}

#Var_eliminar
churn<-churn[ , !(names(churn) %in% c(var_eliminar,"CUOTAS_CALCULADAS_CRÃ???DITOS_buro"))]

a<-sapply(churn, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero

#indice_particion<-createDataPartition(churn$TARGET_objetivo, p=0.7, list =F)
#indice_particion2<-createDataPartition(churn$TARGET2_objetivo, p=0.7, list =F)
#save(indice_particion,indice_particion2, file="indice_particion.Rdata")
load(file="indice_particion.Rdata")
train<-churn[indice_particion,]
test<-churn[-indice_particion,]
 
train2<-churn[indice_particion2,]
test2<-churn[-indice_particion2,]

#Validemos tamANIO de la muestra
rbind(c("train","test"),cbind((dim(train)[1]/dim(churn)[1])*100 , (dim(test)[1]/dim(churn)[1])*100))
rbind(c("train2","test2"),cbind((dim(train2)[1]/dim(churn)[1])*100 , (dim(test2)[1]/dim(churn)[1])*100))
#Validemos la propocion del target en cada muestra
rbind(c("train","test"),cbind(prop.table(table(train$TARGET_objetivo))*100, prop.table(table(test$TARGET_objetivo))*100))
rbind(c("train2","test2"),cbind(prop.table(table(train2$TARGET2_objetivo))*100, prop.table(table(test2$TARGET2_objetivo))*100))

train<-train[ , !(names(train) %in% eliminar)]
test<-test[ , !(names(test) %in% eliminar)]

train2<-train2[ , !(names(train2) %in% eliminar)]
test2<-test2[ , !(names(test2) %in% eliminar)]

#write.xlsx(names(train),file=paste0("SALIDA/05_modelamiento_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
#write.xlsx(summary(train),file=paste0("SALIDA/05_modelamiento_resumen_variables_segmentar_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Var_eliminar
train2<-train2[ , !(names(train2) %in% var_eliminar)]
test2<-test2[ , !(names(test2) %in% var_eliminar)]

#save(train,test,train2, test2, file="pre_balanceo2.Rdata")

#Balanceo de clases  
#load(file="pre_balanceo2.Rdata")
a<-sapply(train, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero
a<-sapply(test, function(x) sum(is.na(x)))
a[a>0] #Debe dar vacio o cero

#Ultima limpieza

var_identificacion<-c("CEDULAENC_objetivo","fecha_objetivo","clave_buro_objetivo","fecha_buro_objetivo","clave_objetivo","TIPO_ID_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","CEDULAENC_buro","FECHA_buro","FECHA_ENVIO2_buro","clave_mora", "FECHA_ENVIO_buro","FECHA_DATA_buro")
eliminar<-c("OBLIGA_MORA_60_CARTERA_ACTUAL_buro",
            "OBLIGA_MORA_90_CARTERA_ACTUAL_buro",
            "CTAS_EN_COBRADOR_CARTERA_ACT_buro",
            "VAL_MORASECTORBANCASIN_POPULAR_buro",
            "VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro",
            "VAL_MORA_SECTOR_HIP_SIN_POPU_buro", "CIUDAD_DE_EXPEDICION_buro","COD_PAG_objetivo ","min_MADURACION_CUOTA_objetivo")

training <-train[ , !(names(train) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo",eliminar) )]
training2 <-train2[ , !(names(train2) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo",eliminar) )]

testing <-test[ , !(names(test) %in% c(var_identificacion,"TARGET2_objetivo","COD_PAG_objetivo", eliminar))]
testing2 <-test2[ , !(names(test2) %in% c(var_identificacion,"TARGET_objetivo","COD_PAG_objetivo",eliminar) )]

#Balanceo
#downSample

down_training<-downSample(x=training, y=training$TARGET_objetivo)
down_training2<-downSample(x=training2, y=training2$TARGET2_objetivo)
save(down_training, down_training2, file="balanceo_down.Rdata")
save(indice_particion, indice_particion2, train, train2, test, test2, training,training2, testing,  testing2, file="balanceo_down_extras.Rdata")

gc()

#UpSample
up_training<-upSample(x=training, y=training$TARGET_objetivo)
save(up_training, file="balanceo_up.Rdata")
save(indice_particion, train, test, training, testing, file="balanceo_up_extras.Rdata")

gc()

up_training2<-upSample(x=training2, y=training2$TARGET2_objetivo)
gc()
save( up_training2, file="balanceo_up2.Rdata")
save(indice_particion2, train2, test2, training2, testing2, file="balanceo_up2_extras.Rdata")

#Rose
rose_training<-ROSE(TARGET_objetivo ~., data =training)$data 
save( rose_training, file="balanceo_rose.Rdata")
save(indice_particion, train, test,training, testing, file="balanceo_rose_extras.Rdata")


#OJO DESDE ACA
rose_training2<-ROSE(TARGET2_objetivo ~., data =training2)$data 
gc()
save(rose_training2, file="balanceo_rose2.Rdata")
save(indice_particion2, train2, test2, training2, testing2,  file="balanceo_rose2_extras.Rdata")

gc()
#SMOTE

training<-training[ , !(names(training) %in% eliminar)]
gc()
smote_training<-SMOTE(TARGET_objetivo ~., training, k=5, perc.over = 100, perc.under = 200)
save( smote_training, file="balanceo_smote.Rdata")
save(indice_particion, train, test,training, testing, file="balanceo_smote_extras.Rdata")

smote_training2<-SMOTE(TARGET2_objetivo ~., training2, k=5, perc.over = 100, perc.under = 200)
save( smote_training2, file="balanceo_smote2.Rdata")
save(indice_particion2, train2, test2,training2, testing2, file="balanceo_smote2_extras.Rdata")

gc()




#modelamiento
#Random Forest
library(randomForest)
set.seed(1976)
eliminar<-c("Class") 
down_training<-down_training[ , !(names(down_training) %in% eliminar)]

plot(modelo_randomForest11)
varImpPlot(modelo_randomForest11, sort = T, main="Variable Importance")
VI<-as.data.frame(unlist(importance(modelo_randomForest11)))
VI<-VI[order( -VI$MeanDecreaseGini),] 
varImp(modelo_randomForest11)
importanceOrder<-order(-modelo_randomForest11$importance)
importanceOrder<-importanceOrder[1:10]
names<-rownames(modelo_randomForest11$importance)[importanceOrder]

par(mfrow=c(5, 2), xpd=NA)
# for (name in names){
#   partialPlot(modelo_randomForest11,down_training , eval(name), main=name, xlab=name,ylim=c(-.2,.9))
# }

# Evaluation metrics  
randomForest11.pred      <- predict(modelo_randomForest11, newdata = testing, type = "class")  
randomForest11.result      <- confusionMatrix(data = randomForest11.pred, testing$TARGET_objetivo)  
randomForest11.precision <- randomForest11.result$byClass['Pos Pred Value']  
randomForest11.recall    <- randomForest11.result$byClass['Sensitivity']  
randomForest11.F1        <- randomForest11.result$byClass['F1']

#Curva roc
library(ROCR)
#probabilidad modelo
#prediccion
probs<-predict(modelo_randomForest11,testing, type="prob")[,2]
pred<-prediction(probs,testing$TARGET_objetivo)
au<-performance(pred,"auc")@y.values[[1]]
#Perfomance dle objeto
pe<-performance(pred,"tpr", "fpr")

#Area sobre la curva (AUC)



pd<-data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))

p<-ggplot(pd, aes(x=fpr, y=tpr))
p<-p+geom_line(colour="red")
p<-p+xlab("False Posite Rate")+ylab("True Posite Rate")
p<-p+ggtitle("Curva Roc")
#p<-p+theme(plot.title = element_test(size=10))
print(p)


