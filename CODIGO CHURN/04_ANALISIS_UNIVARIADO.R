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

discreta<-c("fecha_objetivo","TARGET_objetivo" ,"CALIF_CART_objetivo","COD_OFIC_objetivo","PLAZO_objetivo","COD_PAG_objetivo","FECDES2_objetivo","FECVEN2_objetivo","MOD_DEF_objetivo","COD_PAGADURIA_objetivo","COD_SUCURSAL_objetivo","COD_OFIADMIN_objetivo","COD_OFIPAGO_objetivo","COD_SECTOR_objetivo","COD_SUBSECTOR_objetivo","COD_OFICINA_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","NUMERO_CREDITOS_CB_buro","NUMERO_CREDITOS_CV_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","ESTADO_CONSULTA_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","FECHA_ENVIO_buro","FECHA_DATA_buro","FECHA_buro","FECHA_ENVIO2_buro")
continua<-c("fecha_objetivo","TARGET_objetivo", "TASINT_CTE_objetivo","TASINT_MOR_objetivo","VLR_DESEMB_objetivo","SAL_CAPITA_objetivo","VLR_CUOTA_objetivo","VLR_MORA_objetivo","DIASMORA_I_objetivo","VLR_PAGO_objetivo","VLR_SALDOOBLIG_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","ACIERTA_A_FINANCIERO_buro","QUANTO_buro","VALOR_INICIAL_CB_buro","VALOR_SALDO_CB_buro","VALOR_CUOTAS_CB_buro","VALOR_MORA_CB_buro","VALOR_INICIAL_CV_buro","VALOR_SALDO_CV_buro","VALOR_CUOTAS_CV_buro","VALOR_MORA_CV_buro","VALOR_INICIAL_CF_buro","VALOR_SALDO_CF_buro","VALOR_CUOTAS_CF_buro","VALOR_MORA_CF_buro","VALOR_CUPOS_buro","VALOR_UTILIZADO_buro","PORCENTAJE_UTILIZACION_buro","VALOR_CUOTAS_buro","VALOR_MORA_buro","VALOR_INICIAL_SR_buro","VALOR_SALDO_SR_buro","VALOR_CUOTAS_SR_buro","VALOR_MORA_SR_buro","VALOR_CUOTAS_CELULARES_TELCOS_buro","VALOR_MORA_TELCOS_buro","VALOR_INICIAL_COOPERATIVAS_buro","VALOR_SALDO_COOPERATIVAS_buro","VALOR_CUOTAS_COOPERATIVAS_buro","VALOR_MORA_COOPERATIVAS_buro","OBLIGA_AL_DIA_CARTERA_ACTUAL_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","OBLIGA_MORA_60_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CTAS_EN_COBRADOR_CARTERA_ACT_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CANCEL_MAL_MANEJO_CARTERA_HIST_buro","CARTERA_RECUPE_CARTERA_HIST_buro","TDC_ALTURA_MAXIMA_DE_MORA_buro","CARTERA_BANCA_ALT_MAX_DE_MORA_buro","CARTERA_COOPE_ALT_MAX_DE_MORA_buro","CARTERA_HIPOTE_ALT_MAX_DE_MORA_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_CTES_ACT_CTA_BANCA_buro","CTAS_EMBARGADAS_CTAS_BANCARIAS_buro","CANCEL_MAL_MANEJO_CTAS_BANCA_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","ENDEUDAMIENTO_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","CUPO_SIN_POPULAR_buro","MAX_CUPO_TDC_SIN_POPULAR_buro","PROMEDIO_CUPO_TDC_SIN_POPULAR_buro","VALOR_UTILIZADO_SIN_POPULAR_buro","UTILIZACION_SIN_POPULAR_buro","VALOR_CUOTAS_SIN_POPULAR_buro","VALOR_EN_MORA_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","UTIL_SECTOR_BANCA_SIN_POPULAR_buro","VAL_CUO_SECTOR_BANCA_SIN_POPU_buro","VAL_MORASECTORBANCASIN_POPULAR_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","CUPO_SECTOR_HIP_SIN_POPULAR_buro","MAX_CUPO_SECTOR_HIP_SIN_POPU_buro","PROM_CUPO_SECTOR_HIP_SIN_POPU_buro","VAL_UTIL_SECTOR_HIP_SIN_POP_buro","UTIL_SECTOR_HIP_SIN_POPULAR_buro","VAL_CUOTAS_SECTOR_HIP_SIN_POPU_buro","VAL_MORA_SECTOR_HIP_SIN_POPU_buro", "PLAZO_objetivo","CUOTA_PAGA2_objetivo", "CUOTA_PENDIENTE_objetivo" )

borrar <- c("fecha_objetivo","TARGET_objetivo")
BBDD_continua<-BBDD[continua]
BBDD_discreta<-BBDD[discreta]

#Convertir todo el dataset de discreta a factor
BBDD_discreta<- as.data.frame(lapply(BBDD_discreta, as.factor))

# mean,median,25th and 75th quartiles,min,max
var_cont_summary<-aggregate(BBDD_continua[ , !(names(BBDD_continua) %in% borrar)], by = list(factor(BBDD$fecha_objetivo)), FUN = function(x)  summary(x))
write.table(t(var_cont_summary),file=paste("SALIDA/summary_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)

var_cont_perc<-aggregate(BBDD_continua[ , !(names(BBDD_continua) %in% borrar)], by = list(factor(BBDD$fecha_objetivo)), FUN = function(x) quantile(x, probs = seq(0,1, 0.05),na.rm = TRUE ))
write.table(t(var_cont_perc),file=paste("SALIDA/percentiles_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)
var_cont_mean<-aggregate(BBDD_continua[ , !(names(BBDD_continua) %in% borrar)], by = list(factor(BBDD$fecha_objetivo)), FUN = function(x)  mean(x, na.rm=TRUE))
write.table(t(var_cont_mean),file=paste("SALIDA/mean_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)

# Tukey min,lower-hinge, median,upper-hinge,max

var_cont_tukey<-aggregate(BBDD_continua[ , !(names(BBDD_continua) %in% borrar)], by = list(factor(BBDD$fecha_objetivo)), FUN = function(x)  fivenum(x))
write.table(t(var_cont_mean),file=paste("SALIDA/tukey_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)

#http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics
#the number of values (nbr.val), the number of null values (nbr.null), the number of missing values (nbr.na), the minimal value (min), the maximal value (max), the range (range, that is, max-min) and the sum of all non-missing values (sum)
#https://www.rdocumentation.org/packages/pastecs/versions/1.3.21/topics/stat.desc

#do we have to return various descriptive statistics (by default, it is TRUE)? These are: the median (median), the mean (mean), the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the p level, the variance (var), the standard deviation (std.dev) and the variation coefficient (coef.var) defined as the standard deviation divided by the mean
#do we have to return normal distribution statistics (by default, it is FALSE)? the skewness coefficient g1 (skewness), its significant criterium (skew.2SE, that is, g1/2.SEg1; if skew.2SE > 1, then skewness is significantly different than zero), kurtosis coefficient g2 (kurtosis), its significant criterium (kurt.2SE, same remark than for skew.2SE), the statistic of a Shapiro-Wilk test of normality (normtest.W) and its associated probability (normtest.p)
#library(pastecs)

var_cont_est_desc<-aggregate(BBDD_continua[ , !(names(BBDD_continua) %in% borrar)], by = list(factor(BBDD$fecha_objetivo)), FUN = function(x) stat.desc(x))
write.table(t(var_cont_est_desc),file=paste("SALIDA/est_desc_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)

#Data Frame Summaries
#C:\Users\jennyfer.combariza\AppData\Local\Temp\RtmpKy7upt

BBDD_discreta2<-BBDD_discreta[ , !(names(BBDD_discreta) %in% borrar)]
BBDD_continua2<-BBDD_continua[ , !(names(BBDD_continua) %in% borrar)]
view(dfSummary(BBDD_discreta2))
view(dfSummary(BBDD_continua2))

fecha<-unique(BBDD_continua$fecha_objetivo)
fecha<-sort(fecha, decreasing = FALSE)
names_discreta<-names(BBDD_discreta)
names_continua<-names(BBDD_continua)

for (i in 1:length(fecha)){ 
  aux <- BBDD_continua2[BBDD_continua$fecha_objetivo==fecha[i],]
  aux2<- BBDD_discreta2[BBDD_discreta$fecha_objetivo==fecha[i],]
  view(dfSummary(aux), file = paste0("Variables_continuas_",fecha[i],".html"))
  view(dfSummary(aux2), file = paste0("Variables_discreta_",fecha[i],".html"))
  assign(paste0("BBDD_continua_",fecha[i] ), aux) 
  assign(paste0("BBDD_discreta_",fecha[i] ), aux2)
}

#C:\Users\jennyfer.combariza\AppData\Local\Temp


#Analizando sólo los malos (churn)


for (i in 1:length(fecha)){ 
    aux <- BBDD_continua2[BBDD_continua$fecha_objetivo==fecha[i] & BBDD_continua$TARGET_objetivo==1,]
    aux2<- BBDD_discreta2[BBDD_discreta$fecha_objetivo==fecha[i] & BBDD_continua$TARGET_objetivo==1,]
    view(dfSummary(aux), file = paste0("Variables_continuas_churn",fecha[i],".html"))
    view(dfSummary(aux2), file = paste0("Variables_discreta_churn",fecha[i],".html"))
    assign(paste0("BBDD_continua_churn",fecha[i] ), aux) 
    assign(paste0("BBDD_discreta_churn",fecha[i] ), aux2)
}

#save(BBDD_continua_201711, BBDD_continua_201712, BBDD_continua_201801, BBDD_continua_201802, BBDD_continua_201803, BBDD_continua_201804, BBDD_continua_201805, BBDD_continua_201806, BBDD_discreta_201711, BBDD_discreta_201712, BBDD_discreta_201801, BBDD_discreta_201802, BBDD_discreta_201803, BBDD_discreta_201804, BBDD_discreta_201805, BBDD_discreta_201806,BBDD_continua_churn201711, BBDD_continua_churn201712, BBDD_continua_churn201801, BBDD_continua_churn201802, BBDD_continua_churn201803, BBDD_continua_churn201804, BBDD_continua_churn201805, BBDD_continua_churn201806, BBDD_discreta_churn201711, BBDD_discreta_churn201712, BBDD_discreta_churn201801, BBDD_discreta_churn201802, BBDD_discreta_churn201803, BBDD_discreta_churn201804, BBDD_discreta_churn201805, BBDD_discreta_churn201806, file="univariado.Rdata")
load(file="univariado.Rdata")

#Frecuencia
largo_disc<-size(BBDD_discreta2)[2]

loop.vector_disc <- 2:largo_disc

super_frecuencia=list()

for (i in loop.vector_disc){ # Loop over loop.vector
  # store data in column.i as x
  x <- BBDD_discreta2[,i]
  
  # Plot histogram of x
  aux<-  table( x, BBDD_discreta$TARGET_objetivo,useNA="ifany")
  super_frecuencia<-rbind(super_frecuencia,aux)
}

write.table(super_frecuencia,file=paste("SALIDA/Variables_discretas_frecuencia_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)


#Análisis bivariado

load(file="univariado.Rdata")

ApplyQuintiles <- function(x) {
  if(length(unique(quantile(x, setdiff(seq(0,1, by=0.05),seq(0,1)) ,na.rm = TRUE)))==1 & unique(quantile(x, probs = setdiff(seq(0,1, by=0.05),seq(0,1)) ,na.rm = TRUE))==0){
  "SIN_DATO"
    }else{
  cut(x, breaks=c(unique(quantile(x, probs = seq(0,1, by=0.05),na.rm = TRUE))),include.lowest=TRUE,dig.lab=10)
  }
}

percentiles <- function(x) {
  quantile(x, probs = setdiff(seq(0,1, by=0.05),seq(0,1)),na.rm = TRUE)
}



#names(aux)[42]
names(aux)[5]
x<-aux$VLR_CUOTA_objetivo

a<-names(aux)[43]

ignorar<-c("OBLIGA_MORA_60_CARTERA_ACTUAL_buro","VLR_SALDOOBLIG_objetivo","DUDOSO_RECAUDO_CARTERA_ACTUAL_buro","CARTERA_CASTIG_CARTERA_ACTUAL_buro","OBLIGA_MORA_120_CARTERA_ACTUAL_buro","OBLIGA_MORA_90_CARTERA_ACTUAL_buro")

for (j in 1:length(fecha)){ 
  aux=""
  aux<-as.data.frame(get(paste0("BBDD_continua_",fecha[j] )))
  largo_cont<-dim(aux)[2]
  print(largo_cont)
  loop.vector_cont <- 1:largo_cont
  print(fecha[j])
  for (i in loop.vector_cont){ # Loop over loop.vector
    # store data in column.i as x
    
    x <- aux[,i]
    nombre<-names(aux)
    if(names(aux)[i] %in% ignorar){
      a<-1
    }else{
      pepe<-as.data.frame(ApplyQuintiles(x))
      aux<-as.data.frame(cbind(aux,pepe))
      names(aux)[names(aux) == "ApplyQuintiles(x)"] <- paste0(nombre[i],"_tram")
      print(i)
    }
  }
  if(names(aux)[i] %in% ignorar){
    a<-1
  }else{
    assign(paste0("BBDD_continua_biv_",fecha[j] ), aux)
    print(paste0("BBDD_continua_biv_",fecha[j] ))
    print(paste("i=",i))
  }
}

save(BBDD_continua_biv_201711, BBDD_continua_biv_201801,BBDD_continua_biv_201802, BBDD_continua_biv_201803,BBDD_continua_biv_201804,BBDD_continua_biv_201805,BBDD_continua_biv_201806,file="bivariado.Rdata")

#Listamo de variable trameadas
#Frecuencia x tramo de variables continua

for (j in 1:length(fecha)){ 
  aux=""
  aux<-as.data.frame(get(paste0("BBDD_continua_biv_",fecha[j] )))
  query=paste0("select TARGET_objetivo from  BBDD_continua where fecha_objetivo=",as.numeric(fecha[j]))
  aux0<-sqldf(query,verbose=TRUE) 
  input.list<-names(aux)
  input.list<-input.list[grep("*_tram*", input.list)]
  aux<-aux[,input.list]
  largo_tramo<-size(aux)[2]
  loop.vector_tramo <- 2:largo_tramo
  
  super_tramo=list()
  
  for (i in loop.vector_tramo){ # Loop over loop.vector
    # store data in column.i as x
    x <- aux[,i]
    
    # table 
    aux2<-  table( x, aux0$TARGET_objetivo,useNA="ifany")
    aux2<-rbind(names(aux)[i],aux2)
    super_tramo<-rbind(super_tramo,aux2)
  }
  
  write.table(super_tramo,file=paste0("SALIDA/tramo_BBDD_continua_",fecha[j],"_" ,format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
}
  
#Trameado de la cartera total
#Eliminar notación cientifica para los nombres de los tramos
#options("scipen"=100, "digits"=22)
aux<-BBDD_continua2
col_numeric <- which( sapply(aux, is.numeric ) ) 
percentiles<-quantile(x = unlist( aux[,  col_numeric] ), probs = setdiff(seq(0,1, by=0.05),seq(0,1)),na.rm = TRUE)

largo_cont<-dim(aux)[2]
print(largo_cont)
loop.vector_cont <- 1:largo_cont
percentiles<-""

for (i in loop.vector_cont){ # Loop over loop.vector
  # store data in column.i as x
  x <- aux[,i]
  nombre<-names(aux)
  pepe<-as.data.frame(ApplyQuintiles(x))
  aux<-as.data.frame(cbind(aux,pepe))
  names(aux)[names(aux) == "ApplyQuintiles(x)"] <- paste0(nombre[i],"_tram")
  print(i)
  assign(paste0("BBDD_continua_biv"), aux)
}


aux<-BBDD_continua_biv
aux0<-sqldf("select TARGET_objetivo from  BBDD_continua")
input.list<-names(aux)
input.list<-input.list[grep("*_tram*", input.list)]
aux<-aux[,input.list]
largo_tramo<-size(aux)[2]
loop.vector_tramo <- 2:largo_tramo

super_tramo=list()
#Para eliminar notación científica

for (i in loop.vector_tramo){ # Loop over loop.vector
  # store data in column.i as x
  x <- aux[,i]
  
  # table 
  aux2<-  table( x, aux0$TARGET_objetivo,useNA="ifany")
  aux2<-rbind(names(aux)[i],aux2)
  super_tramo<-rbind(super_tramo,aux2)
}

write.table(super_tramo,file=paste0("SALIDA/tramo_BBDD_continua_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

BBDD_continua$factor_unitario<-1

percentiles<-aggregate(BBDD_continua2, by = list(factor(BBDD_continua$factor_unitario)), FUN = function(x) quantile(x, probs = seq(0,1, 0.05),na.rm = TRUE ))

write.table(t(percentiles),file=paste0("SALIDA/precentiles_continua_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Variables continua a reconsiderar como discreta
discreta_re<-c("fecha_objetivo","TARGET_objetivo" ,"CALIF_CART_objetivo","COD_OFIC_objetivo","PLAZO_objetivo","COD_PAG_objetivo","FECDES2_objetivo","FECVEN2_objetivo","MOD_DEF_objetivo","COD_PAGADURIA_objetivo","COD_SUCURSAL_objetivo","COD_OFIADMIN_objetivo","COD_OFIPAGO_objetivo","COD_SECTOR_objetivo","COD_SUBSECTOR_objetivo","COD_OFICINA_objetivo","CUOTA_PAGA2_objetivo","CUOTA_PENDIENTE_objetivo","RANGO_APROXIMADO_EDAD_buro","GENERO_buro","CIUDAD_DE_EXPEDICION_buro","NUMERO_OBLIGACIONES_ACTIVAS_buro","NUMERO_CREDITOS_CB_buro","NUMERO_CREDITOS_CV_buro","NUMERO_CREDITOS_CF_buro","NUMERO_TDC_buro","FECHA_MAS_ANTIGUA_APERTURA_buro","NUMERO_CREDITOS_SR_buro","NUMERO_CELULARES_TELCOS_buro","NUMERO_CREDITOS_COOPERATIVAS_buro","NUMERO_CREDITOS_CODEUDORES_buro","PEOR_CALIFI_TRIM_1_ENDEUD_buro","PEOR_CALIF_TRIM_2_ENDEUD_buro","ESTADO_CONSULTA_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","FEC_MAS_ANTI_APER_TDC_SIN_POPU_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro","FECANTIAPERSECTORBANCASINPOPU_buro","NUMOBLVIGENSECTOR_HIP_SIN_POPU_buro","FEC_MASANTAPERSECTORHIPSIN_POP_buro","PLAZO_CREDITO_buro","PLAZO_HIPOTECARIO_buro","TASA_HIPOTECARIO_buro","TASA_CREDITOS_buro","PLAZO_TDC_buro","OBLIGA_MORA_30_CARTERA_ACTUAL_buro","ULT_AÑO_MORAS_30_CARTERA_HIST_buro","ULT_AÑO_MORAS_60_CARTERA_HIST_buro","ULT_AÑO_MORAS_90_CARTERA_HIST_buro","ULT_AÑO_MORAS_120_CARTERA_HIST_buro","CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro","CTAS_SALDADAS_CTAS_BANCA_buro","TOTAL_CONSULTAS_ULT_6_MESES_buro","NUM_TDC_VIGENTES_SIN_POPULAR_buro","NUMOBLVIGENSECTORBANCASIN_POPU_buro")
BBDD_discreta_re<-BBDD[discreta_re]
BBDD_discreta2_re<-BBDD_discreta_re[ , !(names(BBDD_discreta_re) %in% borrar)]

largo_disc<-size(BBDD_discreta2_re)[2]

loop.vector_disc <- 2:largo_disc

super_frecuencia=list()

for (i in loop.vector_disc){ # Loop over loop.vector
  # store data in column.i as x
  x <- BBDD_discreta2_re[,i]
  
  # Plot histogram of x
  aux<-  table( x, BBDD_discreta_re$TARGET_objetivo,useNA="ifany")
  aux<-rbind(paste0(names(BBDD_discreta2_re)[i],"_clave"),aux)
  print(names(BBDD_discreta2_re)[i])
  super_frecuencia<-rbind(super_frecuencia,aux)
}

write.table(super_frecuencia,file=paste("SALIDA/Variables_discretas_frecuencia_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)


#Reconsiderar algunas variables de discreta a continua
#Discretas a continuas

reconsideracion<-c("NUMERO_OBLIGACIONES_ACTIVAS_buro","NUMERO_CREDITOS_CB_buro_clave","NUMERO_CREDITOS_CF_buro_clave") 

BBDD_recon<-rbind(rbind("NUMERO_CREDITOS_CB_buro",t(percentiles(BBDD$NUMERO_CREDITOS_CB_buro)), rbind("NUMERO_CREDITOS_CF_buro",t(percentiles(BBDD$NUMERO_CREDITOS_CF_buro))), rbind("NUMERO_OBLIGACIONES_ACTIVAS_buro",t(percentiles(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro)))))
write.table(BBDD_recon,file=paste("SALIDA/Variables_discretas_reconsiderar_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Analizar variables continuas de otra manera

cont_re<-c("CUPOSECTORBANCARIO_SIN_POPULAR_buro","MAXCUPOSECTORBANCASIN_POPULAR_buro","PROMCUPOSECTORBANCASIN_POPULAR_buro","VALOR_UTILISECTORBANCASIN_POPU_buro","TARGET_objetivo")

ANALISIS<-BBDD[cont_re]

cut_CUPOSECTORBANCARIO_SIN_POPULAR_buro<-c(0, 0,2100000,6400000,11300000,19100000,30000000,50400000)
cut_MAXCUPOSECTORBANCASIN_POPULAR_buro<-c(0,2100000,6000000,10174000,16700000,26000000,41127000)
cut_PROMCUPOSECTORBANCASIN_POPULAR_buro<-c(0,2100000,5900000,10000000,15000000,22500000,34800000)
cut_VALOR_UTILISECTORBANCASIN_POPU_buro<-c(0,648000,3686000,7413000,12873000,21794000,39078000)

ANALISIS$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram<-ifelse(ANALISIS$CUPOSECTORBANCARIO_SIN_POPULAR_buro==0,0,1)
ANALISIS$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(ANALISIS$MAXCUPOSECTORBANCASIN_POPULAR_buro==0,0,1)
ANALISIS$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(ANALISIS$PROMCUPOSECTORBANCASIN_POPULAR_buro==0,0,1)
ANALISIS$VALOR_UTILISECTORBANCASIN_POPU_buro_tram<-ifelse(ANALISIS$VALOR_UTILISECTORBANCASIN_POPU_buro==0,0,1)

a1<-table( ANALISIS$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram, ANALISIS$TARGET_objetivo,useNA="ifany")
a2<-table( ANALISIS$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram, ANALISIS$TARGET_objetivo,useNA="ifany")
a3<-table( ANALISIS$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram, ANALISIS$TARGET_objetivo,useNA="ifany")
a4<-table( ANALISIS$VALOR_UTILISECTORBANCASIN_POPU_buro_tram, ANALISIS$TARGET_objetivo,useNA="ifany")

a<-rbind(a1,a2,a3,a4)

write.table(a,file=paste("SALIDA/Variables_continua_reconsiderar_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)



largo_cont<-dim(aux)[2]
print(largo_cont)
loop.vector_cont <- 1:largo_cont
percentiles<-""

for (i in loop.vector_cont){ # Loop over loop.vector
  # store data in column.i as x
  x <- aux[,i]
  nombre<-names(aux)
  pepe<-as.data.frame(ApplyQuintiles(x))
  aux<-as.data.frame(cbind(aux,pepe))
  names(aux)[names(aux) == "ApplyQuintiles(x)"] <- paste0(nombre[i],"_tram")
  print(i)
  assign(paste0("BBDD_continua_biv"), aux)
}


write.table(BBDD_recon,file=paste("SALIDA/Variables_discretas_reconsiderar_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)







#Basura que puede ser util

for (i in loop.vector_tramo){ # Loop over loop.vector
  # store data in column.i as x
  x <- BBDD_discreta2[,i]
  
  # Plot histogram of x
  aux<-  table( x, BBDD_discreta$TARGET_objetivo,useNA="ifany")
  super_frecuencia<-rbind(super_frecuencia,aux)
}

write.table(super_frecuencia,file=paste("SALIDA/Variables_discretas_frecuencia_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)





write.table(t(var_cont_perc),file=paste("SALIDA/percentiles_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)



aggregate(BBDD_continua2, by = list(factor(BBDD_continua$fecha_objetivo)), FUN = function(x) view(dfSummary(x)))

var_discreta_biv<-aggregate(BBDD_discreta2, by = list(factor(BBDD_DISCRETA$fecha_objetivo)), FUN = function(x)  ctable(x, BBDD$TARGET_objetivo, useNA="ifany"))
var_discreta_biv<-aggregate(BBDD_discreta2, BBDD$TARGET_objetivo, by = list(factor(BBDD$fecha_objetivo)), FUN = function(x,y)  table(x, y,  useNA="ifany"))


p<-as.data.frame(aux) 
barplot(t(aux))

write.table(as.data.frame(unlist(aux[1])),file=paste("SALIDA/prueba_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

barplot(t(aux),
        main = "Survival of Each Class",
        xlab = "Class",
        col = c("red","green")
)
legend("topleft",
       c("Not survived","Survived"),
       fill = c("red","green")
)
# Create the loop.vector (all the columns)

largo_cont<-size(BBDD_continua)[2]
loop.vector_cont <- 2:2
 
#Conjunto de datos con predictores que presencia muy significativa de un único valor o "varianza casi cero



for (i in loop.vector_cont){ # Loop over loop.vector
  # store data in column.i as x
  x <- BBDD_continua[,i]
  
  # Plot histogram of x
  hist(x,       main = paste(names(BBDD_continua)[i], i), xlab = "")
  boxplot(x,       main = paste(names(BBDD_continua)[i], i), xlab = "")
  boxplot(x~BBDD_continua$TARGET_objetivo,       main = paste(names(BBDD_continua)[i], i), xlab = "")
  }

boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

var_cont_summary<-aggregate(x, by = list(factor(BBDD$TARGET_objetivo)), FUN = function(x)  hist(x,       main = paste(names(BBDD_continua)[i]), xlab = ""))

x<-BBDD$PLAZO_objetivo


#Preprocesamiento de los datos
#Cargamos la información de Buro
#buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")
#Ind_var_cte_buro<-nearZeroVar(buro, freqCut = 99/9,uniqueCut = 10)
#names(BBDD_continua[,Ind_var_cte_buro]) 

Ind_var_cte<-nearZeroVar(BBDD_continua, freqCut = 99/9,uniqueCut = 10)
write.table(names(BBDD_continua[,Ind_var_cte]),file=paste("SALIDA/Variable_varianza_cero_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)
BBDD_continua2<-BBDD_continua[,-Ind_var_cte]
names(BBDD_continua2)
