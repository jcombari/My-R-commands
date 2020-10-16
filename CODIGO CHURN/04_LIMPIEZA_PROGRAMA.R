#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificaci??n de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","corrplot","ROSE","DMwR","lubridate")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

load(file= "base_objetivo_fid.RData")

churn<-BBDD3
input.list<-names(churn)
input.list<-input.list[grep("*buro*", input.list)]
var_buro_eliminar<-paste0(c("CUOTAS_CALCULADAS_TDC","CUOTAS_CALCULADAS_CRÉDITOS","CUOTAS_CALCULADAS_HIPOTECARIAS","PLAZO_CREDITO","PLAZO_HIPOTECARIO","TASA_HIPOTECARIO","TASA_CREDITOS","PLAZO_TDC","CUPO_SUGERIDO_1","CUPO_SUGERIDO_2","CUPO_SUGERIDO_3","CUPO_SEGUN_MERCADO","CUPO_AJUSTADI","CUPO_EXP_FIN","CUPO_FINAL","EXPERIENCIA_FINANCIERA","PORCEN_DE_CUOTAS_VS_INGRESO","RESPUESTA","ESTADO_CONSULTA","RANGO_0","RANGO_1","RANGO_2","RANGO_3","RANGO_4","RANGO_5","RANGO_6"),"_buro")
input.list_fec1<-input.list[grep("*FECH*", input.list)]
input.list_fec2<-input.list[grep("*fec*", input.list)]
var_no_necesaria<-c("CEDULAENC_buro","TIPO_ID_buro", "QUANTO_MOD_buro","QUANTO_buro")
var_buro_eliminar2<-c(var_buro_eliminar,input.list_fec1,input.list_fec2,var_no_necesaria)

#Base_BURO
buro<-BBDD3[input.list]
buro<-buro[ , !(names(buro) %in% var_buro_eliminar2)]
nums<-sapply(buro, is.numeric)
buro<-buro[,nums]

#Tratamiento de puntos extremos archivo Buro
variables<-names(buro)
largo<-length(variables)

extremo_buro<-""
for(i in variables){
    x <-  as.numeric(buro[[i]])
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    lim_inf<-as.numeric((qnt[1] - H))
    lim_sup<-as.numeric((qnt[2] + H))
    #x2<-ifelse(x>(qnt[2] + H),caps[2], ifelse(x<(qnt[1] - H),caps[1], x))
    #boxplot(x)
    #boxplot(x2)
    aux<-cbind(i,lim_inf,lim_sup)
    extremo_buro<-rbind(extremo_buro,aux)
    boxplot(BBDD3[[i]], main=paste(i," antes"))
    BBDD3[[i]]<-ifelse(x>(qnt[2] + H),caps[2], ifelse(x<(qnt[1] - H),caps[1], x))
    boxplot(BBDD3[[i]], main=paste(i," despues"))
    print(i)
}

write.xlsx(extremo_buro,file=paste0("SALIDA/04_extremo_buro",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

input.list<-names(BBDD3)
input.list<-input.list[grep("*_objetivo*", input.list)]

save(BBDD2, BBDD3,file = "base_objetivo_fid2.RData")
