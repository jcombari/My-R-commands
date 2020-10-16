#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
sub_ruta="/huellas_consulta/"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cargamos la información de Buro
buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")

write.table(summary.default(buro),file=paste("SALIDA/buro_descn_variables_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

#Arreglamos fecha en el archivo buro
freq(buro$FECHA_DATA)
buro$FECHA<-as.numeric(paste0(str_sub(buro$FECHA_DATA,1,4),str_sub(buro$FECHA_DATA,6,7)))
buro$FECHA_ENVIO2<-as.numeric(paste0(str_sub(buro$FECHA_ENVIO,1,4),str_sub(buro$FECHA_ENVIO,6,7)))
freq(buro$FECHA) 
freq(buro$FECHA_ENVIO2)
buro$clave<-paste0(buro$CEDULAENC,buro$FECHA)
names(buro)<-gsub(" ", "",names(buro))
write.xlsx(unlist(names(buro)),file=paste0("SALIDA/buro_nombre_variables_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

#Como 201806 201808 vienen el quanto con valores ceros se reemplaza por el quanto_mod
buro$QUANTO2<-ifelse(buro$FECHA==201806 | buro$FECHA==201808 ,buro$QUANTO_MOD,buro$QUANTO)
save(buro, file = "buro.RData")

#Distribución percentílica
#Ävar_cont_perc<-aggregate(buro$QUANTO2, by = list(factor(buro$FECHA)), FUN = function(x) quantile(x, probs = seq(0,1, 0.05),na.rm = TRUE ))
#write.table(t(var_cont_perc),file=paste("SALIDA/percentiles_quanto2_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T,col.names = T)

write.xlsx(unlist(names(buro)),file=paste0("SALIDA/buro_nombre_variables_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)

