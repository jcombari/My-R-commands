#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("dplyr","tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#save(SUPERBASE_201711, SUPERBASE_201712, SUPERBASE_201801, SUPERBASE_201802, SUPERBASE_201803, SUPERBASE_201804,SUPERBASE_201805, SUPERBASE_201806, SUPERBASE_201807, SUPERBASE_201808, super_super, file="objetivo.Rdata")
load("objetivo.Rdata")

#Seleccionamos la fecha para las cuales vamos a realizar 
OBJETIVO<-sqldf("select * from super_super where fecha>201806")

# > names(OBJETIVO)
# [1] "CEDULAENC.super"       "fecha"                
# [3] "OBLIGACIONENC"         "CALIF_CART"           
# [5] "COD_OFIC"              "FECDES"               
# [7] "FECVEN"                "PLAZO"                
# [9] "TASINT_CTE"            "TASINT_MOR"           
# [11] "VLR_DESEMB"            "SAL_CAPITA"           
# [13] "VLR_CUOTA"             "VLR_MORA"             
# [15] "DIASMORA_I"            "CUOTA_PAGA"           
# [17] "COD_PAG"               "FECDES2"              
# [19] "FECVEN2"               "segmentacion"         
# [21] "OBLIGACIONENC2"        "MOD_DEF"              
# [23] "FECDES_NF"             "MARCA"                
# [25] "COD_PAGADURIA"         "COD_SUCURSAL"         
# [27] "COD_OFIADMIN"          "FEC_PAGO"             
# [29] "VLR_PAGO"              "VLR_SALDOOBLIG"       
# [31] "COD_OFIPAGO"           "NOM_PAGADURIA"        
# [33] "COD_SECTOR"            "NOM_SECTOR"           
# [35] "COD_SUBSECTOR"         "NOM_SUBSECTOR"        
# [37] "COD_OFICINA"           "NOM_SECTORANT"        
# [39] "NOM_UNIDAD"            "CEDULAENCprepago"     
# [41] "FEC_PAGO2"             "FEC_PAGO_marca"       
# [43] "TIPO_DE_PAGO"          "CUOTA_PAGA2"          
# [45] "CUOTA_PENDIENTE"       "CUOTA_P_MARCA"        
# [47] "marca_prepago"         "lista_fin"            
# [49] "MARCA_PREPAGO_RANGO"   "MARCA_VENCIMIENTO"    
# [51] "MARCA_PREPAGO_RANGO2"  "FEC_PAGO_SINIESTRO"   
# [53] "MARCA_SINIESTRO_RANGO" "MARCA_PREPAGO_RANGO3" 
# [55] "MARCA_PREPAGO_RANGO4"  "TARGET"      

load(file="buro.Rdata")

buro$clave<-paste0(buro$CEDULAENC,"_",buro$FECHA)
buro<-buro[order(buro$CEDULAENC, -buro$FECHA),] #ordeno por clave=cedula+fecha
buro<-buro[!duplicated(buro$clave), ] #luego de ordenamos, elimino dupicados
#Como 201806 201808 viene el quanto con valores ceros se reemplaza por el quanto_mod
buro$QUANTO2<-ifelse(buro$FECHA==201806 | buro$FECHA==201808 ,buro$QUANTO_MOD,buro$QUANTO)
names(OBJETIVO)<-gsub("\\.super", "",names(OBJETIVO))

#El dinero nace digital pero se vuelve fisico

#********************************************************
#Convertir Poblacion objetivo de credito a cliente
#********************************************************

TEMPORAL<-OBJETIVO
