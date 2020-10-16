datos <- read.delim("C:/Users/jennyfer.combariza.BFINAMERICA/Desktop/SARC_GESTION/ENTRADA_BEHAVESCORE/Exo_Catego_201408.txt", header = TRUE, sep = "\t")
attach(datos)
library(sqldf)
library(tcltk)
sqldf('select cod_activ_experto2 from datos group by cod_activ_experto2') 

sqldf('select genero from datos group by genero') 


datos[is.na(cod_activ_experto2)] <- "NaN"
