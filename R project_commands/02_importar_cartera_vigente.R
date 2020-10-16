vigente <- read.delim("C:/Back Up Estadístico/Back Up Diana Sofía Orbes/02 Bases de Datos/Bases cartera originales/14.12 CARTERA 31 DICIEMBRE 2014    DEF.txt", header = TRUE)

head(vigente)
names(vigente)
attach(vigente)
#indicador de la cartera vencida de riesgo 
library(sqldf)
library(tcltk)
 
vigente$capital2=as.numeric(vigente$capital)
vigente$capital3=as.numeric(vigente$capital)*1
vigente$capital4=as.numeric(gsub("[,]","",vigente$capital))


aux=sqldf("select calificacion, sum(capital) as capital  from vigente group by calificacion")
aux2=sqldf("select sum(capital) from vigente")
aux3=sqldf("select sum(capital)   from vigente where calificacion in (' B',' C',' D',' E')")

ICV=(as.numeric(aux3)/as.numeric(aux2))*100

#indicador de la cartera vencida por  

ind2=sqldf("select sum(capital) from vigente where dias_de_mora>30")
ICV2=(as.numeric(ind2)/as.numeric(aux2))*100

 A	5.57347E+11
 B	11642831600
 C	7097516350
 D	9312247826
 E	30387537900
