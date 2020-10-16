#LEEME 
#En teoría sólo hay que actualizar la ruta

setwd("C:/Users/jennyfer.combariza.BFINAMERICA/Desktop/validaciones")
#así como el nombre de los archivos planos a utilizar

mora <- read.delim("Mora_201403.txt", header = TRUE, sep = "\t")
tabla<- read.delim("TABLA_MODELOS.txt", header = TRUE, sep = "\t")
beh_cli <- read.delim("BEH_CLI_201403.txt", header = TRUE, sep = "\t")

#Fin de LEEME


attach(mora)
mora$mora_max2<-ifelse(mora$mora_max > 365, 365, mora$mora_max )

#mora2<-data.matrix(mora)

library(sqldf)
library(tcltk)

base <- sqldf("SELECT *
       FROM mora LEFT JOIN tabla USING(cod_linea)") 

base$clave<-paste(base$GrupoModelo,base$nro_id)

detach(mora)
attach(base)
#cuota<-sqldf('select clave, max(val_cuota) as max_cuota from base group by clave order by clave')

total<-sqldf('select clave, sum(val_cuota) as tot_cuota from base group by clave order by clave')

base2 <- sqldf("SELECT *
       FROM base  LEFT JOIN total USING(clave) order by nro_id")

base3<-sqldf('select *, val_cuota/tot_cuota as pond from base2')
base4<-sqldf('select *, (pond*mora_max2) as mora_max_p from base3')

mora_pond<-sqldf('select clave,  sum(mora_max_p) as mora_pond from base4 group by clave order by clave')
mora_pond$mora_pond2=round(mora_pond$mora_pond,0)

cliente<-sqldf("SELECT *
       FROM cuota  LEFT JOIN mora_pond USING(clave) order by clave")

detach(base)
attach(cliente)
#write.table(cliente, "mora_validada.txt", sep="\t")


cuota2<-sqldf('select nro_id, max(val_cuota) as max_cuota from base group by nro_id order by nro_id')
total2<-sqldf('select nro_id, sum(val_cuota) as tot_cuota from base group by nro_id order by nro_id')

general<-sqldf("SELECT *
       FROM base  LEFT JOIN total2 USING(nro_id) order by nro_id")

general2<-sqldf('select *, val_cuota/tot_cuota as pond from general')
general3<-sqldf('select *, (pond*mora_max2) as mora_max_p from general2')

mora_pond2<-sqldf('select nro_id,  sum(mora_max_p) as mora_pond from general3 group by nro_id order by nro_id')
mora_pond2$mora_pond2=round(mora_pond2$mora_pond,0)

cliente2<-sqldf("SELECT *
       FROM cuota2  LEFT JOIN mora_pond2 USING(nro_id) order by nro_id")

cliente2$clave=paste('GENERAL',cliente2$nro_id,sep=" ")



#Para validar que no hay clave duplicadas
attach(cliente2)
anyDuplicated(nro_id)
#Si hay duplicados y queremos saber cuales son basta teclear dup<-duplicated(cliente)
detach(cliente2)

#validacion

attach(cliente)
aux<-sqldf('select clave, mora_pond2 from cliente')
detach(cliente)
attach(cliente2)
aux2<-sqldf('select clave, mora_pond2 from cliente2')

val<-rbind(aux,aux2)

#importamos el archivo de behave a nivel de cliente
#beh_cli <- read.delim("BEH_CLI_201403.txt", header = TRUE, sep = "\t")
beh_cli$clave=paste(beh_cli$MODELO,beh_cli$ID, sep=" ")

beh_cli2<-sqldf("SELECT *
       FROM beh_cli  LEFT JOIN val USING(clave) order by clave")

a<-sqldf("SELECT  sum(MoraMaxMes-mora_pond2) as resultado from beh_cli2")

if (a$resultado==0){
print("La mora al cierre de mes coincide")
}else if (x>0){
print('Error, revisar la base error en la carpeta asignada')
write.table(beh_cli2, "C:/Users/jennyfer.combariza.BFINAMERICA/Desktop/validaciones/error.txt", sep="\t")
}



