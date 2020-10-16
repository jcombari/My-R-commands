#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","xlsx","summarytools")

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()
sub_ruta="./CONTRATOS_OBJETIVO/";
lista_clientes<-list.files(sub_ruta,pattern="contratos_")
ventana_back<-6

for(file in lista_clientes){
  rfile<-read.csv(gsub(" ", "",paste(sub_ruta,file)), header = T,sep=";")
  names(rfile)<-c("ID_CONTRATO","ID_CLIENTE","BRANCH_DEL_CONTRATO","BRANCH_DEL_CLIENTE","FECHA_DE_INICIO","FECHA_DE_FINALIZACION","STATUS_CONTRATO","FECHA_CANCELACION","RAZON_CANCELACION","CANCELACION_TEMPRANA","FECHA_ULTIMA_RENOVACION","CONTRATO_EN_MORA","CONTRATO_EN_DEFAULT","INDICADOR_SISTEMA_ALERTA_TEMPRANA","INDICADOR_DE_BLOQUEO","DIVISA","RAZON_DE_CREDITO","IDIOMA_CONTRATO","TIPO_DE_PRODUCTO","REFINANCIADO","DEUDA_TOTAL_INICIAL","SAL_CAPITA","VLR_CUOTA")
  rfile$fecha<-substr(file,str_length(file)-9, str_length(file)-4 )
  rfile$largo_contrato<-str_length(rfile$ID_CONTRATO)
  rfile$ID_CONTRATO<-as.numeric(rfile$ID_CONTRATO)
  assign(paste0(substr(file,1, str_length(file)-4 )), rfile)  
} 

#load("contratos.Rdata")

#Esto es para validar la calidad del numero de contrato no se debe ejecutar siempre
freq(contratos_201710$largo_contrato)
freq(contratos_201711$largo_contrato)
freq(contratos_201712$largo_contrato)
freq(contratos_201801$largo_contrato)
freq(contratos_201802$largo_contrato)
freq(contratos_201803$largo_contrato)
freq(contratos_201804$largo_contrato)
freq(contratos_201805$largo_contrato)
freq(contratos_201806$largo_contrato)

#save(contratos_201710,contratos_201711,contratos_201712,contratos_201801, contratos_201802, contratos_201803, contratos_201804, contratos_201805,contratos_201806,contratos_201807, contratos_201808, file="contratos.Rdata")
#write.xlsx(unlist(names(contratos_201711)),file=paste0("SALIDA/contratos_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)

#Base de datos de créditos 
base_def<-sqldf("select * from contratos_201711 where TIPO_DE_PRODUCTO in ('Libranzas')")
#freq(base_def$fecha_marca)
#freq(base_def$TIPO_DE_PRODUCTO)
#names(base_def)

#Construcción de la base de datos 
base_aux<-sqldf("select * from contratos_201712 where TIPO_DE_PRODUCTO in ('Libranzas','LB') AND ID_CONTRATO not in (select ID_CONTRATO from  base_def where TIPO_DE_PRODUCTO in ('Libranzas','LB'))  ")
base_def<-rbind(base_aux, base_def)

base_aux<-sqldf("select * from contratos_201801 where TIPO_DE_PRODUCTO in ('Libranzas','LB') AND ID_CONTRATO not in (select ID_CONTRATO from contratos_201712 where TIPO_DE_PRODUCTO in ('Libranzas','LB'))  ")
base_def<-rbind(base_aux, base_def)

base_aux<-sqldf("select * from contratos_201802 where TIPO_DE_PRODUCTO in ('Libranzas','LB') AND ID_CONTRATO not in (select ID_CONTRATO from  base_def where TIPO_DE_PRODUCTO in ('Libranzas','LB'))  ")
base_def<-rbind(base_aux, base_def)

#Distribución de libranzas por año

sqldf("select fecha, TIPO_DE_PRODUCTO, count(TIPO_DE_PRODUCTO) from base_def group by fecha,  TIPO_DE_PRODUCTO order by fecha,  TIPO_DE_PRODUCTO")

#Para saber de esas cuantas se han ido 6 meses despues

lista_ini<-list("201711", "201712", "201801", "201802")
lista_fin<-list()
largo<-length(lista_ini)
for(i in (1:largo)){
  if(substr(lista_ini[i],str_length(lista_ini[i])-1, str_length(lista_ini[i]) ) %in%  c('07','08','09','10','11','12')){
    lista_fin[i]<-as.numeric(as.character(lista_ini[i])) + 88 + ventana_back
  }else{
    lista_fin[i]<- as.numeric(as.character(lista_ini[i])) + ventana_back
  }
}

lista_target<-list()
for(i in (1:largo)){
  query=paste0("select ID_CONTRATO, fecha from contratos_",as.character(lista_ini[i]), " where TIPO_DE_PRODUCTO in ('Libranzas','LB') AND ID_CONTRATO not in (select ID_CONTRATO from  contratos_",as.character(lista_fin[i])," where TIPO_DE_PRODUCTO in ('Libranzas','LB')) ")
  faux<-sqldf(query,verbose=TRUE) 
  lista_target<-rbind(lista_target,faux)
}

target<-as.data.frame(lista_target)
#write.table(as.data.frame(target),file=paste("SALIDA/target_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = F)
target<-sqldf("select ID_CONTRATO, fecha  FROM target order by ID_CONTRATO, fecha")
target<-target  %>% 
  group_by(ID_CONTRATO) %>% 
  filter(row_number()==1)

target$ID_CONTRATO_cruce<-paste0(target$ID_CONTRATO,"_",target$fecha)
target$TARGET_BINARY_TARG<-1

#Cruzamos con la base_def
base_def$ID_CONTRATO_cruce<-paste0(base_def$ID_CONTRATO,"_",base_def$fecha)
base_def<-merge(x=base_def, y=target, by.x="ID_CONTRATO_cruce",  by.y="ID_CONTRATO_cruce",suffixes = c("",".TARGET"),all.x=TRUE)

sqldf("select TARGET_BINARY_TARG, count() from base_def group by TARGET_BINARY_TARG")

sum(!is.na(base_def$TARGET_BINARY_TARG))

#save(base_def, file="base_target.Rdata")
base_def$TARGET_BINARY_TARG[is.na(base_def$TARGET_BINARY_TARG)] <- 0
#save(base_def, file="base_def.Rdat")
#Vamos a identificar la población de 201711 que es nueva
base_0<-sqldf("select ID_CONTRATO from contratos_201711 where TIPO_DE_PRODUCTO in ('Libranzas','LB') AND ID_CONTRATO not in (select ID_CONTRATO from  contratos_201710 where TIPO_DE_PRODUCTO in ('Libranzas','LB'))")
base_0$fecha_marca<-1
base_def<-merge(x=base_def, y=base_0, by.x="ID_CONTRATO", by.y="ID_CONTRATO",suffixes = c("",""), all.x=T)
base_def$fecha_marca[is.na(base_def$fecha_marca)] <- 0
#freq(base_def$fecha_marca)

base_def$fecha<-as.numeric(as.character(base_def$fecha))
base_def$fecha_temporal<-ifelse(base_def$fecha==201711 & base_def$fecha_marca==1, "201711_1" ,base_def$fecha)
freq(base_def$fecha_temporal)

#Porcentaje de los que se fueron
data.frame(table(base_def$TARGET_BINARY_TARG)) %>%
  mutate(Rel_Freq = Freq*100/sum(Freq)) 

gather(base_def, "fecha", "TARGET_BINARY_TARG") %>%
  count(fecha, TARGET_BINARY_TARG) %>%
  group_by(var) %>%             # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))


#Distribución por año opr fecha
distr_fecha<-base_def %>% group_by(fecha,TARGET_BINARY_TARG) %>% tally()

#Distribución por año por fecha discrimando 201711
distr_fecha2<-base_def %>% group_by(fecha_temporal,TARGET_BINARY_TARG) %>% tally()

#write.xlsx(as.data.frame(rbind(distr_fecha,distr_fecha2)),file=paste0("SALIDA/resumen_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)
#Analisis de perimetro

sqldf("select ")

#sqldf("select fecha, count() from base_def group by fecha")
#sqldf("select fecha, count() from target group by fecha")

#Cruce con super base
load("superbase.RData")
load("tabla_mes.Rdata")
 
super_super$fecha2<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-4 )
super_super$anio<-substr(super_super$fecha, nchar(super_super$fecha)-5, nchar(super_super$fecha)-4 )
super_super$mes<-substr(super_super$fecha, nchar(super_super$fecha)-8, nchar(super_super$fecha)-6 )
super_super<-merge(x=super_super, y=tabla_mes, by.x="mes", by.y="mes1",suffixes = c("",""))

super_super$FECDES2<-as.numeric(as.character(paste0(substr(super_super$FECDES,1, 4 ),substr(super_super$FECDES,6, 7 ))))
super_super$FECVEN2<-as.numeric(as.character(paste0(substr(super_super$FECVEN,1, 4 ),substr(super_super$FECVEN,6, 7 ))))
super_super$anio<-as.numeric(as.character(paste0("20",super_super$anio)))
super_super$mes2<-paste0("0",super_super$mes2)
super_super$mes2<-substr(super_super$mes2,str_length(super_super$mes2)-1,str_length(super_super$mes2))
super_super$fecha3<-as.numeric(as.character(paste0(super_super$anio, super_super$mes2)))
super_super$clave<-paste0(super_super$OBLIGACIONENC,"_",super_super$fecha3)

base_def$clave<-paste0(base_def$ID_CONTRATO,"_", base_def$fecha)

freq(base_def$fecha)
freq(super_super$fecha3)

#base_def2<-merge(x=base_def,y=super_super, by.x="ID_CONTRATO", by.y = "OBLIGACIONENC",suffixes = c("",".super"))

#Analisis de perimetro
names(base_def2)

write.xlsx(as.data.frame(sqldf("select  fecha_temporal,TARGET_BINARY_TARG , sum(SAL_CAPITA) from base_def2 group by fecha_temporal,TARGET_BINARY_TARG order by fecha_temporal,TARGET_BINARY_TARG")
),file=paste0("SALIDA/resumen_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)

base_def2<-merge(x=base_def,y=super_super, by.x="clave", by.y = "clave",  all.x=TRUE, suffixes = c("",".super"))

library(data.table)
base_def2<-unique(setDT(base_def2)[order(fecha3)], by = "clave")

#Seleccionamos libranzas con fecha de vencimiento superior a 201808
base_def2<-sqldf("select * from base_def2 where FECVEN2>201808")
base_def_fecha_vencimiento<-sqldf("select FECVEN2, count(FECVEN2) from base_def2 group by FECVEN2 order by FECVEN2")
base_def2$aux<-as.numeric(as.character(substr(base_def2$fecha, nchar(base_def2$fecha)-1, nchar(base_def2$fecha) )))

write.xlsx(base_def_fecha_vencimiento,file=paste0("SALIDA/base_def_fecha_vencimiento_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)

if(any(base_def2$aux>6)){
  base_def2$fecha_cosecha<-as.numeric(as.character(base_def2$fecha))+94
}else{
  base_def2$fecha_cosecha<-as.numeric(as.character(base_def2$fecha))+6
}

#Verificacion de bases para cosechas
sqldf("select fecha, fecha_cosecha, count(fecha_cosecha)  from base_def2 group by fecha, fecha_cosecha order by fecha, fecha_cosecha")

#Incluyendo información de buro
load("buro.RData")
Buro$FECHA_DATA2<-paste0(substr(Buro$FECHA_DATA,1, 4 ),substr(Buro$FECHA_DATA,6, 7 ))
#sqldf("select distinct(FECHA_DATA2) from Buro")
Buro$clave_cliente<-paste0(Buro$CEDULAENC,"_",Buro$FECHA_DATA2)
base_def2$clave_cliente<-paste0(base_def2$ID_CLIENTE,"_",base_def2$fecha)

base_def3<-merge(x=base_def2,y=Buro, by.x="clave_cliente", by.y = "clave_cliente",  all.x=TRUE, suffixes = c("",".buro"))

base_def3<-unique(setDT(base_def3)[order(fecha3)], by = "clave")

write.xlsx(freq(base_def3$TOTAL_CONSULTAS_ULT_6_MESES,na.rm = FALSE),file=paste0("SALIDA/base_def3_TOTAL_CONSULTAS_ULT_6_MESES_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)

names(base_def3)

cualitativo<-base_def3[,c("RANGO_APROXIMADO_EDAD","GENERO","CIUDAD_DE_EXPEDICION","RESPUESTA","NUMERO_OBLIGACIONES_ACTIVAS","NUMERO_CREDITOS_CB","VALOR_MORA_CB","NUMERO_CREDITOS_CV","NUMERO_TDC","NUMERO_CREDITOS_SR","NUMERO_CELULARES_TELCOS","TOTAL_CONSULTAS_ULT_6_MESES","ESTADO_CONSULTA","EXPERIENCIA_FINANCIERA")]

freq_tables <- lapply(cualitativo, freq)

#view(freq_tables,  file = 'freq-tables_cualitativo.html')
#write.table(do.call(rbind, freq_tables), quote = FALSE, row.names = FALSE, col.names = FALSE)



#write.xlsx( ,file=paste0("SALIDA/base_def3_frecuencia_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE, col.names = FALSE)
#write.xlsx(freq_tables, file = "File.xlsx", sheetName = "sheet1", row.names = FALSE)

#options(java.parameters = "-Xmx2048m")
#library(rJava)
#https://www.biostars.org/p/219979/

#for (i in c(1:length(freq_tables))){
#   write.xlsx(freq_tables[i], file="test.xlsx", sheetName=paste(i), append=T)
#}

#install.packages("erer")
#library("erer")

view(dfSummary(cualitativo))
capture.output(freq_tables, file=paste("SALIDA/cualitativo_",format(today, format="%Y%m%d"),".txt", sep=""))


load("buro.Rdata")
names(Buro)