datos <- read.delim("C:/Jennyfer/DIEGO/SOLICITUDES AJUSTADO_REVISAR.txt", header = TRUE, sep = "\t")
head(datos)
names(datos)
attach(datos)
datos1<-table(ciudad_nacimiento)


library(xlsx)
write.xlsx(datos1, "c:/Jennyfer/Diego/ciudades.xlsx")


grupo$nueva5<-Recode(grupo$nota,
'1:10="REPROBADO";
1:15="APROBADO_BAJA"; 
16:20="APROBADO_ALTA"', 
as.factor.result=TRUE, levels=c("REPROBADO","APROBADO_BAJA","APROBADO_ALTA"))

datos$ciudad_nacimiento2<-Recode(datos$ciudad, 
' 

