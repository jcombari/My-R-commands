
#creación de un marco de datos (data.frame)

x<-c("urbana", "rural","rural","rural","urbana","urbana","urbana","urbana","rural")
w<-c("h","m","h", "m","h", "m" ,"h","h","m" )
y<-c(15,16,17,17,18,19,18,16,15)
z<-c(15,15,14,13,17,18,10,17,12)

#Conversion a factores
xf<-factor(x)
wf<-factor(w)

grupo<-data.frame(resi=xf, sexo=wf, edad=y, nota=z)
#cargar paquete car
library(car)
#Forzar orden de variables en la tabla

grupo$nota2<-Recode(grupo$nota,
'1:10=1;
1:15=2; 
16:20=3', 
as.factor.result=TRUE)

grupo$nota3<-Recode(grupo$nota,
'1:10="REPROBADO";
1:15="APROBADO_BAJA"; 
16:20="APROBADO_ALTA"', 
as.factor.result=TRUE, levels=c("REPROBADO","APROBADO_BAJA","APROBADO_ALTA"))

attach(grupo)
table(nota)
table(nota3,edad)

#Tablas cruzadas con porcentajes en columnas
library(abind, Rcmdr)
colPercents(table(resi,sexo))
rowPercents(table(resi,sexo))
totPercents(table(resi,sexo))

#tablas cruzadas con porcentajas en columnas
colPercents(table(resi,sexo,nota))

#test de independencia chicuadrado
chisq.test(table(edad,sexo))
prueba<-chisq.test(table(edad,sexo))
prueba
names(prueba)
prueba$expected
table(edad,sexo)
round(prueba$expected,0)

table(resi,nota3)
colPercents(table(resi,nota))
prueba2<-chisq.test(table(sexo,nota3))
round(prueba2$expected,0)

#Prueba t para muestras independientes









