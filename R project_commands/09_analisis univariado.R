#Breve analisis univariado

#Primero creamos la tabla de datos
x<-c("urbana", "rural","rural","rural","urbana","urbana","urbana","urbana","rural")
w<-c("h","m","h", "m","h", "m" ,"h","h","m" )
y<-c(15,16,17,17,18,19,18,16,15)
z<-c(15,15,14,13,17,18,10,17,12)

#Conversion a factores
xf<-factor(x)
wf<-factor(w)

grupo<-data.frame(resi=xf, sexo=wf, edad=y, nota=z)
attach(grupo)
#Tabla de frecuencias
table(nota)
table(edad)
#convertir frecuencia en una tabla
cuadro1<-table(edad)
cuadro1
#frecuencia absoluta acumulada
cumsum(cuadro1)
#frecuencia relativa
cuadro1/margin.table(cuadro1) #margin.table(cuadro1) da el número total de individuos
#redondeo de frecuencia relativa a dos digitos
round(cuadro1/margin.table(cuadro1)*100,2)
#Frecuencia relativa acumulada
cumsum(round(cuadro1/margin.table(cuadro1)*100,2))
#Medidas de tendencia central
mean(grupo$edad) #media
mean(grupo$nota) #media
median(grupo$edad) #mediana
median(grupo$nota) #mediana
quantile(grupo$edad) #cuartiles
quantile(grupo$nota) #cuartiles
#Distribución percentilíca
quantile(grupo$nota,seq(0,1, by=0.1))
#Medidas de dispersión
range(grupo)#solo si todas las varibles son numericas
#Varianza
var(grupo$edad)
var(grupo$nota)
#Desviación estándar
sd(grupo$edad)
sd(grupo$nota)
#medidas de posición
library(e1071)
skewness(grupo$edad)
kurtosis(grupo$nota)
#Resumir medidas estadístias de todas las variables de una base de datos
summary(grupo)
#Resumir medidas estadístias de UNA variable de una base de datos
summary(grupo$nota)
