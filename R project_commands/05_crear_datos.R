#crear una matriz
m<-matrix(c(1,6,11,16, 2,7,12,17, 3,8,13,18, 4,9,14,19, 5,10,15,20), nrow=5, ncol=4)
#crear un data frame
grupo1<-data.frame(resi=c("urbana", "rural","rural","rural","urbana","urbana","urbana","urbana","rural"),
sexo=c("h","m","h", "m","h", "m" ,"h","h","m" ),
edad=c(15,16,17,17,18,19,18,16,15),
nota=c(15,15,14,13,17,18,10,17,12))
#información de los datos
mean(grupo1$edad)
attach(grupo1) # Para vincular marco de datos y no tener que escribir nombre_tabla$nombre_variable
mean(edad)
mean(nota)
table(resi)
table(sexo)
detach(grupo1) # Para descinvular marco de datos

#creación de vectores
x<-1:4 #también es valido escribir 1:4 ->x
x<-seq(0,10, by=2)
x<-seq(0,10, length=6)
x<-rep(1:2,3)
x
x<-c(5,7,1,0)

#ojo para colocar valores perdidos se utiliza NA
z<-c(7,9,5,8,9,3,NA,4)
z
#valores lógicos
z<-c(7,9,5,8,9,3,4)
z
w<-z<8
w
#valores lógicos con missing
z<-c(7,9,5,8,9,3,NA,4)
z
w<-z<8
w
#creación de un caracter
x<-rep("x",5)
x
#funcion combinar
y<-c("costa", "sierra", "selva")
y
#combinar y replicar
z<-rep(c("x","y"),4)
z
#creacion factor
x<-c(1,2,1,2,2,2,1,1)
x
xf<-factor(x)
xf
table(xf)
y<-c("hombre", "mujer" ,"hombre","mujer", "mujer", "mujer","hombre","hombre")
y
yf<-factor(y)
yf
table(yf)

#creación de una matriz
m1<-matrix(1:20, nrow=5)
m1
dim(m1)

#creación de un marco de datos (data.frame)

x<-c("urbana", "rural","rural","rural","urbana","urbana","urbana","urbana","rural")
w<-c("h","m","h", "m","h", "m" ,"h","h","m" )
y<-c(15,16,17,17,18,19,18,16,15)
z<-c(15,15,14,13,17,18,10,17,12)

#Conversion a factores
xf<-factor(x)
wf<-factor(w)

grupo1<-data.frame(resi=xf, sexo=wf, edad=y, nota=z)
grupo1

#Ayuda
?colors
??colour #si no estamos seguro como se escribe

example(mean)

