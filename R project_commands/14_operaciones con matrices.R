A=matrix(c(1,2,3,4),2,2,byrow=TRUE)
A

#trasponer una matriz
B=t(A)
B

#dimension
dim(A)

#Multiplicacion
A%*%A #como A*A de matlab

A*A #como A.^2 de matlab

#Inversa
solve(A)
solve(A)%*%A

#Matriz diagonal
diag(A) #es un vector

diag(diag(A)) #es una matriz

#Crear vector a partir de una matriz
c(A)

#determinante
det(A)

#Autovalores y autovectores
salida<-eigen(A)
salida$values
salida$vectors

#Descomposición espectral
svdres=svd(A)
U=svdres$u
V=svdres$v
D=svdres$d


