#Conocer si dos archivos tienen nombre de columnas diferentes
aux<- names(buro)!=names(rfile)
aux<-buro[aux==TRUE]
names(aux)
