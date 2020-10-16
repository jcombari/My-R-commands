#eliminar una variable 
archivo$varible<-NULL
#renombrar
names(archivo)[c(1)]<-c("residencia")
names(archivo)[archivo.old_name]<-c("new_name")
#Crear variables a partir de otras
grupo$nueva<-with(grupo,log(nota))
grupo$nueva2<-with(grupo, sqrt(nota))
grupo$nueva3<-with(grupo, round(sqrt(nota),3))
grupo