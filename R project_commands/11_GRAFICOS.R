attach(grupo)
barplot(table(grupo$resi))
barplot(table(grupo$resi),
main="TITULO RESIDENCIA",
sub="SUBTITULO",
xlab="RESIDENCIA",
ylab="Numero de residentes",
ylim=c(0,6),
col="salmon")
colors()
cuadro3<-table(grupo$resi)
cuadro3
C3porc<-round((cuadro3/margin.table(cuadro3)*100),1)
etiquetas<-c("Rural", "Urbana")
etiquetas<-paste(etiquetas,"%", sep="")
etiquetas
pie(C3porc, labels = etiquetas,clockwise=TRUE,
main="TITULO RESIDENCIA",
sub="SUBTITULO RESIDENCIA", col ="salmon")

#histogramas

hist(grupo$nota, 
main="TITULO RESIDENCIA",
sub="SUBTITULO RESIDENCIA",
xlab="EJEX",
ylab="EJEY",
col="pink2")

#Boxplot
boxplot(grupo$nota,
main="TITULO RESIDENCIA",
sub="SUBTITULO RESIDENCIA",
xlab="EJEX",
ylab="EJEY")

#Diagrama de dispersión
plot(grupo$nota, grupo$edad,
main="TITULO RESIDENCIA",
sub="SUBTITULO RESIDENCIA",
xlab="EJEX",
ylab="EJEY")

scatterplot(grupo$nota, grupo$edad,
ellipse=TRUE,labels,
id.n=2,
main="TITULO RESIDENCIA",
sub="SUBTITULO RESIDENCIA",
xlab="EJEX",
ylab="EJEY",
lwd=1.5,
col="red")

