#Creamos tabla1
x=c(1,2,3)
y=c(0,1,0)
z1=c(5000,2000,3000)
z2=c(5500,2200,2000)
z3=c(6000,3300,1000)
tabla1=data.frame(id=x, female=y,
inc80=z1,
inc81=z2,
inc82=z3)
tabla1
#Creamos tabla 2
x2=c(1,1,1,2,2,2,3,3,3)
e2=c(80,81,82,80,81,82,80,81,82)
y2=c(0,0,0,1,1,1,0,0,0)
z12=c(5000,5500,6000,2000,2000,3300,3000,2000,1000)
tabla2=data.frame(id=x2,year=e2, female=y2,
inc=z12)
tabla2
#Merge
new=merge(tabla1, tabla2)
new2=merge(tabla2, tabla1)
new=merge(tabla1, tabla2, by="id")
new2=merge(tabla1, tabla2, by="id", all=TRUE)
new2