seguimiento <- read.delim("C:/Jennyfer/01 Modelo de seguimiento y calificación para las cartera de microcrédito y comercial junio 2014/PRUEBAS/Base Visitas DIC-2014_PLANTILLA_CEDULA.txt", header = TRUE)
vigente <- read.delim("C:/Back Up Estadístico/Back Up Diana Sofía Orbes/02 Bases de Datos/Bases cartera originales/Cartera  Vigente 01-11-2014.txt", header = TRUE)
lineas <- read.delim("C:/Jennyfer/18_tablas de parametros/lineas.txt", header = TRUE)
oficinas <- read.delim("C:/Jennyfer/18_tablas de parametros/oficinas.txt", header = TRUE)

#cruces
library(sqldf)
library(tcltk)
#install.packages(tcltk)
saldo_total=sqldf("select nui, sum(capital) from vigente group by nui")
dim(saldo_total)
attach(vigente)

if(calificacion == ' A'){ 
 	print(1)
  } 
else {print(2)}

if(calificacion==' A') {
    print(1) 
} else { 
    print(2) 
}

if (calificacion==' B'){print(2)
  } else if(calificacion==' C'){print(3)
  }
}

vigente2<- vigente[order(calificacion,-capital,-dias_de_mora),]
base0=sqldf("select a.*, b.linea, b.nombre, b.fecha_vcto 


detach(saldo_total)