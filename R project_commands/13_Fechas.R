dayvar=as.Date("2010-04-29")
dayvar
fecha_hoy=as.Date(Sys.time())
fecha_hoy

dia=weekdays(dayvar)
dia
mes=months(dayvar)
mes
ano=substr(as.POSIXct(dayvar),1,4)
ano
trimestre=quarters(dayvar)
trimestre

#PAra suspender ejecución unos minutos
Sys.sleep(1)
shell("dir")

#Derivada
D(expression(x^3), "x")