#importar cvs
vigente=read.table(file="C:/Jennyfer/00_R/archivos/14.07 CARTERA 31 JULIO 2014     DEF.csv",header=TRUE,sep=",")
#delimitado por tabulacionaes
vigente2=read.table(file="C:/Jennyfer/00_R/archivos/14.07 CARTERA 31 JULIO 2014     DEF.csv",header=TRUE,sep="\t")
#seleccionar el archivo de una carpeta
vigente3=read.table(file.choose(), header=TRUE, sep=",")
#leer un segmento de excel
vigente4=read.table("clipboard")

