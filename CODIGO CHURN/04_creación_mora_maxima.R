#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","openxlsx", "summarytools", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Mora máxima credito-cliente
#fecha_mora<-sub("-","", format(seq(as.Date("2017/1/1"), as.Date("2018/8/1"), "month"), "%Y-%m"))
fecha_mora<-sub("-","", format(seq(as.Date("2017/1/1"), as.Date("2019/1/1"), "month"), "%Y-%m"))

#fecha_mora<-sub("-","", format(seq(as.Date("2018/11/1"), as.Date("2018/12/1"), "month"), "%Y-%m"))

for(mora in fecha_mora){
  lista_mora<-list.files("./mora",pattern=mora)
  resultado_mora<-list()
  super_mora=list() 
  credito=list() 
  cliente=list() 
  contrato<-list.files("./CONTRATOS",pattern=mora)
  for(file in lista_mora){
    rfile=list()
    L = ""
    L = character(0)
    L <- readLines(gsub(" ", "",paste("mora/",file)), n = 1)
    if (grepl(";", L)){
      rfile<-read.csv(gsub(" ", "",paste("mora/",file)), header = T,sep=";")
    } else{
      rfile<-read.csv(gsub(" ", "",paste("mora/",file)), header = T,sep=",")
    }
    L2 <- readLines(gsub(" ", "",paste("CONTRATOS/",contrato)), n = 1)
    if (grepl(";", L2)){
      rfile2<-read.csv(gsub(" ", "",paste("CONTRATOS/",contrato)), header = T,sep=";")
    } else{
      rfile2<-read.csv(gsub(" ", "",paste("CONTRATOS/",contrato)), header = T,sep=",")
    }
    names(rfile2)<-c("ID_CONTRATO","ID_CLIENTE","BRANCH_DEL_CONTRATO","BRANCH_DEL_CLIENTE","FECHA_DE_INICIO","FECHA_DE_FINALIZACION","STATUS_CONTRATO","FECHA_CANCELACION","RAZON_CANCELACION","CANCELACION_TEMPRANA","FECHA_ULTIMA_RENOVACION","CONTRATO_EN_MORA","CONTRATO_EN_DEFAULT","INDICADOR_SISTEMA_ALERTA_TEMPRANA","INDICADOR_DE_BLOQUEO","DIVISA","RAZON_DE_CREDITO","IDIOMA_CONTRATO","TIPO_DE_PRODUCTO","REFINANCIADO","DEUDA_TOTAL_INICIAL","SAL_CAPITA","VLR_CUOTA")
    rfile2<-sqldf("select ID_CONTRATO, ID_CLIENTE from rfile2")
    
    if("VLR_CUOTA" %in% names(rfile)){
      rfile <- rfile[, ! names(rfile) %in% c("VLR_CUOTA"), drop = F]
    }
    rfile$fecha<-substr(file,str_length(file)-11, str_length(file)-4 )
    names(rfile)<-c("FECHAPROC","ID_CONTRATO","FECHA_PAGO","DEUDA_INICIAL","EN_MORA","DIAS","ESTADO_PAGO","ESTRATEGIA_RECOBRO","RIESGO","DIVISA","SALDO_CAPITAL","SALDO_INTERESES_CAPITAL","SALDO_INTERESES_MORA","LINEACREDITO","LINEA","fecha")
    rfile_id<-merge(x=rfile, y=rfile2, by.x="ID_CONTRATO", by.y="ID_CONTRATO", x.all=TRUE)
    super_mora<-rbind(super_mora,rfile_id)
    print(file)
  }
   credito<-sqldf("select ID_CLIENTE, ID_CONTRATO, max(DIAS) mora_max, FECHAPROC from super_mora group by ID_CONTRATO")
   cliente<-sqldf("select ID_CLIENTE,  max(DIAS) mora_max, FECHAPROC from super_mora group by ID_CLIENTE")   
   assign(paste0("mora_max_credito_",mora), credito)
   #write.csv2(credito, file = paste0("mora_max/","mora_max_credito_",mora,".csv"),row.names=FALSE,sep = ";", quote = FALSE)
   write.table(credito, file = paste0("mora_max/","mora_max_credito_",mora,".csv"),row.names=FALSE,sep = ";", quote = FALSE, dec=".")
   assign(paste0("mora_max_cliente_",mora), cliente)
   write.table(cliente, file = paste0("mora_max/","mora_max_cliente_",mora,".csv"),row.names=FALSE,sep = ";", quote = FALSE, dec=".")
   print(mora)
} 


seqDays<-function(from, to, by = 1){
  fromDate <- as.Date(as.character(from), "%Y%m%d")
  toDate <- as.Date(as.character(to), "%Y%m%d")
  s <- seq(fromDate, toDate, "days")
  newDates <- str_replace_all(s, "-", "")
  return(newDates)
}

