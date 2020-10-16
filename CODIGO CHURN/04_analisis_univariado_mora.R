#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
sub_ruta="/huellas_consulta/"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

#Cargamos la base a analizar
load(file="objetivo_buro.Rdata")
BBDD<-OBJETIVO_BURO

#Vamos a separar las bases para poder construir las variabels sinteticas de mora
BBDD<-BBDD[c("fecha_objetivo", "CEDULAENC_objetivo", "OBLIGACIONENC_objetivo", "TARGET_objetivo")]

fecha<-unique(BBDD$fecha_objetivo)

super_bbdd_mora<-list()


for (i in 1:length(fecha)){ 
  aux <- BBDD[BBDD$fecha_objetivo==fecha[i],]
  fecha10<-sub("-","", format(seq(as.Date(paste0(fecha[i],"01"), "%Y%m%d"), length = 10, by = "-1 months"), "%Y-%m"))
  super_mora<-list()
  aux10<-list()
  aux_actual<-list()
  aux_1ant<-list()
  aux_3ant<-list()
  aux_6ant<-list()  
  for(valor in fecha10){
      rfile=list()
      L = ""
      L = character(0)
      L <- readLines(gsub(" ", "",paste("mora_max/mora_max_credito_",valor,".csv")), n = 1)
      if (grepl(";", L)){
        rfile<-read.csv(gsub(" ", "",paste("mora_max/mora_max_credito_",valor,".csv")), header = T,sep=";")
      } else{
        rfile<-read.csv(gsub(" ", "",paste("mora_max/mora_max_credito_",valor,".csv")), header = T,sep=",")
      }
      rfile$FECHAPROC<-factor(rfile$FECHAPROC)
      rfile$mora_max<-as.numeric(rfile$mora_max)
      rfile$fecha2<-as.numeric(valor)
      super_mora<-rbind(super_mora,rfile)
      print(valor)
      
      
      
      
  }

  contrato10<-sqldf("select ID_CONTRATO, max(mora_max)mora_max_10ant, fecha2 fecha_mora_max_10ant from super_mora group by ID_CONTRATO")
  aux10<-merge(x=aux,  y=contrato10, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )

  #freq(contrato10$fecha2)
  query=paste0("select  ID_CONTRATO, max(mora_max)mora_max_actual, fecha2 fecha_mora_max_actual from  super_mora where fecha2=",as.numeric(fecha10[0+1])," group by ID_CONTRATO")
  contrato_mes_actual<-sqldf(query,verbose=TRUE)      
  aux_actual<-merge(x=aux10,  y=contrato_mes_actual, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )  

  #freq(contrato1$fecha2)
  query=paste0("select  ID_CONTRATO, max(mora_max)mora_max_1ant, fecha2 fecha_mora_max_1ant from  super_mora where fecha2=",as.numeric(fecha10[1+1])," group by ID_CONTRATO")
  contrato1<-sqldf(query,verbose=TRUE)      
  aux_1ant<-merge(x=aux_actual,  y=contrato1, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )  

  query=paste0("select  ID_CONTRATO, max(mora_max)mora_max_3ant, fecha2 fecha_mora_max_3ant from  super_mora where fecha2  between ",as.numeric(fecha10[3+1])," and ",as.numeric(fecha10[1+1])," group by ID_CONTRATO")
  contrato3<-sqldf(query,verbose=TRUE)  
  aux_3ant<-merge(x=aux_1ant,  y=contrato3, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )  

  #freq(contrato3$fecha2)
  query=paste0("select  ID_CONTRATO, max(mora_max)mora_max_6ant, fecha2 fecha_mora_max_6ant from  super_mora where fecha2  between ",as.numeric(fecha10[6+1])," and ",as.numeric(fecha10[1+1])," group by ID_CONTRATO")
  contrato6<-sqldf(query,verbose=TRUE)  
  aux_6ant<-merge(x=aux_3ant,  y=contrato6, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )  
  aux_6ant$mora_max_10ant[is.na(aux_6ant$mora_max_10ant)]<-0  
  aux_6ant$mora_max_1ant[is.na(aux_6ant$mora_max_1ant)]<-0
  #freq(contrato6$fecha2)
  assign(paste0("BBDD_mora_",fecha[i]), aux_6ant)
  super_bbdd_mora<-rbind(super_bbdd_mora,aux_6ant)
  
} 
  
save(BBDD_mora_201711,BBDD_mora_201712, BBDD_mora_201801,BBDD_mora_201802,BBDD_mora_201803, BBDD_mora_201804, BBDD_mora_201805, BBDD_mora_201806, super_bbdd_mora, file="BBDD_mora.Rdata")  

#Trameado
ApplyQuintiles <- function(x) {
  if(length(unique(quantile(x, setdiff(seq(0,1, by=0.05),seq(0,1)) ,na.rm = TRUE)))<=1 ){
    "SIN_DATO"
  }else{
    cut(x, breaks=c(unique(quantile(x, probs = seq(0,1, by=0.05),na.rm = TRUE))),include.lowest=TRUE,dig.lab=10)
  }
}

percentiles <- function(x) {
  quantile(x, probs = setdiff(seq(0,1, by=0.05),seq(0,1)),na.rm = TRUE)
}

BBDD_continua<-super_bbdd_mora[c("mora_max_10ant","mora_max_actual","mora_max_1ant", "mora_max_3ant","mora_max_6ant")]

aux<-BBDD_continua
col_numeric <- which( sapply(aux, is.numeric ) ) 

largo_cont<-dim(aux)[2]
print(largo_cont)
loop.vector_cont <- 1:largo_cont
percentiles<-""
aux2<-aux

for (i in loop.vector_cont){ # Loop over loop.vector
  # store data in column.i as x
  x <- aux[,i]
  nombre<-names(aux)
  pepe<-as.data.frame(ApplyQuintiles(x))
  aux2<-as.data.frame(cbind(aux2,pepe))
  names(aux2)[names(aux2) == "ApplyQuintiles(x)"] <- paste0(nombre[i],"_tram")
  print(i)
  assign(paste0("BBDD_continua_biv"), aux2)
}


#BBDD_continua_biv$mora_max_10ant_tram<-ifelse(BBDD_continua_biv$mora_max_10ant==1,"1",ifelse(BBDD_continua_biv$mora_max_10ant==2,"2",BBDD_continua_biv$mora_max_10ant_tram))
#freq(BBDD_continua_biv$mora_max_10ant_tram)

aux<-BBDD_continua_biv

aux0<-sqldf("select TARGET_objetivo from super_bbdd_mora  ")
input.list<-names(aux)
input.list<-input.list[grep("*_tram*", input.list)]
aux<-aux[,input.list]
largo_tramo<-size(aux)[2]
loop.vector_tramo <- 2:largo_tramo

super_tramo=list()

for (i in loop.vector_tramo){ # Loop over loop.vector
  # store data in column.i as x
  x <- aux[,i]
  
  # table 
  aux2<-  table( x, aux0$TARGET_objetivo,useNA="ifany")
  aux2<-rbind(names(aux)[i],aux2)
  super_tramo<-rbind(super_tramo,aux2)
}


write.table(BBDD,file=paste0("SALIDA/super_tramo_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)


#Trameado de mora

super_bbdd_mora$mora_max_actual_tram<-ifelse(is.na(super_bbdd_mora$mora_max_actual),"[ 0 ; 18]",ifelse(super_bbdd_mora$mora_max_actual<=18,"[ 0 ; 18]",">18")) 
freq(super_bbdd_mora$mora_max_actual_tram,useNA="ifany")
super_bbdd_mora$mora_max_1ant_tram<-ifelse(is.na(super_bbdd_mora$mora_max_1ant),"[ 0 ; 16]",ifelse(super_bbdd_mora$mora_max_1ant<=16,"[ 0 ; 16]",">16")) 
freq(super_bbdd_mora$mora_max_1ant_tram,useNA="ifany")
super_bbdd_mora$mora_max_3ant_tram<-ifelse(is.na(super_bbdd_mora$mora_max_3ant),">261",ifelse(super_bbdd_mora$mora_max_3ant<=261,"[ 0 ; 261]",">261")) 
freq(super_bbdd_mora$mora_max_3ant_tram,useNA="ifany")
super_bbdd_mora$mora_max_6ant_tram<-ifelse(is.na(super_bbdd_mora$mora_max_6ant),">491",ifelse(super_bbdd_mora$mora_max_6ant<=491,"[ 0 ; 491]",">491")) 
freq(super_bbdd_mora$mora_max_6ant_tram,useNA="ifany")




#Continuas
load(file="objetivo_buro.Rdata")
BBDD<-OBJETIVO_BURO

BBDD$VLR_DESEMB_objetivo_tram<-ifelse(BBDD$VLR_DESEMB_objetivo<=17000000,"01_[ 0 ; 17000000]",ifelse(BBDD$VLR_DESEMB_objetivo<=29100000,"02_( 17,000,000 ; 29,100,000]","03_>29,100,000"))
BBDD$SAL_CAPITA_objetivo_tram<-ifelse(BBDD$SAL_CAPITA_objetivo<=16585327,"01_[ 0 ; 16,585,327]","02_>16,585,327")
BBDD$VLR_CUOTA_objetivo_tram<-ifelse(BBDD$VLR_CUOTA_objetivo<=510486,"[ 0 ; 510,486]",">510,486")
BBDD$VLR_PAGO_objetivo_tram<-ifelse(is.na(BBDD$VLR_PAGO_objetivo),"SIN_DATO",ifelse(BBDD$VLR_PAGO_objetivo<=11548993,"[ 0 ; 11,548,993]",">11,548,993"))
BBDD$CUOTA_PAGA2_objetivo_tram<-ifelse(is.na(BBDD$CUOTA_PAGA2_objetivo),">24",ifelse(BBDD$CUOTA_PAGA2_objetivo<=24,"[ 0 ; 24]",">24"))
BBDD$CUOTA_PENDIENTE_objetivo_tram<-ifelse(is.na(BBDD$CUOTA_PENDIENTE_objetivo),">55",ifelse(BBDD$CUOTA_PENDIENTE_objetivo<=55,"[ 0 ; 55]",">55"))
BBDD$ACIERTA_A_FINANCIERO_buro_tram<-ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=699 |is.na(BBDD$ACIERTA_A_FINANCIERO_buro),"01_[ 0 ; 699]",ifelse(BBDD$ACIERTA_A_FINANCIERO_buro<=829,"02_( 699 ; 829]","03_>829"))
BBDD$QUANTO_buro_tram<-ifelse(is.na(BBDD$QUANTO_buro),"03_>5,177,000",ifelse(BBDD$QUANTO_buro<=3517000,"01_[ 0 ; 3,517,000]",ifelse(BBDD$QUANTO_buro<=5177000,"02_( 3,517,000 ; 5,177,000]","03_>5,177,000")))
BBDD$VALOR_INICIAL_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CB_buro),"03_>44,000,000",ifelse(BBDD$VALOR_INICIAL_CB_buro<=21300000,"01_[ 0 ; 21,300,000]",ifelse(BBDD$VALOR_INICIAL_CB_buro<=44000000,"02_( 21,300,000 ; 44,000,000]","03_>44,000,000")))
BBDD$VALOR_SALDO_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CB_buro),"03_>35,167,000",ifelse(BBDD$VALOR_SALDO_CB_buro<=14559000,"01_[ 0 ; 14,559,000]",ifelse(BBDD$VALOR_SALDO_CB_buro<=35167000,"02_( 14,559,000 ; 35,167,000]","03_>35,167,000")))
BBDD$VALOR_CUOTAS_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CB_buro),"03_>866,000",ifelse(BBDD$VALOR_CUOTAS_CB_buro<=460000,"01_[ 0 ; 460,000]",ifelse(BBDD$VALOR_CUOTAS_CB_buro<=866000,"02_( 460,000 ; 866,000]","03_>866,000")))
BBDD$VALOR_MORA_CB_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_CB_buro),"0",ifelse(BBDD$VALOR_MORA_CB_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CV_buro),"0",ifelse(BBDD$VALOR_INICIAL_CV_buro==0,"0",">0"))
BBDD$VALOR_SALDO_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CV_buro),"0",ifelse(BBDD$VALOR_SALDO_CV_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_CV_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CV_buro),"0",ifelse(BBDD$VALOR_CUOTAS_CV_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_CF_buro),">2,400,000",ifelse(BBDD$VALOR_INICIAL_CF_buro<=2400000,"[ 0 ; 2,400,000]",">2,400,000"))
BBDD$VALOR_SALDO_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_CF_buro),">748,000",ifelse(BBDD$VALOR_SALDO_CF_buro<=748000,"[ 0 ; 748,000]",">748,000"))
BBDD$VALOR_CUOTAS_CF_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CF_buro),">43,000",ifelse(BBDD$VALOR_CUOTAS_CF_buro<=43000,"[ 0 ; 43,000]",">43,000"))
BBDD$VALOR_CUPOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUPOS_buro),">6,290,000",ifelse(BBDD$VALOR_CUPOS_buro<=500000,"01_[ 0 ; 500,000]",ifelse(BBDD$VALOR_CUPOS_buro<=6290000,"( 500,000 ; 6,290,000]",">6,290,000")))
BBDD$VALOR_UTILIZADO_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILIZADO_buro),">273,000",ifelse(BBDD$VALOR_UTILIZADO_buro<=273000,"[ 0 ; 273,000]",">273,000"))
BBDD$PORCENTAJE_UTILIZACION_buro_tram<-ifelse(is.na(BBDD$PORCENTAJE_UTILIZACION_buro),"[ 0 ; 0.07]",ifelse(BBDD$PORCENTAJE_UTILIZACION_buro<=0.07,"[ 0 ; 0.07]",">0.07"))
BBDD$VALOR_CUOTAS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_buro),"[ 0 ; 15,000]",ifelse(BBDD$VALOR_CUOTAS_buro<=15000,"[ 0 ; 15,000]",">15,000"))
BBDD$VALOR_MORA_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_buro),"0",ifelse(BBDD$VALOR_MORA_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_SR_buro),"0",ifelse(BBDD$VALOR_INICIAL_SR_buro==0,"0",">0"))
BBDD$VALOR_SALDO_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_SR_buro),"0",ifelse(BBDD$VALOR_SALDO_SR_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_SR_buro),"0",ifelse(BBDD$VALOR_CUOTAS_SR_buro==0,"0",">0"))
BBDD$VALOR_MORA_SR_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_SR_buro),"0",ifelse(BBDD$VALOR_MORA_SR_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro),"0",ifelse(BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro==0,"0",">0"))
BBDD$VALOR_MORA_TELCOS_buro_tram<-ifelse(is.na(BBDD$VALOR_MORA_TELCOS_buro),"0",ifelse(BBDD$VALOR_MORA_TELCOS_buro==0,"0",">0"))
BBDD$VALOR_INICIAL_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_INICIAL_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_INICIAL_COOPERATIVAS_buro==0,"0",">0"))
BBDD$VALOR_SALDO_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_SALDO_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_SALDO_COOPERATIVAS_buro==0,"0",">0"))
BBDD$VALOR_CUOTAS_COOPERATIVAS_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_COOPERATIVAS_buro),"0",ifelse(BBDD$VALOR_CUOTAS_COOPERATIVAS_buro==0,"0",">0"))
BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro),">9",ifelse(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro<=4,"[ 0 ; 4]",ifelse(BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro<=9,"( 4 ; 9]",">9")))
BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram<-ifelse(is.na(BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro),"0",ifelse(BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro==0,"0",">0"))
BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram<-ifelse(is.na(BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro),"0",ifelse(BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro==0,"0",">0"))
BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram<-ifelse(is.na(BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro),"0",ifelse(BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro==0,"0",">0"))
BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro),">1",ifelse(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro==0,"0",ifelse(BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro==1,"1","3_>1"))) 
BBDD$CTAS_CTES_ACT_CTA_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_CTES_ACT_CTA_BANCA_buro),"0",ifelse(BBDD$CTAS_CTES_ACT_CTA_BANCA_buro==0,"0",">0"))
BBDD$CTAS_SALDADAS_CTAS_BANCA_buro_tram<-ifelse(is.na(BBDD$CTAS_SALDADAS_CTAS_BANCA_buro),"0",ifelse(BBDD$CTAS_SALDADAS_CTAS_BANCA_buro==0,"0",">0"))
BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram<-ifelse(is.na(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro),"2_>1",ifelse(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro==0,"0_0",ifelse(BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro==1,"1_1","2_>1")))
BBDD$ENDEUDAMIENTO_buro_tram<-ifelse(is.na(BBDD$ENDEUDAMIENTO_buro),"03_>37",ifelse(BBDD$ENDEUDAMIENTO_buro<=14,"00_[ 0 ; 14]",ifelse(BBDD$ENDEUDAMIENTO_buro<=23,"01_(14 ; 23]",ifelse(BBDD$ENDEUDAMIENTO_buro<=37,"02_[ 23 ; 37]","03_>37"))))
BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro),">0",ifelse(BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro==0,"0",">0"))
BBDD$CUPO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$CUPO_SIN_POPULAR_buro),">50,000",ifelse(BBDD$CUPO_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro),">50,000",ifelse(BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro),">50,000",ifelse(BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro<=50000,"[ 0 ; 50,000]",">50,000"))
BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro),">169,000",ifelse(BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro<=169000,"[ 0 ; 169,000]",">169,000"))
BBDD$UTILIZACION_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$UTILIZACION_SIN_POPULAR_buro),">2",ifelse(BBDD$UTILIZACION_SIN_POPULAR_buro<=2,"[ 0 ; 2]",">2"))
BBDD$VALOR_CUOTAS_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$VALOR_CUOTAS_SIN_POPULAR_buro),">2,000",ifelse(BBDD$VALOR_CUOTAS_SIN_POPULAR_buro<=2000,"[ 0 ; 2,000]",">2,000"))
BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram<-ifelse(is.na(BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro),">0",ifelse(BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro==0,"0",">0"))
BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro),"0",ifelse(BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro==0,"0",">0"))
BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro),"0",ifelse(BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro==0,"0",">0"))
BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram<-ifelse(is.na(BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro),"0",ifelse(BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro==0,"0",">0"))
BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro_tram<-ifelse(is.na(BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro),">0",ifelse(BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro==0,"0",">0"))
BBDD$RANGO_APROXIMADO_EDAD_buro<-is.numeric(BBDD$RANGO_APROXIMADO_EDAD_buro)
BBDD$RANGO_APROXIMADO_EDAD_buro_tram<-ifelse(is.na(BBDD$RANGO_APROXIMADO_EDAD_buro),"<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=35,"<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=55, "(35 ; 55]", ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro<=65, "(55 ; 65]", ">65" ))))
a<-unique(BBDD$RANGO_APROXIMADO_EDAD_buro)
BBDD$RANGO_APROXIMADO_EDAD_buro_tram<-ifelse(is.na(BBDD$RANGO_APROXIMADO_EDAD_buro),"1_<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (a[1:5]),"1_<=35",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (c('36-45', '46-55')),"2_(35 ; 55]",ifelse(BBDD$RANGO_APROXIMADO_EDAD_buro %in% (c('56-65')),"3_56-65","4_66+"))))
BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro_tram<-ifelse(is.na(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro),"3_>8",ifelse(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro<=4,"1_<=4",ifelse(BBDD$NUMERO_OBLIGACIONES_ACTIVAS_buro<=8,"2_( 4 ; 8]","3_>8" )))
BBDD$maduracion<-BBDD$CUOTA_PAGA2_objetivo/(BBDD$CUOTA_PAGA2_objetivo+ BBDD$CUOTA_PENDIENTE_objetivo)
BBDD$maduracion_tram<-ifelse(is.na(BBDD$maduracion),"02_>30%",ifelse(BBDD$maduracion<=0.30,"01_<=30%","02_>30%"))

load(file="BBDD_mora.Rdata")

BBDD$clave_mora<-paste0(BBDD$OBLIGACIONENC_objetivo,"_",BBDD$fecha_objetivo)
super_bbdd_mora$clave_mora<-paste0(super_bbdd_mora$OBLIGACIONENC_objetivo,"_",super_bbdd_mora$fecha_objetivo)
super_bbdd_mora<-super_bbdd_mora[order(super_bbdd_mora$OBLIGACIONENC_objetivo -super_bbdd_mora$fecha_objetivo),] #ordeno por clave=cedula+fecha
super_bbdd_mora2<-super_bbdd_mora[!duplicated(super_bbdd_mora$clave_mora), ] #luego de ordenamos, elimino dupicados

BBDD2<-merge(x=BBDD, y=super_bbdd_mora2, by.x="clave_mora", by.y = "clave_mora", all.x = TRUE)

BBDD2$mora_max_actual_tram<-ifelse(is.na(BBDD2$mora_max_actual),"01_<=6",ifelse(BBDD2$mora_max_actual<=6,"01_<=6","02_>6"))

#freq(BBDD2$mora_max_actual_tram)

#BBDD2$mora_max_actual_tram<-ApplyQuintiles(BBDD2$mora_max_actual)
#t(table(BBDD2$TARGET_objetivo.x, BBDD2$mora_max_actual_tram, useNA="ifany"))
#size(BBDD2$TARGET_objetivo.x)

counts<-table(BBDD2$TARGET_objetivo.x, BBDD2$mora_max_actual_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Mora_max_actual",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  




aux10<-merge(x=aux,  y=contrato10, by.x ="OBLIGACIONENC_objetivo", by.y = "ID_CONTRATO", all.x=TRUE )


#Basura que puede se útil
#aux2<-ApplyQuintiles(BBDD$maduracion)
#freq(aux2)
#write.table(aux2,file=paste0("SALIDA/tramo_BBDD_maduracion_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",row.names = T)

input.list<-names(BBDD2)
input.list<-input.list[grep("*_tram*", input.list)]
input.list<-c(input.list,"TARGET_objetivo.x")

BBDD_tram<-BBDD2[input.list]

save(BBDD, BBDD2, BBDD_tram, file="analisis.Rdata")

#Incorporación de información demora

#load(file="analisis.Rdata")

counts<-table(BBDD$TARGET_objetivo, BBDD$maduracion_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Maduración del credito",  col=c("darkblue"), ylim=c(0, 2))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  



today <- Sys.Date()

write.table(BBDD,file=paste("SALIDA/analisis_",format(today, format="%Y%m%d"),".txt", sep=""),sep=";",col.names = T)

counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_DESEMB_objetivo_tram, useNA="ifany")

mp <- barplot(VADeaths) # default
tot <- colMeans(VADeaths)
text(mp,  col = "blue")

barplot((prop.table(counts,2)*100)[2,], main="VLR_DESEMB_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$SAL_CAPITA_objetivo_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="SAL_CAPITA_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_CUOTA_objetivo_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VLR_CUOTA_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_PAGO_objetivo_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VLR_PAGO_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CUOTA_PAGA2_objetivo_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CUOTA_PAGA2_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CUOTA_PENDIENTE_objetivo_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CUOTA_PENDIENTE_objetivo",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ACIERTA_A_FINANCIERO_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ACIERTA_A_FINANCIERO_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$QUANTO_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="QUANTO_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CB_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_INICIAL_CB_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CB_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_SALDO_CB_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CB_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_CB_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_CB_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_MORA_CB_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CV_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_INICIAL_CV_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CV_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_SALDO_CV_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CV_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_CV_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CF_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_INICIAL_CF_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CF_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_SALDO_CF_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CF_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_CF_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUPOS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUPOS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILIZADO_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_UTILIZADO_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$PORCENTAJE_UTILIZACION_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="PORCENTAJE_UTILIZACION_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_MORA_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_SR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_INICIAL_SR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_SR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_SALDO_SR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_SR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_SR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_SR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_MORA_SR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_CELULARES_TELCOS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_TELCOS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_MORA_TELCOS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_COOPERATIVAS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_INICIAL_COOPERATIVAS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_COOPERATIVAS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_SALDO_COOPERATIVAS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_COOPERATIVAS_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_COOPERATIVAS_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="OBLIGA_AL_DIA_CARTERA_ACTUAL_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="OBLIGA_MORA_30_CARTERA_ACTUAL_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="OBLIGA_MORA_120_CARTERA_ACTUAL_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CARTERA_CASTIG_CARTERA_ACTUAL_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="DUDOSO_RECAUDO_CARTERA_ACTUAL_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ULT_AÑO_MORAS_30_CARTERA_HIST_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ULT_AÑO_MORAS_60_CARTERA_HIST_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ULT_AÑO_MORAS_90_CARTERA_HIST_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ULT_AÑO_MORAS_120_CARTERA_HIST_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CARTERA_BANCA_ALT_MAX_DE_MORA_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_CTES_ACT_CTA_BANCA_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CTAS_CTES_ACT_CTA_BANCA_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_SALDADAS_CTAS_BANCA_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CTAS_SALDADAS_CTAS_BANCA_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="TOTAL_CONSULTAS_ULT_6_MESES_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$ENDEUDAMIENTO_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="ENDEUDAMIENTO_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="NUM_TDC_VIGENTES_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CUPO_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CUPO_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="MAX_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="PROMEDIO_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_UTILIZADO_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$UTILIZACION_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="UTILIZACION_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_CUOTAS_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="NUMOBLVIGENSECTORBANCASIN_POPU_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="CUPOSECTORBANCARIO_SIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="MAXCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="PROMCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"))
counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro_tram, useNA="ifany")
barplot((prop.table(counts,2)*100)[2,], main="VALOR_UTILISECTORBANCASIN_POPU_buro",  col=c("darkblue"))
