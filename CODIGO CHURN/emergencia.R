ruta="E:/Jennyfer_C/04_CHURN/BBDD"
setwd(ruta)
getwd()

load(file="analisis.Rdata")

counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_DESEMB_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_DESEMB_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$SAL_CAPITA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="SAL_CAPITA_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_CUOTA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_CUOTA_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD$TARGET_objetivo, BBDD$VLR_PAGO_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_PAGO_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CUOTA_PAGA2_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PAGA2_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CUOTA_PENDIENTE_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PENDIENTE_objetivo",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$ACIERTA_A_FINANCIERO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ACIERTA_A_FINANCIERO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$QUANTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="QUANTO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_INICIAL_CB_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CB_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CB_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_MORA_CB_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_INICIAL_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_INICIAL_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUPOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUPOS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILIZADO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_UTILIZADO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$PORCENTAJE_UTILIZACION_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PORCENTAJE_UTILIZACION_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_MORA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_INICIAL_SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_MORA_SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CELULARES_TELCOS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_MORA_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_MORA_TELCOS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_INICIAL_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_INICIAL_COOPERATIVAS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_SALDO_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_COOPERATIVAS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_COOPERATIVAS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_AL_DIA_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_30_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_120_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CARTERA_CASTIG_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="DUDOSO_RECAUDO_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_30_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_60_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_90_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_120_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD$TARGET_objetivo, BBDD$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CARTERA_BANCA_ALT_MAX_DE_MORA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_CTES_ACT_CTA_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_CTES_ACT_CTA_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CTAS_SALDADAS_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_SALDADAS_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="TOTAL_CONSULTAS_ULT_6_MESES_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$ENDEUDAMIENTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ENDEUDAMIENTO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUM_TDC_VIGENTES_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CUPO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$MAX_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAX_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMEDIO_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILIZADO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_UTILIZADO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$UTILIZACION_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="UTILIZACION_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_CUOTAS_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUMOBLVIGENSECTORBANCASIN_POPU_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPOSECTORBANCARIO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAXCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD$TARGET_objetivo, BBDD$VALOR_UTILISECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_UTILISECTORBANCASIN_POPU_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  



#EMERGENCIA 2

#Limpieza 
rm(list=ls())

#Ruta
ruta="E:/Jennyfer_C/04_CHURN/BBDD"
sub_ruta="/huellas_consulta/"
setwd(ruta)
getwd()

#Verificación de paquetes
list.of.packages <- c("tidyverse", "readxl", "dummies","caret","sqldf", "matlab","matlib","plotly","phantom", "orca","AMR","rlist","devtools","gdata","xlsReadWrite","xlsx","rio","installr","summarytools","GoodmanKruskal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
system("matlab -nodisplay -r 'stuff; to; do; in; matlab;'")
today <- Sys.Date()

load(file="analisis.Rdata")

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$RANGO_APROXIMADO_EDAD_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="RANGO_APROXIMADO_EDAD_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$maduracion_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Maduración del credito",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VLR_DESEMB_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_DESEMB_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$SAL_CAPITA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="SAL_CAPITA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VLR_CUOTA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_CUOTA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VLR_PAGO_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_PAGO_prepago",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CUOTA_PAGA2_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PAGA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CUOTA_PENDIENTE_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PENDIENTE_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ACIERTA_A_FINANCIERO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Score interno de data credito [0;1000]",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$QUANTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="QUANTO Predictor de ingreso de datacredito",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_INICIAL_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Bancos_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_SALDO_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo Cartera Bancos_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas  Cartera Bancos
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_MORA_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor en Mora Cartera Bancos
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_INICIAL_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Vivienda
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_SALDO_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_INICIAL_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Financiera
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_SALDO_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUPOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Sumatoria de cupos de tarjeta de crédito
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_UTILIZADO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo total de tarjeta de crédito
           buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$PORCENTAJE_UTILIZACION_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PORCENTAJE_UTILIZACION VALOR_UTILIZADO/Valor Cupos",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas totales de tarjeta de crédito_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo mora de tarjeta de crédito_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_INICIAL_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_SALDO_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo créditos Sector Real
           _SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_MORA_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo Mora créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CELULARES_TELCOS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_MORA_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo mora en celulares
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_INICIAL_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado sector Cooperativo
           S_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_SALDO_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo créditos Sector Cooperativo
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor Cuotas créditos Sector Cooperativo 
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_AL_DIA_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_30_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_120_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Número de creditos en CARTERA_CASTIG_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="DUDOSO_RECAUDO_CARTERA_ACTUAL_buro Marca de todas las obligaciones en mora de 180",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_30_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_60_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_90_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_120_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CARTERA_BANCA_ALT_MAX_DE_MORA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CTAS_CTES_ACT_CTA_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_CTES_ACT_CTA_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CTAS_SALDADAS_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_SALDADAS_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="TOTAL_CONSULTAS_ULT_6_MESES_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$ENDEUDAMIENTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ENDEUDAMIENTO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUM_TDC_VIGENTES_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CUPO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$MAX_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAX_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMEDIO_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_UTILIZADO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_UTILIZADO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$UTILIZACION_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="UTILIZACION_SIN_POPULAR_buro VALOR_UTILIZADO /Valor Cupos  Sin Popular",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_CUOTAS_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUMOBLVIGENSECTORBANCASIN_POPU_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPOSECTORBANCARIO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAXCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo.x, BBDD_tram$VALOR_UTILISECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor utilizado de tarjeta de crédito sin popular
_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#EMERGENCIA 3

#procesos repetitivos y aburridos que no le genera nungun valor a la empresa ni al empleado

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

load(file="analisis.Rdata")

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$RANGO_APROXIMADO_EDAD_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="RANGO_APROXIMADO_EDAD_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$maduracion_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Maduración del credito",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VLR_DESEMB_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_DESEMB_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$SAL_CAPITA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="SAL_CAPITA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VLR_CUOTA_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_CUOTA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VLR_PAGO_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VLR_PAGO_prepago",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CUOTA_PAGA2_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PAGA_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CUOTA_PENDIENTE_objetivo_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUOTA_PENDIENTE_super_base",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ACIERTA_A_FINANCIERO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Score interno de data credito [0;1000]",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$QUANTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="QUANTO Predictor de ingreso de datacredito",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_INICIAL_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Bancos_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_SALDO_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo Cartera Bancos_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas  Cartera Bancos
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_MORA_CB_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor en Mora Cartera Bancos
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_INICIAL_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Vivienda
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_SALDO_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_CV_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CV_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_INICIAL_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado Cartera Financiera
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_SALDO_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_SALDO_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_CF_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CF_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUPOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Sumatoria de cupos de tarjeta de crédito
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_UTILIZADO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo total de tarjeta de crédito
           buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$PORCENTAJE_UTILIZACION_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PORCENTAJE_UTILIZACION VALOR_UTILIZADO/Valor Cupos",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas totales de tarjeta de crédito_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo mora de tarjeta de crédito_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_INICIAL_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_SALDO_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo créditos Sector Real
           _SR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Cuotas créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_MORA_SR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo Mora créditos Sector Real
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_CELULARES_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_CELULARES_TELCOS_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_MORA_TELCOS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo mora en celulares
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_INICIAL_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor desembolsado sector Cooperativo
           S_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_SALDO_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Saldo créditos Sector Cooperativo
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_COOPERATIVAS_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor Cuotas créditos Sector Cooperativo 
           _buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$OBLIGA_AL_DIA_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_AL_DIA_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$OBLIGA_MORA_30_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_30_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$OBLIGA_MORA_120_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="OBLIGA_MORA_120_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CARTERA_CASTIG_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Número de creditos en CARTERA_CASTIG_CARTERA_ACTUAL_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$DUDOSO_RECAUDO_CARTERA_ACTUAL_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="DUDOSO_RECAUDO_CARTERA_ACTUAL_buro Marca de todas las obligaciones en mora de 180",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ULT_AÑO_MORAS_30_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_30_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ULT_AÑO_MORAS_60_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_60_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#ojo
counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ULT_AÑO_MORAS_90_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_90_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ULT_AÑO_MORAS_120_CARTERA_HIST_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ULT_AÑO_MORAS_120_CARTERA_HIST_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  


counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CARTERA_BANCA_ALT_MAX_DE_MORA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CARTERA_BANCA_ALT_MAX_DE_MORA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_DE_AHORRO_ACT_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CTAS_CTES_ACT_CTA_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_CTES_ACT_CTA_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CTAS_SALDADAS_CTAS_BANCA_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CTAS_SALDADAS_CTAS_BANCA_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="TOTAL_CONSULTAS_ULT_6_MESES_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$ENDEUDAMIENTO_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="ENDEUDAMIENTO_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$NUM_TDC_VIGENTES_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUM_TDC_VIGENTES_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CUPO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$MAX_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAX_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$PROMEDIO_CUPO_TDC_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMEDIO_CUPO_TDC_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_UTILIZADO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_UTILIZADO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$UTILIZACION_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="UTILIZACION_SIN_POPULAR_buro VALOR_UTILIZADO /Valor Cupos  Sin Popular",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_CUOTAS_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="VALOR_CUOTAS_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$NUMOBLVIGENSECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="NUMOBLVIGENSECTORBANCASIN_POPU_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$CUPOSECTORBANCARIO_SIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="CUPOSECTORBANCARIO_SIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$MAXCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="MAXCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$PROMCUPOSECTORBANCASIN_POPULAR_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="PROMCUPOSECTORBANCASIN_POPULAR_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

counts<-table(BBDD_tram$TARGET_objetivo2, BBDD_tram$VALOR_UTILISECTORBANCASIN_POPU_buro_tram, useNA="ifany")
x<-barplot((counts[2,]/counts[1,])*100, main="Valor utilizado de tarjeta de crédito sin popular
_buro",  col=c("darkblue"), ylim=c(0, 5))
aux<-(counts[2,]/counts[1,])*100
text(x = x,0 ,  paste(round(aux, digits = 2),"%"), col = "white",cex=1,pos=3)  

#Emergencia 4
#Test chi cuadrado de independencia

table(BBDD2$TARGET_objetivo2, BBDD2$TARGET_objetivo3, useNA = "ifany")

BBDD2$marca_ind<-ifelse(BBDD2$marca_fidelizacion==1 ,2,BBDD2$TARGET_objetivo2)

table(BBDD2$marca_ind, BBDD2$TARGET_objetivo3, useNA="ifany")

#cathy o'neill arma de destruccion matematica - weapons of math destruction

input.list<-names(BBDD2)
input.list<-input.list[grep("*_tram*", input.list)]
BBDD_tram<-BBDD2[input.list]
super_GKtau_sin_fid<-''
super_GKtau_con_fid<-''

BBDD2$TARGET_objetivo2<-as.factor(BBDD2$TARGET_objetivo2)

for (variable in input.list){
  aux<-""
  aux<-BBDD2[variable]
  sin_fid<-GKtau(BBDD2$TARGET_objetivo3, BBDD2$TARGET_objetivo2)
  GoodmanKruskalTau(aux, BBDD2$TARGET_objetivo2)
  con_fid<-GKtau(aux, BBDD2$TARGET_objetivo3)
  super_GKtau_sin_fid<-rbind(super_GKtau_sin_fid,sin_fid)
  super_GKtau_con_fid<-rbind(super_GKtau_con_fid,con_fid)
}


BBDD2$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram<-
BBDD2$TARGET_objetivo3


for (i in 1:10){
  d.frame <- # create your data frame here
    assign( variable,BBDD2[i])
}

tabla1<-table(BBDD2$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram,BBDD2$TARGET_objetivo3, useNA="ifany")
chisq.test(x = tabla1)

tabla2<-table(BBDD2$TOTAL_CONSULTAS_ULT_6_MESES_buro_tram,BBDD2$TARGET_objetivo2, useNA="ifany")
chisq.test(x = tabla2)

lapply(get(paste0("df", seq_len(n))), summary)
lapply(mget(paste0("BBDD2$", variable)), summary)

aux<-paste0("BBDD2$", variable)
get("aux")
