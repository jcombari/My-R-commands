prepagos<-read.csv(gsub(" ", "",paste("PREPAGOS/","PLEXUS_PREPAGOS_ENC")), header = T,sep=";")
write.xlsx(unlist(names(prepagos)),file=paste0("SALIDA/prepagos_",format(today, format="%Y%m%d"),".xlsx"),row.names = FALSE)
sqldf("select NOM_UNIDAD, count(NOM_UNIDAD) from prepagos group by NOM_UNIDAD")

Buro<-read.csv(gsub(" ", "",paste("Buro/","Consultas_Buro")), header = T,sep=";")

buro_subset<-as.data.frame(Buro[,c("TIPO_ID","TOTAL_CONSULTAS_ULT_6_MESES","CEDULAENC")])

prepagos2<-merge(x=prepagos, y=buro_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

sqldf("select TOTAL_CONSULTAS_ULT_6_MESES, count(TOTAL_CONSULTAS_ULT_6_MESES) from prepagos2 group by TOTAL_CONSULTAS_ULT_6_MESES")

super_subset<-as.data.frame(super_super[,c("CALIF_CART","CEDULAENC")])

prepagos3<-merge(x=prepagos, y=super_subset, by.x="CEDULAENC", by.y="CEDULAENC", all.x=TRUE )

sqldf("select CALIF_CART, count(CALIF_CART) from prepagos3 group by CALIF_CART")

#save(prepagos, file = "prepago.RData")
#save(buro, file = "buro.RData")
load("prepago.RData")


