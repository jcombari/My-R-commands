#cargar paquete car
grupo$nueva4<-Recode(grupo$nota,
'1:10=1;
1:15=2; 
16:20=3', 
as.factor.result=TRUE)
grupo$nueva5<-Recode(grupo$nota,
'1:10="REPROBADO";
1:15="APROBADO_BAJA"; 
16:20="APROBADO_ALTA"', 
as.factor.result=TRUE)
table(grupo$nueva4)
table(grupo$nueva5)

#Forzar orden de variables en la tabla

grupo$nueva5<-Recode(grupo$nota,
'1:10="REPROBADO";
1:15="APROBADO_BAJA"; 
16:20="APROBADO_ALTA"', 
as.factor.result=TRUE, levels=c("REPROBADO","APROBADO_BAJA","APROBADO_ALTA"))

table(grupo$nueva4)
table(grupo$nueva5)