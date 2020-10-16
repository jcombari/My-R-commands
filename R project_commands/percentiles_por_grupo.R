percentiles<-c(0,0.05, 0.1, 0.15,0.20, 0.25, 0.30, 0.35, 0.40 , 0.45, 0.5, 0.55,0.60 , 0.65 , 0.70 ,0.75,  0.80, 0.85, 0.90, 0.95, 1)

#quantile(as.numeric(super_HUELLAS$SALDO_CONSUMO_MERCADO), probs = percentiles ) # quartile
aggregate(super_HUELLAS$ENDEUDAMIENTO_TOTAL_S_FINAN, by = list(super_HUELLAS$nombre), FUN = function(x) quantile(x, probs = percentiles))
