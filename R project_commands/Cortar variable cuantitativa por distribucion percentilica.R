breaks<-quantile(buro_201612$ACIERTA_A_FINANCIERO, probs = seq(0, 1, by= 0.1)) # decile

size(cut(buro_201612$ACIERTA_A_FINANCIERO, breaks, right=FALSE))