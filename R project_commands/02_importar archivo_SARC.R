catego <- read.delim("C:/Users/JENNYFER/Desktop/TODO/varios/00_R/bases/Exo_Catego_201408.txt", header = TRUE, sep = "\t")
head(catego)
colnames(catego)
#rownames(catego)
attach(catego)

library(sqldf)
library(tcltk)


