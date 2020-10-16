 x <- -1:4
ifelse(x == 1, 'same', ifelse(x > 1, 'bigger', 'smaller'))

ifelse(a$resultado==0, print("La mora al cierre de mes coincide"), print("error")) 
DT[, season := ifelse(month %in% c("dec", "jan", "feb"), "winter",
               ifelse(month %in% c("mar", "apr", "may"), "spring",
               ifelse(month %in% c("jun", "jul", "aug"), "summer", 
               ifelse(month %in% c("sep", "oct", "nov"), "autumn", NA))))]