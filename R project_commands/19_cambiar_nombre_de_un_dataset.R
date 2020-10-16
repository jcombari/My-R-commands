df <- data.frame(A=(1:10),B=(1:10),C=(1:10))
df_names <- data.frame(code=c("A","B","D","E","C"),name=c("Col A","Col B","Col D","Col E","Col C"))

names(df)<-df_names$name[match(names(df),df_names$code)]