Inner join: merge(df1, df2) will work for these examples because R automatically joins the frames by common variable names, but you would most likely want to specify merge(df1, df2, by="CustomerId") to make sure that you were matching on only the fields you desired. You can also use the by.x and by.y parameters if the matching variables have different names in the different data frames.

Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x=TRUE)

Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y=TRUE)

Cross join: merge(x = df1, y = df2, by = NULL)