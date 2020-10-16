old_names<- list.files()
new_names<-gsub(" ", "_",file_names)
file.rename(from=old_names, to=new_names) 
list.files() #to check
