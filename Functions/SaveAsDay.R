SaveAsDay <- function(df, path){
  #Saves each day as an individual file. an important intermediary stage
  #This function is preceded by the loading of all the cleaned and 
  #this function is followed by the creation of the corellation matrices
  
  datevect <- df$Date.Time %>%as.Date() %>% unique

  for (i in 1:length(datevect)){
    filter(df, as.Date(Date.Time)==datevect[i]) %>%
      saveRDS(., file.path(path, paste("date_", datevect[i] ,sep="")))
    print(i) 
    
  }
}


