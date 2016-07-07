# This function is used to load saved data frames it was made because R 
# crashed so much due to lack of ram

loadRDS <- function(path = getwd()){
  setwd(path)
  files <- list.files()
  file.index<- grep(".rds", files)  
  var.names <- sub(".rds","", files[file.index])
  for(i in 1:length(file.index)){
    assign(var.names[i], readRDS(files[file.index[i]]), envir = .GlobalEnv)
    print(var.names[i])
  }
  }



