CreateAllGraphs2 <- function(SourceFolder, TargetFolder,cutoff=0.7,  StartTime = "00:00:00", EndTIme = "24:00:00", files=NA ){
  #A function to simplify the creation of all graphs, This version avoids needing all the corellation matrices but takes longer as a result
  #SourceFolder: FOlder where the day data is kept
  #TargetFolder: folder where the graphs will be stored
  #cutoff: Minimum edge corellation
  #StartTime: The start time of the period being analysed
  #EndTime: The end time of the period being analysed
  #files: Optional allows a smaller number of files to be selected
  
  #dependent packages
  #dplyr, igraph
  
  #Find all source files
  setwd(SourceFolder)
  if(is.na(files[1])) {
    files <-list.files(SourceFolder, full.names = TRUE) }
  
    for (i in 1:length(files)){

      datdat<-readRDS(files[i]) %>% 
        as.tibble %>% 
        filter(hms(sub("^.*\\s", "", as.character(Date.Time)))>=hms(StartTime), 
               hms(sub("^.*\\s", "", as.character(Date.Time)))<hms(EndTIme))
      
      cormat <- datdat[,2:ncol(datdat)] %>%
        as.matrix %>% cor
      diag(cormat) <- 0
      cormat[is.na(cormat)] <- 0
      graph <- createcleangraph2(cormat, cormat >0.7)
      print("Graph created")
      graph <-detectcomms(graph)
      print("Clusters detected")
      
      filename <- sub(".rds","",files[i])
      SavePath <- file.path(TargetFolder, paste(basename(filename) ,".graphml",sep=""))
      print(SavePath)
      write.graph(graph, 
                  SavePath, 
                  format = "graphml")
      print(paste("Completed",i,"of",length(files))) 
    }
    
  }