# A function to simplify the creation of all graphs
# allowing a single line command, allows for parallel processing although
# this should be used with caution. Parallel mode will probably end up with an
# error and then take some time to write to the drive if being used on AWS

#dependent packages
#dplyr, parallel, igraph

CreateAllGraphs <- function(SourceFolder, TargetFolder,cutoff=0.7 , parallel.mode= FALSE, files=NA ){

  #Find all source files
  setwd(SourceFolder)
  if(is.na(files)) {
    files <-list.files()}
  
  if(parallel.mode){
    
    mclapply(1:length(files), function(i){
      
      setwd(SourceFolder)
      fileid <- readRDS(files[i])
      #The Na's are removed here as otherwise there are links all over the place due to the missing 2 nodes
      fileid[is.na(fileid)] <- 0
      graph <- createcleangraph2(fileid, fileid >0.7)
      print("Graph created")
      graph <-detectcomms(graph)
      print("Clusters detected")
      setwd(TargetFolder)
      filename <- sub(".rds","",files[i])
      write.graph(graph, paste(filename ,".graphml",sep=""), format = "graphml")
      print(paste("Completed",i,"of",length(files))) 
      
    },mc.cores =detectCores()
    )
    
  } else {
    
    for (i in 1:length(files)){
      print(i)
      setwd(SourceFolder)
      fileid <- readRDS(files[i])
      #The Na's are removed here as otherwise there are links all over the place due to the missing 2 nodes
      fileid[is.na(fileid)] <- 0
      graph <- createcleangraph2(fileid, fileid >0.7)
      print("Graph created")
      graph <-detectcomms(graph)
      print("Clusters detected")
      setwd(TargetFolder)
      filename <- sub(".rds","",files[i])
      write.graph(graph, paste(filename ,".graphml",sep=""), format = "graphml")
      print(paste("Completed",i,"of",length(files))) 
    }
    
    
  }
  
  
    
}