# A function to simplify the creation of all corellation matrices
# allowing a single line command, allows for parallel processing although
# this should be used with caution. Parallel mode will probably end up with an
# error and then take some time to write to the drive if being used on AWS

CreateAllCormats <- function(SourceFolder, TargetFolder, parallel.mode= FALSE ){

  #Find all source files
  setwd(SourceFolder)
  files <-list.files()
  
  if(parallel.mode){
    
    mclapply(1:length(files), function(i){
      setwd(SourceFolder)
      file <- files[i]
      datdat<-readRDS(file) %>%ungroup
      meterIDs <- names(datdat)[-1]
      print("Data Loaded")
      weightmat <- datdat[,2:ncol(datdat)] %>% 
        as.matrix %>% cor
      diag(weightmat) <- 0
      
      setwd(TargetFolder)  
      saveRDS(weightmat, paste(file,".rds",sep=""))
      print(paste(i,"of",length(files))) 
      gc()
    },mc.cores =detectCores()
    )
    
  } else {
    
    for (i in 1:length(files)){
      
      setwd(SourceFolder)
      file <- files[i]
      
      if(file.exists(file.path(TargetFolder,paste(file,".rds",sep="")))){
        print(paste("day", i,"file found proceeding to next day"))
      }else{

        datdat<-readRDS(file) %>%ungroup
        meterIDs <- names(datdat)[-1]
        print("Data Loaded")
        weightmat <- datdat[,2:ncol(datdat)] %>%
          as.matrix %>% cor
        diag(weightmat) <- 0
        setwd(TargetFolder)  
        saveRDS(weightmat, paste(file,".rds",sep=""))
        print(paste(i,"of",length(files))) 
        gc()
      }
    }
    
    
  }
  
  
    
}