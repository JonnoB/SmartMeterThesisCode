CreatePredictions <-function(DayTransfer, ModelBlock, daypart, TransmissionType, clusterprofile, Clustconversion,
                              DayNodes, TargetDays = "all"){
  #This function creates the cross validated transmission matrices and also predicts on the data.
  #
  #TransmissionType: This is a character string, it is what you decide to name this particular transmission matrix e.g "Monday24hr" or 
  #                   "MondayWeekAhead". You need to make sure there is a folder for this
  #TargetDays: a character vector of the days you want to be included in the transmission matrix. use the first day of the day pair.
  #             e.g. if you want a transmission matrix for Monday to tuesday only, the TargetDays is "Monday", if you want Monday to Friday
  #             The target days are c("Monday", "Tuesday", "Wednesday", "Thursday").

  #filter to necessary days
  if(TargetDays[1] != "all"){
 
    daydates <- names(DayTransfer) %>% as.Date()
    DayOfWeek <- weekdays(daydates)
    daydates2 <- DayOfWeek %in% TargetDays
    DayTransfer <- DayTransfer[daydates2] 
  
  }

  
  #Create the folds for cross validation
  Dayindices <- 1:length(DayTransfer)
  Folds <- split(Dayindices, ceiling(seq_along(Dayindices)/ceiling(length(Dayindices)/10))) #the indices of the test data
  rm(Dayindices)
  
  1:10 %>% walk(~{
    
    print(paste0("Fold ", .x, " of 10"))
    #Split train and test
    TestDates <- names(DayTransfer)[Folds[[.x]]]
    TrainDates <- names(DayTransfer)[!(names(DayTransfer) %in% TestDates)]
    #Create transition matrix using a specific fold of the data
    
    TransFile <- file.path(ModelBlock, daypart, "TransitionMat", TransmissionType, paste0("Transition_Fold",.x,".rds"))
    
    print(dirname(TransFile))
    dir.create(dirname(TransFile), recursive = TRUE) #folder created if necessary
    if(file.exists(TransFile)){
      
      TransistionMat <- readRDS(TransFile)
      
    }else{
      TransistionMat <- CreateTransistionMatrix(DayTransfer, TrainDates)
      saveRDS(TransistionMat, TransFile)
    }
    
    #Create a mean cluster profile from the training data to fill in any missing clusters
    MeanClustProfile <- clusterprofile[, c(1, Folds[[.x]]+1)] %>%
      gather(key = "UniqueID", value = "kwh", -time) %>%
      left_join(., Clustconversion %>% select(UniqueID, ClustID), by = "UniqueID") %>%
      select(-UniqueID) %>%
      group_by(ClustID, time) %>%
      summarise_all(mean)
    
    PredProfileFile <- file.path(ModelBlock, daypart, "PredictedProfiles", TransmissionType, paste0("PredProfiles_Fold",.x,".rds"))
    
    #this is the final part of the walk process. If the profile already exists no action is required
    print(paste("Creating prediction profiles. saving to", PredProfileFile))
    if(!file.exists(PredProfileFile)){ 
      
      PredictedProfiles <- 1:(length(TestDates)-1) %>% map_df(~{
        
        #print(.x)
        daydate <- TestDates[.x]
        CreatePredictedProfiles(daydate, DayNodes, TransistionMat, clusterprofile, Clustconversion, MeanClustProfile) %>%
          mutate(date = daydate)
        
      })
      dir.create(dirname(PredProfileFile), recursive = TRUE) #folder created if necessary
      saveRDS(PredictedProfiles, PredProfileFile)
    }
    
    print(paste("Completed daypart",daypart))
    
  })
  
  
  
}