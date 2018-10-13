#This subscript goes through all the steps neccessary to create cross-validated predictions for all time periods.
#The script is run inside a walk function and all the useful parts are saved as files withing a files structure

#creates a list of which node is part of which day cluster
print(daypart)

#Split train and test

#Create the folds for cross validation
Dayindices <- 1:length(DayTransfer)
Folds <- split(Dayindices, ceiling(seq_along(Dayindices)/ceiling(length(Dayindices)/10))) #the indices of the test data
rm(Dayindices)

1:10 %>% walk(~{
  
  print(paste0("Fold ", .x, " of 10"))
  
  TestDates <- names(DayTransfer)[Folds[[.x]]]
  TrainDates <- names(DayTransfer)[!(names(DayTransfer) %in% TestDates)]
  #Create transition matrix using a specific fold of the data
  
  TransFile <- file.path(ModelBlock, daypart, "TransitionMat", paste0("Transition_Fold",.x,".rds"))
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
  
  PredProfileFile <- file.path(ModelBlock, daypart, "PredictedProfiles", paste0("PredProfiles_Fold",.x,".rds"))
  
  #this is the final part of the walk process. If the profile already exists no action is required
  print(paste("Creating prediction profiles. saving to", PredProfileFile))
  if(!file.exists(PredProfileFile)){ 
    
    PredictedProfiles <- 1:(length(TestDates)-1) %>% map_df(~{
      
      print(.x)
      daydate <- TestDates[.x]
      CreatePredictedProfiles(daydate, DayNodes, TransistionMat, clusterprofile, Clustconversion, MeanClustProfile) %>%
        mutate(date = daydate)
      
    })
    
    saveRDS(PredictedProfiles, file.path(ModelBlock, daypart, "PredictedProfiles", paste0("PredProfiles_Fold",.x,".rds")))
  }
  
  print(paste("Completed daypart",daypart))
  
})
