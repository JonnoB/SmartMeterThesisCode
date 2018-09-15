#This subscript goes through all the steps neccessary to create cross-validated predictions for all time periods.
#The script is run inside a walk function and all the useful parts are saved as files withing a files structure

#creates a list of which node is part of which day cluster
print(daypart)

if(file.exists(file.path(ModelBlock, daypart ,"nodeclustlist.rds"))){
    nodeclustlist <- readRDS(file.path(ModelBlock, daypart ,"nodeclustlist.rds"))
  Clustconversion <- readRDS(file.path(ModelBlock, daypart ,"Clustconversion.rds"))
  
}else{
  print(paste("Creating node cluster list for", daypart))
  nodeclustlist <- NodeClusterList(file.path(ModelBlock, daypart ,"Graphs")) 
  saveRDS(nodeclustlist, file.path(ModelBlock, daypart ,"nodeclustlist.rds"))
  Clustconversion <- CreateClusterConversion(nodeclustlist)
  saveRDS(Clustconversion, file.path(ModelBlock, daypart ,"Clustconversion.rds"))
  
}


if(file.exists(file.path(ModelBlock, daypart, "clusterprofile.rds"))){
  clusterprofile <- readRDS(file.path(ModelBlock, daypart , "clusterprofile.rds"))
}else{
  clusterprofile <- CreateAllProfiles(Clustconversion, daytimeseries, nodeclustlist, StartTime = StartTimei, EndTime = EndTimei)
  saveRDS(clusterprofile , file.path(ModelBlock, daypart ,"clusterprofile.rds"))
}

if(file.exists(file.path(ModelBlock, daypart, "Clustergraph.rds"))){
  Clustergraph <- readRDS(file.path(ModelBlock, daypart , "Clustergraph.rds"))
}else{
  Clustergraph <- CreateClusterGraph(Clustconversion, clusterprofile, clustercutoff = 50, edgecutoff = cutoffi)
  saveRDS(Clustergraph, file.path(ModelBlock, daypart ,"Clustergraph.rds"))
  
}


Clustconversion <- CreateClusterConversion2(Clustergraph, Clustconversion)

#creates a data frame of Nodes cluster membership across all days
NodeClust <- CreateNodeClust(nodeclustlist, Clustconversion)

#creates the transfer counts from day A to day B, used as the basis for creating the transition matrix.
#doing it this wayis faster as the conversion only has to be done once.
DayTransfer <- CreateDayTransfer(NodeClust, Clustconversion)

#Total number of nodes in each cluster each day
DayNodes <- DayTransfer %>% map(~{
  
  .x %>%
    select(-Day2) %>%
    group_by(Day1) %>%
    summarise(Nodes = sum(Nodes)) %>%
    left_join(Clustconversion %>%  #joining in this ensures all clusters are present on everyday
                distinct(ClustID) %>%
                rename(Day1= ClustID),., by = "Day1") %>%
    mutate(Day1 = ifelse(is.na(Day1),0, Day1))
  
})
names(DayNodes) <- names(nodeclustlist)[-length(nodeclustlist)]

#Split train and test

#Create the folds for cross validation
Dayindices <- 1:length(DayTransfer)
Folds <- split(Dayindices, ceiling(seq_along(Dayindices)/ceiling(length(Dayindices)/10)))
rm(Dayindices)

1:10 %>% walk(~{
  
  print(paste0("Fold ", .x, " of 10"))
  
  TrainDates <- names(DayTransfer)[Folds[[.x]]]
  TestDates <- names(DayTransfer)[!(names(DayTransfer) %in% TrainDates)]
  #Create transition matrix using a specific fold of the data
  
  TransFile <- file.path(ModelBlock, daypart, "TransitionMat", paste0("Transition_Fold",.x,".rds"))
  if(file.exists(TransFile)){
    
    TransistionMat <- readRDS(TransFile)
    
  }else{
    TransistionMat <- CreateTransistionMatrix(DayTransfer, TrainDates)
    saveRDS(TransistionMat, TransFile)
  }
  
  PredProfileFile <- file.path(ModelBlock, daypart, "PredictedProfiles", paste0("PredProfiles_Fold",.x,".rds"))
  
  #this is the final part of the walk process. If the profile already exists no action is required
  print(paste("Creating prediction profiles. saving to", PredProfileFile))
  if(!file.exists(PredProfileFile)){ 
    
    PredictedProfiles <- 1:(length(TestDates)-1) %>% map_df(~{
      
      print(.x)
      daydate <- TestDates[.x]
      CreatePredictedProfiles(daydate, DayNodes, TransistionMat, clusterprofile, Clustconversion) %>%
        mutate(date = daydate)
      
    })
    
    saveRDS(PredictedProfiles, file.path(ModelBlock, daypart, "PredictedProfiles", paste0("PredProfiles_Fold",.x,".rds")))
  }
  
  print(paste("Completed daypart",daypart))
  
})
