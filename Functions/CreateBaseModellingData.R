CreateBaseModellingData <- function(daypart, ModelBlock, StartTimei, EndTimei, cutoffi){
  #This function just creates or loads the data needed for the modelling
  #I have turned it into a function so that it can be used more easily in the other areas
  #like linear models and more complex tranmission matrices
  #Most og the information for this function can be found in the Timeperiods dataframe
  #daypart: the part of the day that will be used. e.g WholeDay, Evening6
  #ModelBlock: the folder where the different model types are stored
  #StartTimei: the start time of the the daypart
  #EndTimei: The Endtime of the timeblock
  
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
  
  
  #make cluster of clusters
  if(file.exists(file.path(ModelBlock, daypart, "Clustergraph.rds"))){
    Clustergraph <- readRDS(file.path(ModelBlock, daypart , "Clustergraph.rds"))
  }else{
    Clustergraph <- CreateClusterGraph(Clustconversion, clusterprofile, clustercutoff = 50, edgecutoff = cutoffi)
    saveRDS(Clustergraph, file.path(ModelBlock, daypart ,"Clustergraph.rds"))
    
  }
  
  
  
  #Update ClusterConversion with cluster membership and save.
  if(file.exists(file.path(ModelBlock, daypart ,"Clustconversion2.rds"))){
    Clustconversion <- readRDS(file.path(ModelBlock, daypart ,"Clustconversion2.rds"))
  }else{
    Clustconversion <- CreateClusterConversion2(Clustergraph, Clustconversion)
    saveRDS(Clustconversion, file.path(ModelBlock, daypart ,"Clustconversion2.rds"))
    
  }
  
  #creates a data frame of Nodes cluster membership across all days
  
  if(file.exists(file.path(ModelBlock, daypart ,"NodeClust.rds"))){
    NodeClust <- readRDS(file.path(ModelBlock, daypart ,"NodeClust.rds"))
  }else{
    NodeClust <- CreateNodeClust(nodeclustlist, Clustconversion)
    saveRDS(NodeClust, file.path(ModelBlock, daypart , "NodeClust.rds"))
    
  }
  
  
  #creates the transfer counts from day A to day B, used as the basis for creating the transition matrix.
  #doing it this wayis faster as the conversion only has to be done once.
  if(file.exists(file.path(ModelBlock, daypart , "DayTransfer.rds"))){
    DayTransfer <- readRDS(file.path(ModelBlock, daypart ,"DayTransfer.rds"))
    DayNodes <- readRDS(file.path(ModelBlock, daypart ,"DayNodes.rds"))
  }else{
    
    DayTransfer <- CreateDayTransfer(NodeClust, Clustconversion, nodeclustlist)
    saveRDS(DayTransfer, file.path(ModelBlock, daypart ,"DayTransfer.rds"))
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
    saveRDS(DayNodes, file.path(ModelBlock, daypart ,"DayNodes.rds"))
  }
  
  #Assign all to the workspace
  ls() %>%
    walk(~{
      
      assign(.x, get(.x), envir = .GlobalEnv)
      
    })
  
  print(paste("Creation of Modelling Data for",daypart, " complete."))
  
  
  
}