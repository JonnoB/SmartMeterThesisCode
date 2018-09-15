CreatePredictedProfiles <- function(daydate, DayNodes, TransistionMat, clusterprofile, Clustconversion){
  #Creates the predicted profiles for all clusters across the following day.
  #DayNodes: The numbers of nodes in that day
  #TransitionMat: The transition matrix,
  #clustprofile: the dataframe of all cluster profiles.
  #Clustconversion: This is the conversion dictionary for clusters
  
  ref <- match(daydate, names(DayNodes))
  
  PredictedNodes <- DayNodes[[ref]]$Nodes %*% as.matrix(TransistionMat[,-1])
  PredictedNodes <- data_frame(ClustID = colnames(PredictedNodes), NodesInCluster = (t(PredictedNodes)[,1]))
  
  DayInfo <- Clustconversion %>%
    filter(date == daydate)
  
  PredictedProfiles <- clusterprofile[, names(clusterprofile) %in% c("time", DayInfo$UniqueID)] %>%
    gather(key = "UniqueID", value = "kwh", -time) %>%
    left_join(., select(DayInfo, UniqueID, ClustID, NodesInCluster), by = "UniqueID") %>%
    mutate(kwh = kwh*NodesInCluster) %>%
    select(-UniqueID) %>%
    group_by(ClustID, time) %>%
    summarise_all(sum) %>%
    mutate(kwh = kwh/NodesInCluster) %>%
    select(-NodesInCluster) %>% #remove the true nodes in cluster
    full_join(PredictedNodes, by = "ClustID") %>% #replace with the predicted nodes in cluster
    mutate(kwh = kwh*NodesInCluster) %>% ungroup
  
  DayInfo2 <- Clustconversion %>%
    filter(date ==names(DayNodes)[ref + 1])
  Truth <- clusterprofile[, names(clusterprofile) %in% c("time", DayInfo2$UniqueID)] %>%
    gather(key = "UniqueID", value = "kwh", -time) %>%
    left_join(., select(DayInfo2, UniqueID, ClustID, NodesInCluster), by = "UniqueID") %>%
    mutate(kwh = kwh*NodesInCluster) %>%
    select(-UniqueID) %>%
    group_by(ClustID, time) %>%
    summarise_all(sum) %>%
    select(time, ClustID, TrueValues = kwh) %>% ungroup
  
  PredictedProfiles <- PredictedProfiles %>%
    full_join(., Truth, by = c("ClustID", "time")) %>%
    mutate(diff = TrueValues-kwh) %>% ungroup 

  
  return(PredictedProfiles)
  
}