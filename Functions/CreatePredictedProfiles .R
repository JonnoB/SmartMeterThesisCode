CreatePredictedProfiles <- function(daydate, DayNodes, TransistionMat, clusterprofile, Clustconversion, MeanProfiles){
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
  
  NodeTimeCombos <- expand.grid(time = unique(clusterprofile$time),ClustID = TransistionMat$Day1) %>% as.tibble
  
  #Creates load profile per node per cluster aka mean load profile
  PredictedProfiles <- clusterprofile[, names(clusterprofile) %in% c("time", DayInfo$UniqueID)] %>%
    gather(key = "UniqueID", value = "kwh", -time) %>%
    left_join(., select(DayInfo, UniqueID, ClustID, NodesInCluster), by = "UniqueID") %>%
    mutate(kwh = kwh*NodesInCluster) %>%
    select(-UniqueID) %>%
    group_by(ClustID, time) %>%
    summarise_all(sum) %>%
    mutate(kwh = kwh/NodesInCluster) %>%
    select(-NodesInCluster) %>% #remove the true nodes in cluster
    left_join(NodeTimeCombos, ., by = c("ClustID", "time")) %>%#need to add in an autofill that replaces NA with mean cluster profile 
    mutate(ProfileType = "True Profile")  

  #making a scaler for the generic  
  if(sum(is.na(PredictedProfiles$kwh))>0){

    #create a scale value
   ScaleValue <- left_join(PredictedProfiles, MeanProfiles, by = c("time", "ClustID")) %>%
     filter(!is.na(kwh.x)) %>%
     mutate(scale = kwh.x/kwh.y) %>%
     pull(scale) %>% mean
  
   #Replace Missing cluster profiles with a generic cluster profile
   PredictedProfiles <- unique(PredictedProfiles$ClustID) %>%
     map_df(~{
       
       Temp <- MeanProfiles %>% 
         filter(ClustID == .x)
       
       Temp2 <- PredictedProfiles %>% 
         filter(ClustID == .x) 
       
       if(sum(is.na(Temp2$kwh))>0){
         
         Temp2<- Temp2 %>%
           select(-kwh) %>%
           left_join(., Temp, by = c("ClustID", "time")) %>%
           mutate(ProfileType = "Generic Profile",
                  kwh = kwh*ScaleValue)
         
       }
       
       Temp2
       
     })
   
   }

  
  #Creates sum load profile per cluster
  PredictedProfiles <- PredictedProfiles %>%
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
    mutate(
      TrueValues = ifelse(is.na(TrueValues), 0 , TrueValues), #ensures that there are 0 values for the clusters missing in the truth
      diff = TrueValues-kwh) %>% ungroup 

  
  return(PredictedProfiles)
  
}