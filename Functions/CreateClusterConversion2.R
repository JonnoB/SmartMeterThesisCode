CreateClusterConversion2 <- function(Clustergraph, Clustconversion, minpercent = 0.01){
  #Creates the multiday cluster ID's from the cluster graph. Is used to create the new cluster
  #Conversion dataframe that is then used as the conversion dictionary for cluster ID's
  #ClusterGraph: the graph that is the cluster of clusters. outputted by the CreateClusterGraph function
  #ClustConversion, the original cluster conversion dataframe
  #minpercent: the cutoff point for the soup as decimal. default is 1%
  
  UniqueClusters<- data.frame(UniqueID= V(Clustergraph)$name ,
                              ClustID= get.vertex.attribute(Clustergraph, name = "ClusterID"),
                              stringsAsFactors =FALSE)
  
  Clustconversion2 <- Clustconversion %>% 
    left_join(.,UniqueClusters, by="UniqueID") %>%
    mutate(ClustID= ifelse(is.na(ClustID),"soup",ClustID))
  
  #remove clusters that have less than 1% of node volumne
  LargeClusts <- Clustconversion2 %>% group_by(ClustID) %>% 
    summarise(TotalNodes= sum(NodesInCluster), counts = n()) %>%
    ungroup %>% 
    mutate(PercVol = TotalNodes/sum(TotalNodes), 
           ClustID2 = ifelse(PercVol<minpercent, "soup", ClustID))
  
  Clustconversion2 <- LargeClusts %>% select(ClustID, ClustID2) %>%
    left_join(Clustconversion2, LargeClusts, by="ClustID") %>%
    select(-ClustID) %>%rename(ClustID=ClustID2) %>% 
    arrange(sub("X", "",UniqueID) %>% as.numeric)
  
  return(Clustconversion2)
}