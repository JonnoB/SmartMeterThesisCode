CreateClusterGraph <- function(Clustconversion, clusterprofile, clustercutoff = 50, edgecutoff = 0.7){
  #This function creates the graph of graphs used for defining which clusters are similar
  #The cluster detection can take a while
  #Clustconversion: the dataframe Clustconversion that switches between cluster names
  #clusterprofile: the datframe that is the profile of all the clusters
  #Clustercutoff: the minimum number of nodes a cluster needs to avoid being in the soup
  #edgecutoff: the minimimum corellation required for an edge to exist
  
  LargeClusts <- filter(Clustconversion, NodesInCluster>clustercutoff)$UniqueID %>%
    match(.,colnames(clusterprofile)) 
  
  ClustComms <- cor(clusterprofile[,LargeClusts])
  
  ClustComms[is.na(ClustComms)]<-0
  Clustergraph <- createcleangraph2(ClustComms, ClustComms > edgecutoff)
  print("Graph created")
  Clustergraph <-detectcomms(Clustergraph)
  print("Clusters detected")
  
  return(Clustergraph)
  
}