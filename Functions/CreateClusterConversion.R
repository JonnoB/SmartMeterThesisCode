CreateClusterConversion <- function(nodeclustlist){
  # creates the cluster conversion dictionary from the base clusters to the unique clusters
  # these clusters can then be used to make a graph of graphs to find the cluster that
  # exist across time
  #nodeclustlist: a list produced by NodeClustList function
  
  dayorder <- names(nodeclustlist) %>% ymd
  
  #creates the table that gives the number of nodes per cluster, and gives each cluster a unique ID across all the days
  Clustconversion <- 1:length(nodeclustlist) %>%
    map_df(~{
      nodeclustlist[[.x]] %>%
        group_by(cluster) %>%
        summarise(NodesInCluster= n()) %>% 
        mutate(day=.x, 
               date=dayorder[.x],
               IntraDayRank=rank(-(NodesInCluster), ties.method="random"))
    }) %>%
    mutate(UniqueID = 1:nrow(.) %>% make.names)
  
  return(Clustconversion)
  
}