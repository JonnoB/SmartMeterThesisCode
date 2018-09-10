NodeClusterList <- function(Graphpath){
  #Creates a list of nodeclusters
  #Graphpath: the path where all the graphs have been saved
  
  files <- list.files(path = GraphPath, full.names = TRUE)
  
  #The Mclapply version kept crashing so the part is done in single ##is this true should I delete this line?
  
  nodeclustlist <-lapply(files, function(n){
    graph <- read_graph(n, format="graphml")
    vertices <-data.frame(NodeID= V(graph)$name, 
                          cluster=
                            get.vertex.attribute(graph, "ClusterID"))# %>%
     # mutate(yesno=1)%>%
    #  spread(., key= cluster, value=yesno) %>% 
   #   mutate(NodeID= as.character(NodeID))
    print(paste("file:", n," date:",  grep(n, files)))
    vertices
  }
  )
  
  
  names(nodeclustlist)<-sub(".graphml", "",files) %>%sub("date_","",.) %>%ymd
  
  return(nodeclustlist)
  
}