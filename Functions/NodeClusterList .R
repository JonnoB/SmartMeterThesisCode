NodeClusterList <- function(GraphPath){
  #Creates a list of nodeclusters
  #Graphpath: the path where all the graphs have been saved
  
  files <- list.files(path = GraphPath, full.names = TRUE)

  #The Mclapply version kept crashing so the part is done in single ##is this true should I delete this line?
  
  nodeclustlist <-lapply(files, function(n){
    graph <- read_graph(n, format = "graphml")
    vertices <-data.frame(NodeID = V(graph)$name, 
                          cluster = get.vertex.attribute(graph, "ClusterID"))
    
    print(paste("file:", n," date:",  grep(n, files)))
    
    return(vertices)
  }
  )
  
  
  names(nodeclustlist) <- files %>% basename %>% sub(".graphml", "",.) %>% sub("date_","",.) %>% ymd
  
  return(nodeclustlist)
  
}