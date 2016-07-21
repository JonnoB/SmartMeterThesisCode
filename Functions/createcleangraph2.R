createcleangraph2 <- function(cormat, edgemat, sweep =FALSE){

  weightmat <- sqrt(2*(1-cormat))
  diag(weightmat) <- 0 #removes the diagonal this isn't necessary as it will actually be done in the graph stage
  #not Necessary as edge create is done by the edgemat!
  #weightmat[is.na(weightmat)] <- 0 #removes NA's to prevent automatic edge creation
  vertexIDs <-cormat %>% colnames %>% make.names

  hasedges <- rowSums(edgemat,na.rm=TRUE) > 0
  weightmat <- weightmat*edgemat
  
  #Sweep: Sweeps away loose nodes, and so makes plotting 
  #easier preventing the halo that can occur with many loose nodes
  weightmat<- if (sweep) {
    weightmat[hasedges,hasedges]
  } else {
    weightmat
  }
  
  vertexIDs <- if (sweep) {
    vertexIDs[hasedges]
  } else {
    vertexIDs
  }
  # weightmat <- weightmat[hasedges,hasedges]
  graph = graph.adjacency(as.matrix(weightmat), 
                          mode = "undirected",
                          weighted = TRUE, diag = FALSE)
  graph <- set.vertex.attribute(graph, "name", 
                                index=V(graph), 
                                value=vertexIDs)
  
  
}