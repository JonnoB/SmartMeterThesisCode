createcleangraph2 <- function(cormat, edgemat, sweep =TRUE){
  
  weightmat <- sqrt(2*(1-cormat))
  diag(weightmat) <- 0 #removes the diagonal this isn't necessary as it will actually be done in the graph stage
  weightmat[is.na(weightmat)] <- 0 #removes NA's to prevent automatic edge creation
  vertexIDs <-cormat %>% colnames %>% make.names

  hasedges <- rowSums(edgemat,na.rm=TRUE) > 0
  weightmat <- weightmat*edgemat
  
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