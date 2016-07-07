createcleangraph <- function(weightmat, edgemat,mincons=10, sweep =TRUE){
  hasedges <- rowSums(edgemat,na.rm=TRUE) > mincons
  weightmat <- weightmat*edgemat
  
  weightmat<- if (sweep) {
            weightmat[hasedges,hasedges]
            } else {
              weightmat
            }
 # weightmat <- weightmat[hasedges,hasedges]
  graph = graph.adjacency(as.matrix(weightmat), 
                              mode = "undirected",
                              weighted = TRUE, diag = FALSE)
  graph <- set.vertex.attribute(graph, "name", 
                               index=V(graph), 
                               value=names(weightmat))
  
  
}