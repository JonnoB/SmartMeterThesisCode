detectcomms <- function(graph){
  
  #fc <- fastgreedy.community(graph)
  fc <- walktrap.community(graph)
  community <-membership(fc) %>%table %>% as.data.frame()
  names(community)[1] <- "community_number"
  community$community_number<-community$community_number
  community$community_conv <-1:nrow(community)
  community$community_conv[community$community_conv>5] <-6
  
  community %<>% left_join(., data.frame(community_conv = 1:6, shades =rainbow(6)), by="community_conv")
  
  #colour by community
  V(graph)$color <- community$shades[match(membership(fc) ,
                                           community$community_number)]
  
  graph <- set.vertex.attribute(graph, 
                                name = "ClusterColour", index = V(graph), 
                                value = 
                                  community$shades
                                [match(membership(fc) ,community$community_number)] %>% 
                                  make.names)
  
  graph <- set.vertex.attribute(graph, 
                                name = "ClusterID", index = V(graph), 
                                value =membership(fc) %>% 
                                  make.names)
  graph
  
}