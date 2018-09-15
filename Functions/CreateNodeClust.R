CreateNodeClust <- function(nodeclustlist, Clustconversion){
  
  #creates a data frame of Nodes cluster membership across all days
  NodeClust <- 1:length(nodeclustlist) %>% map(~{
    print(.x)
    DayNodes <- nodeclustlist[[.x]]
    
    daydate <- names(nodeclustlist)[.x]
    
    DayConv <- Clustconversion %>%
      filter(date == daydate)
    
    DayNodes2 <- left_join(DayNodes, DayConv, by ="cluster") %>%
      select(NodeID, ClustID) %>%
      setnames(c("NodeID", daydate))
    
  }) %>% 
    reduce(left_join, by = "NodeID")
  
  return(NodeClust)
}