CreateDayTransfer <- function(NodeClust, Clustconversion, nodeclustlist, DaysAhead = 1){
  #creates the transfer counts from day A to day B, ensures that all clusters are included even if transfer = 0

  Combos <- unique(Clustconversion$ClustID)
  Combos <- expand.grid(Combos,Combos) %>% as.tibble() %>%
    rename(Day1 = Var1, Day2 = Var2)
  
  DayTransfer <- 2:(ncol(NodeClust)-1) %>% #one is lost as it is the NodeID column the other because it's the next day
    map(~{
      print(.x)
      Temp <- NodeClust[,c(1, .x, .x + DaysAhead)] %>%
        set_names(c("NodeID","Day1", "Day2")) %>%
        mutate(val = 1) %>%
        spread(key = Day2, value = val, fill = 0 ) %>%
        select(-NodeID) %>%
        group_by(Day1) %>%
        summarise_all(sum) %>%
        gather(key = "Day2", value = "Nodes", -Day1) %>%
        left_join(Combos,., by = c("Day1", "Day2")) %>%
        mutate(Nodes = ifelse(is.na(Nodes), 0, Nodes))
      
    })
  names(DayTransfer) <- names(nodeclustlist)[-length(nodeclustlist)]
  
  return(DayTransfer)
  
}