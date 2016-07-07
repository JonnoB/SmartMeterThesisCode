#This function breaks up the usable clusters into individual 
#day files for ease of handling

saveCluster <- function(cluster){
  
  uniquedates <- date(cluster$Date.Time) %>% unique
  total <- length(uniquedates)  
  for (i in 1:total){
    cluster %>% filter(date(Date.Time) == uniquedates[i]) %>%
      saveRDS(., paste("date_",sub("-", "_",uniquedates[i]),".rds"))
    print(paste(i, "of",total ))
  }
  
}