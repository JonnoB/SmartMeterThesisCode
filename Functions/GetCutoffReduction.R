GetCutoffReduction <- function(daydatapath = daytimeseries, StartTime = "00:00:00", EndTIme = "24:00:00", samples = 10, seed = 1372){
  #Randomly samples certain days and finds the number of edges and nodes in a graph with a given cutoff
  #daydatapath: the path where the data is stored
  #StartTime: beginning of time period of interest
  #EndTime: End of time period of interest
  #samples: number of days to check
  #seed: the random seed to be used in the experiment
  
  cutoff <- data.frame(cutoff = c(0,seq(0.1,1,0.05)), edges = NA,nodes=NA)
  
  set.seed(seed)
  files <-sample(list.files(path = daydatapath, full.names = T), samples)
  
  x <-map_df(1:length(files), ~{
    print(.x)
    datdat<-readRDS(files[.x]) %>% 
      as.tibble %>% 
      filter(hms(sub("^.*\\s", "", as.character(Date.Time)))>=hms(StartTime), 
             hms(sub("^.*\\s", "", as.character(Date.Time)))<=hms(EndTIme))
    cormat <- datdat[,2:ncol(datdat)] %>%
      as.matrix %>% cor
    diag(cormat) <- 0
    
    cutoff[,-1] <- map_df(cutoff$cutoff ,~{
      logicmat<-cormat>.x
      c(sum(logicmat, na.rm = TRUE),
        sum(rowSums(logicmat,na.rm=TRUE)>0)
      ) %>% 
        t %>% as.data.frame
    })
    
    return(cutoff)
  })
  
  GetGraphCutoffs <- function(df, type){
    df %>% select(cutoff, type = type)%>% 
      group_by(cutoff) %>%
      summarise(value = mean(type),stdev = sd(type)) %>%
      mutate(upper=value+stdev, lower= value-stdev) %>% ungroup %>% 
     # gather(., key=Average, value, -cutoff, -(stdev:lower)) %>% 
      mutate(
        lower = lower/max(upper),
        value = value/max(upper), 
        upper=upper/max(upper),
        type = type) #done last for obvious reasons
  }
  
  CutoffReduction <-c("edges", "nodes") %>% 
    map_df(~{ GetGraphCutoffs(x, .x)})
}
