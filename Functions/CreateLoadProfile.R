CreateLoadProfile <- function(date, Graphdir=GraphPath, daydata=daytimeseries){
  setwd(Graphdir)
  date <-paste("date_",date,sep="")
  graph3 <- read.graph(paste(date,".graphml",sep=""), format = "graphml")
  
  setwd(daydata)
  timeseries <- date
  daydata <- readRDS(timeseries) 
  rownames(daydata) <- daydata[,1] %>% make.names()
  colnames(daydata) <- daydata %>% names %>%make.names()
  
  daydata <- daydata[,-1]%>%t %>% as.data.frame()
  
  x<- daydata[(daydata[,-13])%>% rowSums %>% is.na %>% which,]
  
  clusters <-data.frame(NodeID= get.vertex.attribute(graph3, "name"), 
                        ClusterID = as.character(get.vertex.attribute(graph3, "ClusterID")))
  
  
  daydata <- daydata %>% mutate(NodeID = as.character(rownames(.))) %>%
    left_join(., clusters, by="NodeID") %>% 
    select(-NodeID) 
  
  daydata2<- daydata %>% gather(., time, kwh,-ClusterID) %>%
    group_by(ClusterID,time) %>% 
    summarise_each(funs(sum, n(), mean,median, sd)) %>% ungroup%>% 
    mutate(time= ymd_hms(sub("X","",time)))
  
  
}