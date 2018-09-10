#Creates the load profile required to plot various metrics
#the profiles created are the unique cluster profiles.
#This allows for the creation of the graph of clusters
 
CreateLoadProfile <- function(date, Graphdir=GraphPath, daydatapath=daytimeseries){
  setwd(Graphdir)
  date <-paste("date_",date,sep="")
  graph3 <- read.graph(paste(date,".graphml",sep=""), format = "graphml")
  
  setwd(daydatapath)
  timeseries <- date
  daydata <- readRDS(timeseries) 
  rownames(daydata) <- daydata[,1] %>% make.names()
  colnames(daydata) <- daydata %>% names %>%make.names()
  
  daydata <- daydata[,-1]%>%t %>% as.data.frame()
  
  x<- daydata[(daydata[,-13])%>% rowSums %>% is.na %>% which,]
  
#  Clustconversion2 <- Clustconversion %>% filter(date==)
  
  clusters <-data.frame(NodeID= get.vertex.attribute(graph3, "name"), 
                        ClusterID = as.character(get.vertex.attribute(graph3, "ClusterID"))) %>%
    mutate(NodeID = as.character(NodeID))# %>% left_join(.,Clustconversion2, )
  
  
  daydata <- daydata %>% mutate(NodeID = as.character(rownames(.))) %>%
    left_join(., clusters, by="NodeID") %>% 
    select(-NodeID) 
  
  daydata2<- daydata %>% gather(., time, kwh,-ClusterID) %>%
    group_by(ClusterID,time) %>% 
    summarise_all(funs(sum, n(), mean,median, sd)) %>% ungroup %>% 
    mutate(time= ymd_hms(sub("X","",time)))
  
  
}