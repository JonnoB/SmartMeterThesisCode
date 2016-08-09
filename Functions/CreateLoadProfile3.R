#Creates the load profile required to plot various metrics
#Makes the data required for a hairplot of a cluster at a specific time

CreateLoadProfile3 <- function(dayinit, Graphdir=GraphPath, 
                               daydata=daytimeseries,
                               Clustconv =Clustconversion){
  
  Clustconv<- Clustconversion %>% filter(date==dayinit) %>% 
    select(ClustID,oldClustID)
  
  setwd(Graphdir)
  daydate <-paste("date_",dayinit,sep="")
  graph3 <- read.graph(paste(daydate,".graphml",sep=""), format = "graphml")
  
  setwd(daydata)
  timeseries <- daydate
  daydata <- readRDS(timeseries) 
  rownames(daydata) <- daydata[,1] %>% make.names()
  colnames(daydata) <- daydata %>% names %>%make.names()
  
  daydata <- daydata[,-1]%>%t %>% as.data.frame()
  
  x<- daydata[(daydata[,-13])%>% rowSums %>% is.na %>% which,]
  
  clusters <-data.frame(NodeID= get.vertex.attribute(graph3, "name"), 
                        oldClustID = as.character(get.vertex.attribute(graph3, "ClusterID"))) %>%
    mutate(NodeID = as.character(NodeID)) %>% 
    left_join(.,Clustconv, by="oldClustID" )%>% select(-oldClustID)
  
  daydata2 <- daydata %>% mutate(NodeID = as.character(rownames(.))) %>%
    left_join(., clusters, by="NodeID") 
  
  daydata2<- daydata2 %>% gather(., time, kwh,-ClustID,-NodeID) %>% 
    mutate(time= ymd_hms(sub("X","",time)))
  
  
}