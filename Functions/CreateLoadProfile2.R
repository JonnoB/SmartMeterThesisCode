#Creates the load profile required to plot various metrics 
#Using the the ClustID not the Unique CLusters. 
#This means that the means and sums etc return the right values for cluster families
#When there are more than one Unique ID in a cluster on the same day, e.g
#the 1600 cluster or the soup cluster as they can have multiple occurances in the 
#same day.

CreateLoadProfile2 <- function(dayinit, Graphdir=GraphPath, 
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
  
  daydata <- daydata %>% mutate(NodeID = as.character(rownames(.))) %>%
    left_join(., clusters, by="NodeID") %>% 
    select(-NodeID) 
  
  daydata2<- daydata %>% gather(., time, kwh,-ClustID) %>%
    group_by(ClustID,time) %>% 
    summarise_each(funs(sum, n(), mean,median, sd)) %>% ungroup%>% 
    mutate(time= ymd_hms(sub("X","",time)))
  
  
}