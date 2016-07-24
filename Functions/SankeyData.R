#Function Returns a sankey

#### The function returns a two element list that can be used with networkD3's 
# ###sankeydiagram function
# 
# period: the days for which a sankey diagram is required
# 
# Sankey frame: a square matrix that contains the transitions of all the 
# clusters from day to day.
# 
# Clustconversion: a data frame that contains the unique cluster ID's
# and the Cluster ID groups.
#
#Requires: dplyr
SankeyData <- function(period,SankeyFrame,ClusterConversion, simple=TRUE){

#subtracts the last element of period so that the correct total
  #is returned
Sankey2<-lapply(period[-length(period)], function(day)
{x<-SankeyFrame[Clustconversion$day ==day,
                Clustconversion$day ==(day+1)]

x <- x %>% bind_cols(select(Clustconversion[Clustconversion$day
                                            ==(day),], ClustID),.) %>%
  group_by(ClustID) %>% summarise_each(funs(sum)) %>%ungroup

sourcenames <- select(x, ClustID)
x <- x %>% select(-ClustID) %>% t %>% as.data.frame %>% 
  bind_cols(select(Clustconversion[Clustconversion$day 
                                   ==(day+1),], ClustID),.) %>%
  group_by(ClustID) %>% summarise_each(funs(sum)) %>%ungroup

targetnames <- select(x, ClustID)

x %<>% select(-ClustID) %>% t %>% as.data.frame %>% bind_cols(sourcenames,.) %>%
  rename_(.dots=setNames(names(.), c("ClustID",targetnames[[1]])))

x <-x %>%   gather(.,key= target, value=Nodes, -ClustID) %>%
  mutate(source= paste(day,"-",ClustID, sep=""),
         target=paste(day+1,"-",target, sep="")) %>%
  select(-ClustID)
return(x)
}) %>%bind_rows()


#reshape to create a list of unique Cluster Names and their 
#Cluster ID to colour the groups by
SankeyNodes <- Sankey2 %>% select(-Nodes) %>% 
  gather(key=TS, value=UniqueID)%>% 
  group_by(UniqueID) %>%
  summarise(counts=n()) %>% ungroup %>% 
  mutate(ClustID=sub("([0-9]{1,3}-)", "",.$UniqueID)) %>%
  select(-counts) %>% as.data.frame


Sankey2 <- Sankey2%>%
  mutate(source=
           match(Sankey2$source, SankeyNodes$UniqueID)-1,                                    
         target=match(Sankey2$target, SankeyNodes$UniqueID)-1) %>% 
  as.data.frame

Xvect<- unique(SankeyNodes$ClustID)

SankeyColours <-ggplotColours(length(Xvect)) %>%toString() %>%gsub('#','"#',.) %>%gsub(',','",',.) %>%paste('d3.scale.ordinal()
        .domain([',paste('"',Xvect,'"', sep="")%>%toString()  ,'])
        .range([',. ,'\"]);', sep ="")

if(simple==TRUE){
  output <- sankeyNetwork(Links=Sankey2,
                     Nodes=SankeyNodes,
                     NodeID = "UniqueID",
                     Source="source",
                     Target= "target",
                     Value = "Nodes",
                     NodeGroup = "ClustID",
                     fontSize = 0,
                     colourScale = JS(SankeyColours))
  
  
} else{
  
output <- list(Sankey2, SankeyNodes, SankeyColours)
names(output) <- c("Sankey2", "SankeyNodes", "SankeyColours")

}

return(output)
}