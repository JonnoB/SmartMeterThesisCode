CreateAllProfiles <- function(Clustconversion, daytimeseries, nodeclustlist, StartTime = "00:00:00", EndTime = "24:00:00"){
  #This function creates all the cluster profiles. It is used to work out the names of the profiles
  #across all the clusters 
  #Clustconversion: the dataframe output of the clustconverion function that converts cluster names to unique cluster names
  #daytimeseries: the file path where all the day data is stored
  #nodeclustlist: the list that has the cluster affiliation of each node for each day
  #StartTime: The start time of the period being analysed
  #EndTime: The end time of the period being analysed
  originalwd <- getwd()
  setwd(daytimeseries)
  dates <- list.files() %>% sub("date_", "",.)
  
  #Makes a load profile for every single cluster represented in the clusterconversion dataframe... take time to calculate
  
  BaseTime <- seq(ymd_hms('2014-01-21 00:00:00'), 
                  by = '30 min',length.out=(60*24/30)) %>% 
    strftime(., format="%H:%M:%S", tz = "UTC") %>% 
    data_frame(time = .) %>%
    filter(time >= StartTime, time < EndTime)
  
  
  clusterprofile <- dates  %>% map(~{
    print(.x)
    #make profiles
    daydata <- readRDS(list.files(daytimeseries, .x))
    
    daydata3 <- daydata %>%
      gather(key = NodeID, value = value, -Date.Time) %>%
      left_join(., nodeclustlist[[.x]] %>% mutate(NodeID = sub("X", "", NodeID)), by = "NodeID") %>%
      select(value, ClusterID = cluster, time = Date.Time) %>%
      mutate(time = strftime(time, format="%H:%M:%S", tz = "UTC"),
             ClusterID = as.character(ClusterID)) %>%
      group_by(ClusterID, time) %>%
      summarise_all(mean) %>% ungroup 
    
    #convert to new cluster names
    daydata3 <- daydata3 %>% 
      left_join(., Clustconversion %>%
                  filter(date == .x) %>%
                  select(ClusterID = cluster, UniqueID), by = "ClusterID")
    
    #Ensuring 48 time periods and correct order
    #removes time becuase we know how many time periods there are and what order they are in.
    daydata4 <- left_join(BaseTime, daydata3, by = "time") %>%
      select(-ClusterID) %>%
      spread(key = UniqueID, value ) %>%
      select(-time)
    
    return(daydata4)
    
  }) %>%
    bind_cols() %>%
    bind_cols(BaseTime,.)
  
  setwd(originalwd)
  return(clusterprofile)

  }

