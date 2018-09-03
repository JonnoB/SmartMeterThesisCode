MakeClusterProfile <- function(path){
  #This makes the profile for all clusters across all days
  #path: the path to the files containing each day
  
  currentwd <- getwd()
  setwd(path)
  dates <- list.files() %>%sub("date_", "",.)
  
  #Makes a load profile for every single cluster represented in the clusterconversion dataframe... take time to calculate
  
  
  allprofiles<-mclapply(dates, function(n){
    daydata <- CreateLoadProfile(n)
    variable <- "mean" #the metric to use
    print(n)
    
    daydata2 <- daydata %>% 
      select_("ClusterID", "time", variable) %>%
      spread_(., key="ClusterID", value=variable) %>% 
      mutate(time= format(.$time, format="%H:%M:%S"))
    return(daydata2)
  }, mc.cores = detectCores())
  
  #this second bit of code is because I got worried the nodes weren't being linked in the right order
  #creates a loadprofile data frame of all the uniqely named clusters across all time periods
  clusterprofile <- lapply(1:length(allprofiles), function(n){
    dayID <-n
    print(n)
    x <-allprofiles[[dayID]]
    z <-Clustconversion %>% filter(day==dayID)
    names(x)[match(z$oldClustID,names(x))]<-z$UniqueID
    
    return(x)
  }
  ) 
  
  
  clusterprofile2<- clusterprofile
  clusterprofile <- clusterprofile[[1]] %>% select(time)
  
  for (i in 1:length(clusterprofile2)) {
    print(i)
    clusterprofile <- left_join(clusterprofile, clusterprofile2[[i]], by="time")   
  }
  
  setwd(currentwd)
  
  return(clusterprofile)
  
}