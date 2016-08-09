# Creates a matrix of the mean load profile of each cluster for the days
# in the date vector

DayProfilemat <-function(datevect, AllClusts){
DayProfile <- mclapply(datevect, function(n){
  
  x <-CreateLoadProfile2(n) %>%
    mutate(date=as.Date(time), 
           time= format(time, format="%H:%M")) %>%
    select(ClustID, date, time, mean) %>%
    spread(key= time, value=mean)
  return(x)
}, mc.cores=detectCores()
) %>% bind_rows()


#ensure that all cluster types are represented, 
#filling missing clusters with 0 for that day
DayProfile <- data.frame(ClustID= rep(AllClusts$ClustID, times=length(datevect)), 
                         date= rep(datevect, each=10)) %>%
  left_join(., DayProfile, by=c("ClustID", "date"))

DayProfile[is.na(DayProfile)] <-0

return(DayProfile)
}