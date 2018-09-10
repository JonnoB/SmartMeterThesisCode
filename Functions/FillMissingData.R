FillMissingData <- function(df){
  #fills the missing data for the cleaned and subsetted data set.
  #Filling in missing values prevents NA's when doing the corellation
  #df the data frame to do the filling
  
  dayhourmin <- paste(wday(df$Date.Time),
                      hour(df$Date.Time),
                      minute(df$Date.Time),
                      sep=":")
  
  meanvals <- df[,-1] %>%
    mutate(time.day = dayhourmin) %>% group_by(time.day) %>%
    summarise_all(funs(mean(., na.rm=TRUE))) %>%ungroup
  
  navect <- df %>% is.na %>% which(., arr.ind=T)
  
  NACols <- unique(navect[,2] )
  
  for(i in 1:length(NACols)){
    colID <-NACols[i]
    rowIDs <- navect[navect[,2]==colID,1]
    
    RowsFromMeanVals<- match(dayhourmin[rowIDs],meanvals$time.day)
    
    df[rowIDs,colID] <- meanvals[RowsFromMeanVals,colID] %>%unlist
    if((i%%100)==0){print(i)}  
  }
  
  
  return(df)
}