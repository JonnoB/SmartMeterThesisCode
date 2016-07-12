#This function simply fixes the problems in date time of GMT at
#Europe/London between


filterTime<-function(df, datasource,StartHour=16, EndHour=21, segments=500){
  names(df)<-df %>% names %>% make.names
  #rename for ease of later processing
  df %<>% rename(Date.Time = Date.and.Time.of.capture)
  
  #add in a column for datasource
  df <- df %>% mutate(datasource = datasource$Data.provider[
  match(df$Location.ID, 
        datasource$Location.ID)
  ])

#create vector of columns to use so that it doesn't crash
rowIDs <-seq.int(from = 0, to=nrow(df), length.out = segments) %>% round


test<-lapply(1:(length(rowIDs)-1), function(n){
  print(n)
  rows <-(rowIDs[n]+1):rowIDs[n+1] 
  smartTrill <- df[rows,] %>% filter(datasource=="Trilliant")
  smartTrill%<>%
    mutate(Date.Time = dmy_hms(Date.Time, tz="Europe/London"), 
           hour = hour(Date.Time)) %>%
    filter(hour >= StartHour, hour<=EndHour)
  
  smartLog <- df[rows,] %>% filter(datasource=="Logica")
  smartLog%<>%
    mutate(Date.Time = dmy_hms(Date.Time, tz="GMT") %>%
             with_tz(.,tz ="Europe/London"), 
           hour = hour(Date.Time)) %>%
    filter(hour >= StartHour, hour<=EndHour)
  
  bind_rows(smartTrill,smartLog)
  }
  ) %>% bind_rows(.) %>%select(-datasource)

return(test)
}
