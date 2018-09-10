LoadAllSmartMeterData <-function(SmartMeterPath, TimeZonePath){
  #Loads the smart meter data correcting for the different time stamps
  #SmartMeterPath: Path to the smart meter file
  #TimeZonePath: Path to the time zone info
  
  print("Loading Time Zone Data")
  #Load smart meter type data for timezone conversion
  TimeZoneDF <- read.csv(TimeZonePath, check.names = FALSE) %>% 
    mutate(TimeZoneCol = ifelse(`Data provider` == "Logica", "GMT", "Europe/London")) %>%
    select(-"Data provider")
  
  #Load smart meter data
  print("Loading All smart meter data. This takes a while")
  SmartmeterData.raw <- read_csv(SmartMeterPath)
  
  #Fix time periods
  print("Fixing time zones and converting to datetime object")
  SmartmeterData.raw <- SmartmeterData.raw %>% mutate(`Location ID` = as.character(`Location ID`)) %>%
    left_join(., mutate(TimeZoneDF, 
                        `Location ID` = as.character(`Location ID`)), 
              by= "Location ID") %>% 
    ConvertToTime(., "Date and Time of capture")  %>%
    select("Location ID", Date.Time = "Date and Time of capture", Parameter)
  
  
  # print("Spreading data")
  # #Spread smart meter data. This greatly reduces the size of the files
  # smartdata <- dcast(SmartmeterData.raw, Date.Time ~`Location ID`, 
  #                    value.var = "Parameter", 
  #                    drop=FALSE)

  #return(smartdata)
  
  return(SmartmeterData.raw)
  
  
}