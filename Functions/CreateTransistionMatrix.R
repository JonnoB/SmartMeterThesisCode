CreateTransistionMatrix <- function(DayTransfer, Dates){
  #Creates the transition matrix for prediction
  #DayTransfer: the list of dataframes containing the date pair node tranistion totals across clusters
  #Dates: the date you want to include in the analysis. This is used for cross validation
  
  #creates the transition matrix
  #This can be filtered to allow a transition matrix created from different subsets of the data
  TransistionMat <- DayTransfer[names(DayTransfer) %in% Dates] %>%
    reduce(left_join, by = c("Day1", "Day2")) %>%
    mutate(Total = rowSums(.[,-c(1:2)], na.rm = T)) %>%
    select(Day1, Day2, Total) %>%
    group_by(Day1) %>%
    mutate(Total = Total/sum(Total, na.rm = T)) %>% #normalise the rows
    spread(key = Day2, value = Total, fill = 0)
  
  return(TransistionMat)
  
}