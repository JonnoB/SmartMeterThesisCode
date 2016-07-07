ggplot_missing <- function(x){
  
  x[,-1] <- x[,-1] %>% is.na
  x%<>%    gather(.,key, value, -Date.Time) %>% 
    mutate(value = value*1, Date.Time =as.numeric(Date.Time) %>% rank)
  x %>%
    ggplot(.,
           aes(x = as.numeric(Date.Time),
               y = key,
               fill = as.factor(value))) +
    geom_raster()  +
    scale_fill_manual(values = c("red","blue", "green"), name = "",
                      labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5), axis.text.y = element_blank()) +    
    labs(x = "Date time",
         y = "Smartmeter ID")
  
  #Some adaptation but mostly taken from http://www.r-bloggers.com/ggplot-your-missing-data/
}
