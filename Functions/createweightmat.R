createweightmat <- function(filename, cortype ="spearman"){
  datdat<-readRDS(filename) %>%ungroup
  meterIDs <- names(datdat)[-1]
  print("Data Loaded")
  corlist <- datdat[,2:ncol(datdat)] %>% 
    as.matrix %>% 
    bigcor(., nblocks= 3, verbose = TRUE, method = cortype)
  
  print("Corellation complete")
  
  corlist %<>%as.ffdf %>% as.data.frame
  print("Corellation data frame converted")
  
  weightmat <- abs(corlist)
  diag(weightmat) <- 0 #removes the diagonal this isn't necessary as it will actually be done in the graph stage
  weightmat[is.na(weightmat)] <- 0 #removes NA's to prevent automatic edge creation
  names(weightmat) <-make.names(meterIDs)
  weightmat
}