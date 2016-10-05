
jaccard <- function(m) {
  ## common values:
  A = tcrossprod(m)
  ## indexes for non-zero common values
  im = which(A > 0, arr.ind=TRUE)
  ## counts for each row
  b = rowSums(m)
  
  ## only non-zero values of common
  Aim = A[im]
  
  ## Jacard formula: #common / (#i + #j - #common)
  J = sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  
  return( J )
  #this code was taken from http://stats.stackexchange.com/questions/49453/calculating-jaccard-or-other-association-coefficient-for-binary-data-using-matri
}

library(dplyr); library(tidyr)
setwd("~/")
SAP <- read.csv("table.csv")
SAP_DF <- (SAP[,"Close"])
SAP_DF_1 <-data.frame(Y=as.matrix(SAP_DF), Closing_date=as.Date(SAP$Date)) %>% arrange(Closing_date)

mvalue = cpt.mean(SAP_DF_1$Y, method="PELT") 
plot(x=mvalue, cpt.width=3, cpt.col='red', main="Changepoint detection", sub="SAP Stock", xlab="Date", ylab="Stock price in $")


#make a vector that includes row one
rowvect <- c(1, mvalue@cpts)
#create a dataframe which has the mean level for each time period
SAP_DF_change <- lapply(1:length(mvalue@cpts), function(n){
  #Create a vector of the closing dates 
  temp <- SAP_DF_1$Closing_date[rowvect[n]:rowvect[n+1]]
  #Create a data frame that matches date and mean
  out <-data.frame(Closing_date = temp , Y = rep(mvalue@param.est$mean[[n]], length(temp)))
  out
  }) %>% bind_rows()

#Bind both dataframes together including a reference to which type of line they are
SAP_DF_2 <- bind_rows(mutate(SAP_DF_1, type="Close"), mutate(SAP_DF_change, type= "Mean"))

#Plot the lines colour by type
ggplot( SAP_DF_2, aes( x= Closing_date, y= Y, colour = type)) + geom_line() 
