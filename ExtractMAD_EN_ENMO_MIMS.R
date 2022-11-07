# own function to calculate EN, MAD, ENMO
# Euclidean Norm
# Euclidean Norm minus One
# Mean Aplitude Deviation in epochs of 5 seconds (n seconds)


#### FUNCTION get Epochs - creates a vector with indices per epoch   
splitDataInEpochs<- function (ACCData, EpochLength) {
  start<- ACCData$HEADER_TIME_STAMP [1]
  end <- ACCData$HEADER_TIME_STAMP [length(ACCData$HEADER_TIME_STAMP)]
  
  epochsNeeded<- floor(as.numeric(end-start)/ EpochLength )
  
  indices<- numeric()
  indices [1] <- 1
  EpochStart <- start #first timepoint starts
  
  for (j in 1:epochsNeeded){
    index1<- which (ACCData$HEADER_TIME_STAMP < EpochStart + EpochLength)
    index2<- which (ACCData$HEADER_TIME_STAMP [index1] > EpochStart)
    
    indices [index2] <- j
    EpochStart <- EpochStart + EpochLength
  }
  return(indices)
}
  

#FUNCTION to get MAD, EN, ENMO
#enter epochLengthMAD as numeric (in seconds)
#enter epochLengthMIMS as character (eg '10 min' or '10 sec')

extractENMO_MAD <- function (dataInMIMSFormat, epochLengthMAD, epochLengthMIMS) {
  
  # use MIMS_DF_Input data format (as formatted in ExtractMIMS.R script)
  
  acc_x <- dataInMIMSFormat$X 
  acc_y <- dataInMIMSFormat$Y 
  acc_z <- dataInMIMSFormat$Z 
  
  #calculate EN and ENMO per input second -- this is also needed for MAD later on
  
  EN <- sqrt((acc_x^2 + acc_y^2 + acc_z^2)) #per input second
  ENMO <- EN -1 #account for gravity
  
  #run MAD function on Input Data and use Indices from split Data Function to navigate
  #MAD = \frac{1}{n}Σ|r_i - \overline{r}|
  start<- dataInMIMSFormat$HEADER_TIME_STAMP [1]
  end <- dataInMIMSFormat$HEADER_TIME_STAMP [length(dataInMIMSFormat$HEADER_TIME_STAMP)]
  
  MAD <- numeric()
  for (k in 1: floor(as.numeric(end-start)/ EpochLengthMAD )) {
    split<- which(indices == k)
    meanEN <- mean(EN [split])
    deviationPerI <- abs(EN [split] - meanEN)
    sumDeviation <- sum(deviationPerI)
    MAD [k] <- sumDeviation / epochLengthMAD
  }
  
  #seems to give a negative value if there is a data problem?? 
  MIMS<- MIMSunit::mims_unit(dataInMIMSFormat, dynamic_range=c(-3,3), epoch= epochLengthMIMS)
  
}



## TODO: package this nicely into two functions, 
# returning a full data.frame with EN and ENMO 
# returning per entered dataset and epoch length a vector of MAD values






