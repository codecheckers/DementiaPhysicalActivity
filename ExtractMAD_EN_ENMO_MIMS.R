# own function to calculate EN, MAD, ENMO
# Euclidean Norm
# Euclidean Norm minus One
# Mean Aplitude Deviation in epochs of 5 seconds (n seconds)


#### FUNCTION get Epochs - creates a vector with indices per epoch   
splitDataInEpochs<- function (dataInMIMSFormat, EpochLength) {
  start<- dataInMIMSFormat$HEADER_TIME_STAMP [1]
  end <- dataInMIMSFormat$HEADER_TIME_STAMP [length(dataInMIMSFormat$HEADER_TIME_STAMP)]
  
  epochsNeeded<- floor(as.numeric(difftime(end,start, units = "secs"))/ EpochLength )
  
  indices<- numeric()
  indices [1] <- 1
  EpochStart <- start #first timepoint starts
  
  for (j in 1:epochsNeeded){
    index1<- which (dataInMIMSFormat$HEADER_TIME_STAMP < EpochStart + EpochLength)
    index2<- which (dataInMIMSFormat$HEADER_TIME_STAMP [index1] > EpochStart)
    
    indices [index2] <- j
    EpochStart <- EpochStart + EpochLength
  }
  return(indices)
}
  

#FUNCTION to get MAD, EN, ENMO
#enter epochLengthMAD as numeric (in seconds)
#enter epochLengthMIMS as character (eg '10 min' or '10 sec')

extractENMO_MAD <- function (dataInMIMSFormat, EpochLengthMAD_ENMO, epochLengthMIMS) {
  
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
  EN_Epochs <- numeric ()
  ENMO_Epochs <- numeric ()
  
  #use Function to get epoch information/ index per epoch
  indices<- splitDataInEpochs (dataInMIMSFormat, EpochLengthMAD_ENMO)
    
  for (k in 1: floor(as.numeric(difftime(end,start, units = "secs"))/ EpochLengthMAD_ENMO ) ) {
    split<- which(indices == k)
    meanEN <- mean(EN [split]) # average EN per epoch
    deviationPerI <- abs(EN [split] - meanEN)
    sumDeviation <- sum(deviationPerI)
    MAD [k] <- sumDeviation / EpochLengthMAD_ENMO
    
    #save per epoch averages of EN and ENMO:
    EN_Epochs[k]  <- meanEN
    ENMO_Epochs [k] <- mean(ENMO [split])
  }
  
  EN_MAD_Measures<- data.frame(MAD, EN_Epochs, ENMO_Epochs)
  
  #seems to give a negative value if there is a data problem?? 
  #MIMS<- MIMSunit::mims_unit(dataInMIMSFormat, dynamic_range=c(-3,3), epoch= epochLengthMIMS)
  return(EN_MAD_Measures)
}



#combine measures per residentID and calculate:
#Mean MAD
#SD MAD
#Mean EMNO
#SD EMNO


#return data.frame with: 
#ResidentID MAD_Mean MAD_SD ENMO_Mean ENMO_SD





