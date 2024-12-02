#install.packages('MIMSunit')

## Info on how to use the MIMSunit package
#MIMSunit::mims_unit(input_dataframe, dynamic_range=c(-3,3), epoch='1 min')

#Assume the input dataframe is in following format, with the first column (timestamp) in 
#POSXlct objects and the device used to collect this data has dynamic range being -3g to 3g. 
#You may set the epoch length to be 1 min, 1 sec, 5 sec, 10 sec and so on.

#FUNCTION to turn collected Accelerometer Data into a MIMS format/ format for the function to extract MIMS, EN, ENMO, MAD measures

#HEADER_TIME_STAMP,X,Y,Z
#2016-10-03 14:51:14.236,0.007,-0.005,0.984

#transform Acc data into a dataframe that matches input for MIMS
#SampleDat<- read.table("DataAcc/000 aa1d 2021 05 10 18 25 08 aag.dat", header=TRUE,skip=1, sep = ",")

#enter filename with " " eg. "DataAcc/000 aa1d 2021 05 10 18 25 08 aag.dat"

formatAccDat <- function (filename) {
  
  #this throws a warnning message, as last line of the .dat file is not complete.
  SampleDat<- read.table(filename, header=TRUE,skip=1, sep = ",")
  
  #tail(SampleDat)
  #remove last row (incomplete measurement)
  SampleDat <- SampleDat [-nrow(SampleDat), ] #Does this happen in every file?
  
  #use only four first columns of dataframe (rest is gyroscope, GPS data, privacy info):
  SampleDat<- SampleDat[,1:4]
  
  #add starttime from filename
 
  filename_split<-strsplit(filename, " ")
  #6,7,8 are the entries for start time in the filename
  starttime<- paste(filename_split [[1]] [6], filename_split [[1]] [7], filename_split [[1]] [8], sep = ":")
  startdate<- paste(filename_split [[1]] [3], filename_split [[1]] [4], filename_split [[1]] [5], sep = "-")
  fullStart<- paste(startdate, starttime)
  
  #to see the milliseconds, this needs to be set (set outside of function/when calling the function???)
  options("digits.secs"=6) 
  
  HEADER_TIME_STAMP <- as.POSIXct (fullStart) + SampleDat[,1]
  X <- SampleDat[,2]
  Y <- SampleDat[,3]
  Z <- SampleDat[,4]
  
  
  MIMS_DF_Input<- data.frame(HEADER_TIME_STAMP, X, Y, Z)
  
  return (MIMS_DF_Input)
}



