#handle wearable data as collected with Samsung/Tizen/WEARDA package
#EXAMPLE: 000 aa1d 2021 05 10 18 25 08 aag.dat
#all collected data is in .dat format
# 000 first three entries are the participant ID
# aa1d is the device ID
# yyyy mm dd 
# hh mm (start of data collection)
# aag -- accelerometer data
# bar -- barometer data
# con -- 
# gps -- GPS data

######
#Data is stored in batches
#per batch several different measures and differently many participants are included in one batch

# get all filenames
files_from_batch<- list.files("DataAcc") #add path here - will be the path to the data folder on the server
# filter those files that have aag.dat at the end
filterAAG<-sapply(strsplit(files_from_batch, " "), function (x) x [9] == "aag.dat" )

# run the functions to get measurements on those data
datafiles<- paste0("DataAcc/",files_from_batch [filterAAG]) #sorted by participant ID!

# run this on all datafiles and store
ENMO_MAD <- list ()
residentID<- character ()
startAcc<-as.POSIXct(numeric())
endAcc<-as.POSIXct(numeric())
for (i in 1:length(datafiles)) {
  AccDat<-formatAccDat(datafiles [i]) 
#  ENMO_MAD [[i]] <- extractENMO_MAD(AccDat, 60, '30min')
  residentID [i] <- strsplit(strsplit(datafiles [i], " ") [[1]] [1], "/") [[1]][2]
  startAcc [i] <- AccDat$HEADER_TIME_STAMP[1]
  endAcc [i] <- AccDat$HEADER_TIME_STAMP[length (AccDat$HEADER_TIME_STAMP )]
}



