#import Data

#Categories "24hrs log": Per resident and minute of recording, 
#we have one of seven categories recorded. Per resident and 
#category, we extract how many minutes of each category was 
#recorded and normalize this count by the total recording 
#length. This results in seven scores per resident.

##########################
##########################
#CODES:
# 1 Inactive
# 2 Light
# 3 Heavy
# 4 Sleeping
# 5 Not on wrist
# 6 Cycling
# 7 No Data

###########################
###########################

ActivityDat19<-  read.csv("Data24hr/LongFormat-Table 1.csv",header=FALSE)
ActivityDat19 <- ActivityDat19 [,-c(6,7)] #redundant empty columns
colnames(ActivityDat19) <- c("Date", "Time", "Activity", "Minutes", "ID")

#remove empty rows - those are times between switching on the watch and putting it on wrist or
#btw removing wristband and switching watch off
ActivityDat19<-ActivityDat19[- which(ActivityDat19$Activity == ""), ]
ActivityDat19<-ActivityDat19[- which(ActivityDat19$Activity == "-"), ]

#which level is which activity? --- see header of this script
levels(ActivityDat19) <- c(1,2,4,5,6,7)
ActivityDat19$Activity <- factor(ActivityDat19$Activity)

# activity summed up per Person
splitdat19<- split(ActivityDat19, ActivityDat19$ID)
#this gives an overview per ID and per factor of Activity
#ID in columns, activity in rows
PerPerson19<- sapply(splitdat19,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )

#################################
#################################
library(lubridate)

#this function takes a .csv entry and returns an r object  with Information as needed for further analysis:
# i.e., per day one matrix with PatientID in the columns and minutes of activity levels in rows

#"Data24hr/ActivityLog_20210514.csv"

transformData21<- function (csvfile) {
  ActivityDat21<- read.csv(csvfile,header=TRUE)
  ActivityDat21$Date <- as.Date(as.character(ActivityDat21$Date), format='%Y%m%d')
  
  ## to turn time into Posixlt -- fuse date and time.start/time.end and 
  # as.POSIXct(ActivityDat21$Time.Start,tz= "", format = "%H:%M")
  
  ActivityDat21$Start<- as.POSIXct(paste(ActivityDat21$Date, ActivityDat21$Time.Start, " "), 
                                   format = "%Y-%m-%d %H:%M")
  ActivityDat21$End<- as.POSIXct(paste(ActivityDat21$Date, ActivityDat21$Time.End, " "), 
                                 format = "%Y-%m-%d %H:%M")
  
  #Remove empty rows - either empty rows from the excel file or rows with "not on wrist" without end time 
  #-- i.e. times after removing or before placing of wristband
  if (any(ActivityDat21$Time.End == "")) {
  ActivityDat21 <- ActivityDat21 [- which(ActivityDat21$Time.End == ""), ]}
  ActivityDat21 <- ActivityDat21 [- which(is.na (ActivityDat21$End)), ]
  
  #merge with keyfile info to get the ID
  KeyDat21<- read.csv("Data24hr/completeKeyfileWatches.csv",header=TRUE)
  #change and add date notation
  dayonly<-sapply(strsplit(as.character(KeyDat21$Date), "202105"),function (x) x[2])
  newDate<- paste0("2021-05-",dayonly)
  KeyDat21$Date<- as.POSIXct(newDate)
  KeyDat21$dayonly<- dayonly
  # Date + IPaddress are keys to fuse the two datasets 
  # (we didnt use the same watch twice on one day- i.e. "Timepoint" can be ignored as a key)
  # add ParticipantID column to ActivityDat21
  
  #add those cols to Activity Data for merging 
  ActivityDat21$MacWatchID<- ActivityDat21$IP.address #create a key variable
  ActivityDat21$dayonly <- sapply(strsplit(as.character(ActivityDat21$Date), "2021-05-"),function (x) x[2])
  #only use subset of Keyfile:
  subsetKeys<- KeyDat21[which(KeyDat21$dayonly == ActivityDat21$dayonly [1]), ]
  combined24hrsDat21<-merge(ActivityDat21, subsetKeys, by=c("MacWatchID","dayonly"))
  
  Date<- combined24hrsDat21$Date.x
  Time <- paste(combined24hrsDat21$Time.Start,combined24hrsDat21$Time.End, sep= "-" )
  Activity <- as.factor(combined24hrsDat21$Activity) # rename levels once all data is transformed
  Minutes<- combined24hrsDat21$End - combined24hrsDat21$Start #gives activity in minutes
  ID <- combined24hrsDat21$PatientID #
  
  returnData<- data.frame( Date, Time,Activity,Minutes, ID)
  
  return (returnData)
}

Dat10<- transformData21 ( "Data24hr/ActivityLog_20210510.csv")
Dat11<- transformData21 ( "Data24hr/ActivityLog_20210511.csv")
Dat12<- transformData21 ( "Data24hr/ActivityLog_20210512.csv")
Dat13<- transformData21 ( "Data24hr/ActivityLog_20210513.csv")
Dat14<- transformData21 ( "Data24hr/ActivityLog_20210514.csv")

DataActive21<- rbind(Dat10, Dat11, Dat12, Dat13, Dat14)

#Levels:  cycling inactive light not on wrist sleeping
# rename levels into 1 - 7
# current: "Cycling"      "Inactive"     "Light"        "Not on wrist" "Sleeping"     "cycling"      "inactive"     "light"       
# "not on wrist" "sleeping"  
# needs to be fixed manually bc of different ways of entering data in datasheets.

levels(DataActive21$Activity) <- c ("6",  "1", "2", "5", "4", "6" , "1", "2", "5", "4")

#CODES:
# 1 Inactive
# 2 Light
# 3 Heavy
# 4 Sleeping
# 5 Not on wrist
# 6 Cycling
# 7 No Data
##################

#transform into per Person counts: 
# activity summed up per Person
splitdat21<- split(DataActive21, DataActive21$ID)
#this gives an overview per ID and per factor of Activity
#ID in columns, activity in rows
PerPerson21<- sapply(splitdat21,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )


#################
#normalize to 30 min (analogue to MEDLO data)

TotalObservedMinutes21<- colSums(PerPerson21,na.rm = TRUE)

Normalized21 <- matrix(nrow=5, ncol = ncol(PerPerson21))
NormalizedTo30min21<- matrix(nrow=5, ncol = ncol(PerPerson21))
for (i in 1: length(PerPerson21[1,])) {
  Normalized21 [,i] <- round(as.numeric(PerPerson21 [,i]/ TotalObservedMinutes21 [i]),3)
  NormalizedTo30min21 [,i] <- round(as.numeric(PerPerson21 [,i]/ TotalObservedMinutes21 [i]),3) * 30
}
colnames(Normalized21) <- colnames(PerPerson21)
colnames(NormalizedTo30min21) <- colnames(PerPerson21)

###########
TotalObservedMinutes19<- colSums(PerPerson19,na.rm = TRUE)

Normalized19 <- matrix(nrow=6, ncol = ncol(PerPerson19))
NormalizedTo30min19<- matrix(nrow=6, ncol = ncol(PerPerson19))
for (i in 1: length(PerPerson19[1,])) {
  Normalized19 [,i] <- round(as.numeric(PerPerson19 [,i]/ TotalObservedMinutes19 [i]),3)
  NormalizedTo30min19 [,i] <- round(as.numeric(PerPerson19 [,i]/ TotalObservedMinutes19 [i]),3) * 30
}
colnames(Normalized19) <- colnames(PerPerson19)
colnames(NormalizedTo30min19) <- colnames(PerPerson19)

############

#check how we can bring 21 and 19 together into one database while keeping the Activity Levels and 
#linking it to accelerometer data

#!!! Activity Level Names might differ in 19 and 21 dataset?? Double check!!





