get24hourcounts_21 <- function (clean21Data) {
  splitdat21<-split(clean21Data, clean21Data$ID)
  PerPerson21_doubles<- sapply(splitdat21,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )
  ## manual cleaning and summing as some entries are capitalized and otheres are not....
  PerPerson21<- rbind ( rowSums(cbind(PerPerson21_doubles [1,], PerPerson21_doubles [6,]), na.rm= TRUE ), #Cycling/ cycling
                        rowSums(cbind(PerPerson21_doubles [2,], PerPerson21_doubles [7,]), na.rm= TRUE ), #Inactive/ inactive
                        rowSums(cbind(PerPerson21_doubles [3,], PerPerson21_doubles [8,]), na.rm= TRUE ), #Light/ light
                        rowSums(cbind(PerPerson21_doubles [4,], PerPerson21_doubles [9,]), na.rm= TRUE ), #Not/not on wrist
                        rowSums(cbind(PerPerson21_doubles [5,], PerPerson21_doubles [10,]), na.rm= TRUE )) #Sleeping/sleeping
  row.names(PerPerson21) <- c("Cycling", "Inactive", "Light", "NotOnWrist", "Sleeping")
  
  return (PerPerson21)
  
}

removeEmptyEntries19Data <- function (subpartClean19Data) {
  indexNA<- which(subpartClean19Data$Time == "")
  ifelse(any(subpartClean19Data$Time == "-"),
         indexNA<- c(indexNA,which(subpartClean19Data$Time == "-")),
         indexNA<-indexNA)
  removedNA<- subpartClean19Data [-indexNA,]
  return(removedNA)
}

get24hourcounts_19 <- function (clean19Data) {
  emptyEntriesRemoved<- lapply(clean19Data, removeEmptyEntries19Data)
  perPersonList<- sapply(emptyEntriesRemoved, function (x) {tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) )} )
  
  perPerson<- matrix(ncol=11, nrow=7)
  
  for (i in 1:length(perPersonList)) {
    counts <- numeric(length = 7)
    counts [ as.numeric(dimnames(perPersonList[[i]])[[1]]) ] <- perPersonList[[i]]
    perPerson[,i] <- counts
  }
  
  #remove row 3 and 7 - HEAVY is empty and 7 is "no Data" an artifact from data entry
  perPerson <- perPerson[-c(3,7),]
  
  IDs <- sapply(emptyEntriesRemoved, function (x) x$ID[1] )
  #add ID as colnames and activitynames as rownames
  colnames(perPerson) <- IDs
  
  rownames(perPerson) <- c("Inactive", "Light", "Sleeping", "NotOnWrist", "Cycling")
  
  return(perPerson)
}

#normalize to 30 min (analogue to MEDLO data)
normalize24hrs30min<- function (PerPersonOverview){
  
  TotalObservedMinutes<- colSums(PerPersonOverview,na.rm = TRUE)
  
  #Normalized24hrs21 <- matrix(nrow=5, ncol = ncol(PerPersonOverview))
  Normalized24hrsTo30min<- matrix(nrow=5, ncol = ncol(PerPersonOverview))
  for (i in 1: length(PerPersonOverview[1,])) {
    # Normalized24hrs21 [,i] <- round(as.numeric(PerPersonOverview [,i]/ TotalObservedMinutes21 [i]),3)
    Normalized24hrsTo30min [,i] <- round(as.numeric(PerPersonOverview [,i]/ TotalObservedMinutes [i]),3) * 30
  }
  
  colnames(Normalized24hrsTo30min) <- colnames(PerPersonOverview)
  rownames(Normalized24hrsTo30min) <- rownames(PerPersonOverview)
  
  return (Normalized24hrsTo30min)
}

















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

###! CLEAN DATA by HAND!! - The "No Data" category is an artifact of data entry in 2019. 
#To "fill" the minutes between 9 hrs and 15 hrs, the assistant entering the data added a "No Data"
#category. Those times are before or after the data collection, for example if a resident slept longer
#or didnt want to wear the watch anymore. Here, we are removing this category manually. 

ActivityDat19<- ActivityDat19[- which(ActivityDat19$Activity == 7), ]

#### More artifacts of data collection: Whenever category "5" (not on wrist) is the first entry of a 
# data collection day or the second entry after "2" (light) --> remove both data points. Light activity 
# is the switching on of watch and moving to prepare watches/finding the resident; "not on wrist" is 
# data points while the watch is on but lies in a basket and not yet fitted.

#remove empty rows - those are times between switching on the watch and putting it on wrist or
#btw removing wristband and switching watch off
ActivityDat19<-ActivityDat19[- which(ActivityDat19$Activity == ""), ]
ActivityDat19<-ActivityDat19[- which(ActivityDat19$Activity == "-"), ]

#which level is which activity? --- see header of this script
levels(ActivityDat19) <- c(1,2,4,5,6,7)
ActivityDat19$Activity <- factor(ActivityDat19$Activity)

# activity summed up per Person
# splitdat19<- split(ActivityDat19, ActivityDat19$ID)


#this script removes entries of category 5 as first entry or as second entry of the activities
#this is an artifact of the data collection with the wearables being already on while not yet on the residents' wrists
#
splitdataPerPerson19<- split(ActivityDat19, ActivityDat19$ID)

for (j in 1: length(splitdataPerPerson19)) {
  splitPerDate<- split(splitdataPerPerson19 [j][[1]], splitdataPerPerson19 [j] [[1]]$Date )
  
  for (i in 1:length(splitPerDate) ) {
    if(length(splitPerDate[i][[1]]$Activity) == 1) {
      ifelse(splitPerDate[i][[1]]$Activity [1] == 5,
             splitPerDate[i][[1]]$Activity <- NA,#replace,
             splitPerDate[i][[1]] <- splitPerDate[i][[1]]);
      next
    } 
    
    check<-0
    check<- splitPerDate[i][[1]]$Activity [1] == 5 #turns 1/0
    checkTwo<- splitPerDate[i][[1]]$Activity [2] == 5 #turns 1/0
    
    ifelse(checkTwo == TRUE, check<-2, check<-check)
    
    if(check == 1) {
      splitPerDate[i][[1]] <- splitPerDate[i][[1]] [-1,] }
    if(check == 2) {
      splitPerDate[i] [[1]]<- splitPerDate[i][[1]] [-c(1,2),] }
    
    else{splitPerDate[i] [[1]]<- splitPerDate[i][[1]]}
    
  }
  unsplitvector<- unlist(sapply(splitPerDate, function (j) j [1]$Date))
  splitdataPerPerson19 [j][[1]] <- unsplit(splitPerDate, unsplitvector)
  
  ifelse(any (is.na(splitdataPerPerson19 [j][[1]]$Activity)),
         splitdataPerPerson19 [j][[1]] <- splitdataPerPerson19 [j][[1]] [- which(is.na(splitdataPerPerson19 [j][[1]]$Activity)), ],#rmv NA rows,
         splitdataPerPerson19 [j][[1]] <- splitdataPerPerson19 [j][[1]])
  
}

#this gives an overview per ID and per factor of Activity
#ID in columns, activity in rows
PerPerson19<- sapply(splitdataPerPerson19,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )

#old version on un-cleaned data (i.e. including the "not on wrist" at the beginning of data collection)
#PerPerson19<- sapply(splitdat19,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )

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
  KeyDat21<- read.csv("Data24hr/completeKeyfileWatches.csv",header=TRUE) #this file cannot be shared as there are names in this file
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

Dat10[
  order( Dat10[,5],Dat10[,1], Dat10[,2] ),
]
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
# 7 No Data (either at begin or end of a data collection) 
##################

#transform into per Person counts: 
# activity summed up per Person
splitdat21<- split(DataActive21, DataActive21$ID)
#this gives an overview per ID and per factor of Activity
#ID in columns, activity in rows
PerPerson21<- sapply(splitdat21,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )



#################

#TotalObservedMinutes21<- colSums(PerPerson21,na.rm = TRUE)

#Normalized24hrs21 <- matrix(nrow=5, ncol = ncol(PerPerson21))
#Normalized24hrsTo30min21<- matrix(nrow=5, ncol = ncol(PerPerson21))
#for (i in 1: length(PerPerson21[1,])) {
#  Normalized24hrs21 [,i] <- round(as.numeric(PerPerson21 [,i]/ TotalObservedMinutes21 [i]),3)
#  Normalized24hrsTo30min21 [,i] <- round(as.numeric(PerPerson21 [,i]/ TotalObservedMinutes21 [i]),3) * 30
#}

#colnames(Normalized24hrs21) <- colnames(PerPerson21)
#colnames(Normalized24hrsTo30min21) <- colnames(PerPerson21)
#rownames(Normalized24hrs21) <- rownames(PerPerson21)
#rownames(Normalized24hrsTo30min21) <- rownames(PerPerson21)

###########
#TotalObservedMinutes19<- colSums(PerPerson19,na.rm = TRUE)

#Normalized24hrs19 <- matrix(nrow=6, ncol = ncol(PerPerson19))
#Normalized24hrsTo30min19<- matrix(nrow=6, ncol = ncol(PerPerson19))
#for (i in 1: length(PerPerson19[1,])) {
#  Normalized24hrs19 [,i] <- round(as.numeric(PerPerson19 [,i]/ TotalObservedMinutes19 [i]),3)
#  Normalized24hrsTo30min19 [,i] <- round(as.numeric(PerPerson19 [,i]/ TotalObservedMinutes19 [i]),3) * 30
#}
#colnames(Normalized24hrs19) <- colnames(PerPerson19)
#colnames(Normalized24hrsTo30min19) <- colnames(PerPerson19)
#rownames(Normalized24hrs19) <- rownames(PerPerson19)
#rownames(Normalized24hrsTo30min19) <- rownames(PerPerson19)

############

#check how we can bring 21 and 19 together into one database while keeping the Activity Levels and 
#linking it to accelerometer data

#!!! Activity Level Names might differ in 19 and 21 dataset?? Double check!!
#Tripple check the Level Names, especially the "No Data" category -- No Data category seems to be only in 2019 
# --> artifact from data collection to fill days from 9 am to 17 hrs


#Visualize Activity Data

dat24hrs21 <- data.frame(
  
  MovementCategory = factor(c(rownames(Normalized24hrsTo30min21),3), #add category 3 and 7 as NA row 
                            levels=c("1","2", "3", "4", "5", "6")),
  
  TimeSums = c(rowSums(Normalized24hrsTo30min21, na.rm = TRUE), 0 ) #category 3 and 7 has 0 entries in this data
)

dat24hrs19 <- data.frame(
  
  MovementCategory = factor(c(rownames(Normalized24hrsTo30min19),3), #add category 3 and 7 as NA row 
                            levels=c("1","2", "3", "4", "5", "6")),
  
  TimeSums = c(rowSums(Normalized24hrsTo30min19, na.rm = TRUE), 0 ) #category 3 and 7 has 0 entries in this data
)

all24hrs<- rbind(dat24hrs21, dat24hrs19)


#the levels and labels are sorted manually to get the colors correctly mapped
dat24hrs <- data.frame(
  MovementCategory = factor(c(rownames(Normalized24hrsTo30min19),3), 
                            levels=c("1","2", "4", "5", "6", "3", "7"),
                            labels = c( "Inactive","Light", "Heavy", "Sleeping",
                                        "Not on wrist", "No Data ", "Cycling") ),
  
  TimeSums = c(tapply(as.numeric(all24hrs$TimeSums), all24hrs$MovementCategory, function(x) sum (x, na.rm=TRUE)),0 )
)
  
  
#add manually matching labels/ legend is off!!

p24hrs <- ggplot(data=dat24hrs, aes(x=MovementCategory, y=TimeSums, fill=MovementCategory)) +
  
  geom_bar(stat="identity")

p24hrs +  theme_bw() + scale_fill_discrete(labels=c("Inactive", "Light", "Heavy", "Sleeping",
                                                    "Not on Wrist", "Cycling", "No Data"))
  


#CODES:
# 1 Inactive
# 2 Light
# 3 Heavy
# 4 Sleeping
# 5 Not on wrist
# 6 Cycling
# 7 No Data

########################
##Clean Version - Remove the empty "No Data" category -- those data points are an artifact of data entry.

#the levels and labels are sorted manually to get the colors correctly mapped
dat24hrs <- data.frame(
  MovementCategory = factor(c(rownames(Normalized24hrsTo30min19),3), 
                            levels=c("1","2", "4", "5", "6", "3"),
                            labels = c( "Inactive","Light", "Heavy", "Sleeping",
                                        "Not on wrist", "Cycling") ),
  
  TimeSums = tapply(as.numeric(all24hrs$TimeSums), all24hrs$MovementCategory, function(x) sum (x, na.rm=TRUE) )
)


#add manually matching labels/ legend is off!!

p24hrs <- ggplot(data=dat24hrs, aes(x=MovementCategory, y=TimeSums, fill=MovementCategory)) +
  
  geom_bar(stat="identity")

p24hrs +  theme_bw() + scale_fill_discrete(labels=c("Inactive", "Light", "Heavy", "Sleeping",
                                                    "Not on Wrist", "Cycling"))



#CODES:
# 1 Inactive
# 2 Light
# 3 Heavy
# 4 Sleeping
# 5 Not on wrist
# 6 Cycling
# 7 No Data

