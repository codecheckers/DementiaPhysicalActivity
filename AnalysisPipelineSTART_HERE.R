### Pipeline from Data Import to Correlation Matrices:
library(haven) #to import .dta data

### Import: 
### DataLink LIACS/NIVEL
Link19_21 <- read.csv("DataMedloADL/Link19_21.csv")

### Medlo Data 2021 and 2019

Medlo_2019 <- read_dta("DataMedloADL/Medlo data_2019_def1.dta")
Medlo_2021 <- read_dta("DataMedloADL/Medlo data_2021_def1.dta")
#manually fix a mistake by observer when noting client numbers
Medlo_2019 [which(Medlo_2019 [,8] == "12 P KWA-056-01-01-07"),8] <- "KWA-056-01-01-07"
Medlo_2019 [which(Medlo_2019 [,8] == "11 S KWA-056-01-02-08"),8] <- "KWA-056-01-02-08"



### Accelerometer Data 2021
#Data is stored in batches
#per batch several different measures and differently many participants are included in one batch

# get all filenames
files_from_batch<- list.files("DataAcc") #add path here - will be the path to the data folder on the server
# filter those files that have aag.dat at the end
filterAAG<-sapply(strsplit(files_from_batch, " "), function (x) x [9] == "aag.dat" )
# run the functions to get measurements on those data
datafiles<- paste0("DataAcc/",files_from_batch [filterAAG]) #sorted by participant ID!

## import Accelerometer Times (start end times) for filtering
load("DataAcc1/AccTimesForFiltering.RData") #loads an object called: AccTimesForFiltering




### 24hrs Data 2021 and 2019
#2019:
ActivityDat19<-  read.csv("Data24hr/LongFormat-Table 1.csv",header=FALSE)
ActivityDat19 <- ActivityDat19 [,-c(6,7)] #redundant empty columns
colnames(ActivityDat19) <- c("Date", "Time", "Activity", "Minutes", "ID")
#2021:
Data24hrs_21_10_File<- "Data24hr/ActivityLog_20210510.csv"
Data24hrs_21_11_File<- "Data24hr/ActivityLog_20210511.csv"
Data24hrs_21_12_File<- "Data24hr/ActivityLog_20210512.csv"
Data24hrs_21_13_File<- "Data24hr/ActivityLog_20210513.csv"
Data24hrs_21_14_File<- "Data24hr/ActivityLog_20210514.csv"

### Clean 24 hrs data
clean2021_24hrs_Dat<- clean2021_24hrs (Data24hrs_21_10_File, 
                                       Data24hrs_21_11_File,
                                       Data24hrs_21_12_File,
                                       Data24hrs_21_13_File,
                                       Data24hrs_21_14_File,
                                       AccTimesforFiltering)

clean2019_24hrs_Dat <- clean2019_24hrs ( ActivityDat19)


### Calculate Activity Estimators for Accelerometer Data 2021
### use three different epochs to calculate these estimators (calculate on stand alone PC --- Import Results)

#Import ENMO MAD Averages:
AveragesPerResident_5sec<- ENMO_MAD_Averages5_sec #or use load()
AveragesPerResident_60sec<- ENMO_MAD_Averages60_sec
AveragesPerResident_30min<- ENMO_MAD_Averages30_min


## Normalize Medlo Data



## Normalize 24hrs Data



### Link Data into data frames to correlate data:
### For 2019 and 2021: Medlo and 24 hrs data
### For 2021: Medlo, 24hrs, esimators (MAD and ENMO)
MedloData <- NormalizedTo30min ##still needs to enter this pipeline
Normalized_24hrs_Data <- Normalized24hrsTo30min21 ##still needs to enter this pipeline
ENMO_MAD_Data <- 

LinkedData_21<- link21data (MedloData, Normalized_24hrs_Data, ENMO_MAD_Data)












