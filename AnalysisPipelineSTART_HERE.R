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
### Actual processing into ENMO and MAD is done on a stand alone laptop, this is just to show the pipeline

# get all filenames
files_from_batch<- list.files("DataAcc") #add path here - will be the path to the data folder on the server
# filter those files that have aag.dat at the end
filterAAG<-sapply(strsplit(files_from_batch, " "), function (x) x [9] == "aag.dat" )
# run the functions to get measurements on those data
datafiles<- paste0("DataAcc/",files_from_batch [filterAAG]) #sorted by participant ID!



## import Accelerometer Times (start end times) for filtering of "not on wrist times" -- I.e. we only use wearable data collected 
## during the times we also switched the watches on/off for data collection

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

### Clean 24 hrs data with two cleaning functions:
### 2019 data gets cleaned by removing category 5 data as first or second entry 
###- wristband was switched on but not yet worn by client
### 2021 data gets cleaned by using the times that Accelerometer Data was collected
### during 2021 the wristband were switched on just before Acc Data got recorded

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
ENMO_30min <- readRDS("averagesENMO_1800.rds") #30 min epoch
ENMO_60sec <- readRDS("averagesENMO_60.rds") #60 sec epochs 
ENMO_5sec<- readRDS("averagesENMO_5.rds")


## Normalize Medlo Data
#getCounts !!throws many warning messages bc of iterative merging
MedloCounts19<- extractActivityCountsPerR (Medlo_2019)
MedloCounts21<- extractActivityCountsPerR (Medlo_2021)
#normalizeData to 30 min
#adjust colnames because of typo in codebw/codebew
colnames(MedloCounts19) <- c('codebw', '1', '2', '3', '4', '5', '6', '7', '99999')
MEDLO_Total<- rbind(MedloCounts21, MedloCounts19 [,1:8]) #only 1:8 to get rid of "99999" entries
MedloDat<- normalizeMedlo30min(MEDLO_Total)


## Normalize 24hrs Data
#use clean data to get per Person counts:

#todo: turn clean data into row/column overview per person and level
perPerson19 <- get24hourcounts_19 (clean2019_24hrs_Dat)
perPerson21 <- get24hourcounts_21 (clean2021_24hrs_Dat)

#normalize per Person counts
Data24hrs19<- normalize24hrs30min(perPerson19)
Data24hrs21<- normalize24hrs30min(perPerson21)

### Link Data into data frames to correlate data:
### For 2019 and 2021: Medlo and 24 hrs data



### For 2021: Medlo, 24hrs, estimators (MAD and ENMO)
MedloData <- NormalizedTo30min #
Normalized_24hrs_Data <- Normalized24hrsTo30min21 #
keydata<- Link19_21

### create linkedData for different ENMO/MAD epochs to compare results

ENMO_MAD_Data <- ENMO_30min
LinkedData_21_30min<- link21data (MedloData, Normalized_24hrs_Data, ENMO_MAD_Data, keydata)

ENMO_MAD_Data <- ENMO_60sec
LinkedData_21_60sec<- link21data (MedloData, Normalized_24hrs_Data, ENMO_MAD_Data, keydata)

ENMO_MAD_Data <- ENMO_5sec
LinkedData_21_5sec<- link21data (MedloData, Normalized_24hrs_Data, ENMO_MAD_Data, keydata)


### see file with PlottingScripts.R for the correlation matrices










