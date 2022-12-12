#clean 2021 data
#the 24hrs data was manually extracted from the watches and includes times that are non-wear times
#this script checks with the Accelerometer Data for start and end times and removes all 24hrs entries that 
#are outside of those recordings

#data.frame: Day startAcc endAcc PatientID


#24hrs data in need of cleaning:
Dat10<- transformData21 ( "Data24hr/ActivityLog_20210510.csv")
Dat11<- transformData21 ( "Data24hr/ActivityLog_20210511.csv")
Dat12<- transformData21 ( "Data24hr/ActivityLog_20210512.csv")
Dat13<- transformData21 ( "Data24hr/ActivityLog_20210513.csv")
Dat14<- transformData21 ( "Data24hr/ActivityLog_20210514.csv")

DataActive21<- rbind(Dat10, Dat11, Dat12, Dat13, Dat14)

Date24<-as.Date(as.POSIXct(DataActive21$Date, format = "%Y-%m-%d"))
start24<-sapply(strsplit(DataActive21$Time, split = "-"), function (x) x[1])
end24<-sapply(strsplit(DataActive21$Time, split = "-"), function (x) x[2])
End24<- as.POSIXct( paste(Date24, end24), format = "%Y-%m-%d %H:%M")
Start24<- as.POSIXct( paste(Date24, start24), format = "%Y-%m-%d %H:%M")

Full24Data<-data.frame(DataActive21, Date24,Start24,End24)

#Date, Time (as chr), Activity, Minutes, ID
splitdat21<- split(Full24Data, Full24Data$ID)

#from BatchProcessing all datafiles with Acc Data, we get:
AccTimes<- data.frame(Date=as.Date(startAcc),startAcc,endAcc,residentID)
#as.numeric(AccTimes$residentID)

#create for each entry in Full24Data an entry if datapoint should be kept or not

for (i in 1: length(Full24Data$ID)) {
  accSameID<- AccTimes[which(Full24Data[i,]$ID == as.numeric(AccTimes$residentID)), ]
  accSameIDSameDate<- accSameID [which(Full24Data[i,]$Date24 == accSameID$Date ), ]
  #there can be several Acc measurements of one resident on one day!! -> use min/max for start/end
  #24hrs datapoint starts after the earliest Acc Data recording:
  #reduce start time Acc by 5 min as there is a timelag issue/recording issue??!
  startLogic[i]<- (Full24Data[i,]$Start24 > (min(accSameIDSameDate$startAcc) - 300)  )
  #does the 24hrs data point end before the end of the Acc measures?
  endLogic[i]<- (Full24Data[i,]$End24 < min(accSameIDSameDate$endAcc)   )
}

KeepIndex<- startLogic+endLogic

cleaned24hrsdat<- Full24Data[which(KeepIndex ==2), ] #filter those whose start/end times are ok


