#Link Data for correlational analyses

Link19_21 <- read.csv("DataMedloADL/Link19_21.csv")
keydata<-as.data.frame(Link19_21)[,1:2]


AveragesPerResident<- ENMO_MAD_Averages
AveragesPerResident[,4]<- as.numeric(AveragesPerResident[,4])


#Link 2021 Data:

link21data<- function (MedloData, Normalized_24hrs_Data, ENMO_MAD_Data, keydata) {
  
  SensorData_21<- merge(ENMO_MAD_Data, keydata, by.x = "ResidentID", by.y = "LIACS.ID" )
  
  # add 24hrs data
  #add colnames
  Dat21_24hrs<-cbind(t(Normalized_24hrs_Data), ResidentID= as.numeric(colnames(Normalized_24hrs_Data)))
  
  #Sensors and 24hrs logs in 21
  WatchData_21 <- merge(SensorData_21,Dat21_24hrs, by = "ResidentID" )
  
  #Sensors, 24hrs, Medlo in 21
  MovementData_21 <- merge(WatchData_21, MedloData, by.x = "NIVEL.ID..Compas.2019.", by.y = "bwcode") #this is where "cycling" disappears 
  #(only full observations are kept - for correlations later)
  
  #MovementData_21 <- MovementData_21[,-c(21,22,23,24)] #remove empty cols from keyfile - not needed anymore, cleaned keyfile when importing data
  
  colnames(MovementData_21) <- c( "NIVEL_ID", "LIACS_ID", 
                                  "MADmean", "MeanEN", "MeanENMO",
                                  "MinMAD","MinEN", "MinENMO",
                                  "MaxMAD", "MaxEN", "MaxENMO",
                                  "VarMAD", "VarEN", "VarENMO",
                                  "NoEpochs",
                                  "MADsd", "MADmedian", "MADqant", "MADfrag","MADrel",
                                  "Cycling","Inactive", "Light", 
                                  "NotOnWrist", "Sleeping", 
                                  "MLO1", "MLO2", "MLO3", "MLO4", "MLO5",
                                  "MLO6", "MLO7")
  
  #select a subset of entries for correlation matrix:
  data<- MovementData_21[, c(3, #MADmean
                             16,17,18,19,20, #sd,median, qant,frag,rel
                             22,23,24,25, #inactive, light, sleeping, not on wrist
                             26,27,29,30) ]#ml1,ml2,ml4, ml5
  data[is.na(data)] <- 0
  
  return(data)
}





Dat19_24hrs<-cbind(t(Normalized24hrsTo30min19), residents= as.numeric(colnames(Normalized24hrsTo30min19)))
#make sure to add NA for empty columns

########

######
#for 19: only Medlo, 24hrs 
hrs_Linked_19<- merge(Dat19_24hrs, keydata, by.x = "residents", by.y = "LIACS ID" )
hrs_Medlo_19 <-merge(hrs_Linked_19, NormalizedTo30min, by.x = "NIVEL ID (Compas 2019)", by.y = "bwcode")

#####
#Categories: For 21 and 19: Medlo and 24hrs
NormalizedTo30min, 

#CODES:
# 1 Inactive
# 2 Light
# 3 Heavy
# 4 Sleeping
# 5 Not on wrist
# 6 Cycling
# 7 No Data

data<- MovementData_21[, c(3,5,7,8,11,12,13,14,15,16) ]
data[is.na(data)] <- 0

corrplot(cor(data), method = "number",
         title = "method = 'number'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 

corrplot.mixed(cor(data),
               lower = "number", 
               upper = "circle",
               tl.col = "black")


#24hrsxMedlo
dat<- MovementData_21[, c(7,8,11,12,13,14,15,16) ]
dat[is.na(dat)] <- 0

corrplot.mixed(cor(dat),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

