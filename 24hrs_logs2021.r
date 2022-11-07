ActivityDat21<- read.csv("Data24hr/24hrsLongForm.csv",header=TRUE)
ActivityDat21 <- ActivityDat21 [, -1] #remove row numbering

#turn data into format: 
#minutes, activities 
#minutes are the total minutes of one type of data collected
#activities are one of the following: "Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data"

countsPerActivity<-table(ActivityDat21$completeActivity)
activities21<- c("Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data")
minutes21<- c( countsPerActivity [3] + countsPerActivity [4],
             countsPerActivity [5] + countsPerActivity [6],
             countsPerActivity [7] + countsPerActivity [8],
             0,
             countsPerActivity [1] + countsPerActivity [2],
             0)


df21<- data.frame(minutes21 = minutes21, 
                activities21 = activities21)

library(ggplot2)
# Basic barplot
p21<-ggplot(data=df21, aes(x=activities21, y= minutes21 )) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=minutes21), vjust=-0.3, size=4)+
  labs(x = "", y = "Overall Minutes", size= 18)+
  theme (axis.text.x  = element_text (size=15), 
         axis.text.y = element_text (size = 18),
         axis.title.y = element_text (size = 18),
         panel.background = element_rect(fill = "white"))

p21

###################### Alternative to compare info better ##########
#What fraction of time was which activity 

#year (x), activities (fill), minutes (y)
df_compare<- data.frame(year=c(rep(19, 6),rep(21, 6)),
                        activities_compare = c(activities21, activities21),
                        minutes_compare = c(PerActivityLevel, minutes21))
exclude<-c(which(df_compare$activities_compare == "Not on wrist"),
which(df_compare$activities_compare == "No data") )

df_compare <- df_compare [-exclude,]

###### Stacked Percentages #######################
ggplot(df_compare, aes(x = year,
                       y = minutes_compare,
                       fill = activities_compare)) + 
  geom_bar(stat = "identity", position = "fill")+
  
  labs(x = "Year of Data Collection", y = "Overall Minutes", size= 18)+
  theme (axis.text.x  = element_text (size=15), 
         axis.text.y = element_text (size = 15),
         axis.title.y = element_text (size = 15),
         panel.background = element_rect(fill = "white"))

########### Stacked Bar Chart #####################
ggplot(df_compare, aes(x = year,
                       y = minutes_compare,
                       fill = activities_compare)) + 
  geom_bar(stat = "identity", position = "fill")+
  
  labs(x = "Year of Data Collection", y = "Overall Minutes", size= 18)+
  theme (axis.text.x  = element_text (size=15), 
         axis.text.y = element_text (size = 15),
         axis.title.y = element_text (size = 15),
         panel.background = element_rect(fill = "white"))


###################### Make Data for 21 only about those with advanced dementia ##############
install.packages("lubridate")
library(lubridate)

#add Date column to ActivityDat21 
ActivityDat21$completeTime <- as.POSIXlt(ActivityDat21$completeTime)
ActivityDat21$Date <- as.Date(ActivityDat21$completeTime)
ActivityDat21$MacWatchID<- ActivityDat21$completeIPaddress #create a key variable
ActivityDat21$dayonly <- sapply(strsplit(as.character(ActivityDat21$Date), "2021-05-"),function (x) x[2])
# import keyfile and adjust Date in dataframe
KeyDat21<- read.csv("completeKeyfileWatches.csv",header=TRUE)
#change and add date notation
dayonly<-sapply(strsplit(as.character(KeyDat21$Date), "202105"),function (x) x[2])
newDate<- paste0("2021-05-",dayonly)
KeyDat21$Date<- as.POSIXct(newDate)
KeyDat21$dayonly<- dayonly

# Date + IPaddress are keys to fuse the two datasets
# add ParticipantID column to ActivityDat21

combined24hrsDat21<-merge(ActivityDat21, KeyDat21, by=c("MacWatchID","dayonly"))

# add Dementia Stage variable to ActivityDat21

beginningDementia<- c(5,8,9,10,17,18)
indexBegin<-which(combined24hrsDat21$PatientID %in% beginningDementia )
DementiaStage<- numeric(length = length(combined24hrsDat21$PatientID))
DementiaStage[indexBegin] <- 1 #beginning (0 = advanced)

############ New Plot with only Advanced or only Beginning ############
#make two new counts, 21beginning, 21advanced
countsPerActivitybeginning<- table(combined24hrsDat21$completeActivity [indexBegin])
activities21beginning<- c("Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data")
minutes21beginning<- c( countsPerActivitybeginning [3] + countsPerActivitybeginning [4],
                        countsPerActivitybeginning [5] + countsPerActivitybeginning [6],
                        countsPerActivitybeginning [7],
                        0,
                        countsPerActivitybeginning [1] + countsPerActivitybeginning [2],
                        0)

countsPerActivityadvanced<- table(combined24hrsDat21$completeActivity [which(DementiaStage==0)])
activities21advanced<- c("Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data")
minutes21advanced<- c( countsPerActivityadvanced [1] + countsPerActivityadvanced [2],
                       countsPerActivityadvanced [3] + countsPerActivityadvanced [4],
                       countsPerActivityadvanced [5] + countsPerActivityadvanced [6],
                        0,
                        0,
                        0)
### Add this info to compare dataset

df_compare2<- data.frame(year=c(rep(19, 6),rep(21, 6), rep(211,6), rep(212,6)), #211=begin 212=advanced
                        activities_compare = c(activities21, activities21, 
                                               activities21beginning,activities21advanced ),
                        minutes_compare = c(PerActivityLevel, minutes21, 
                                            minutes21beginning, minutes21advanced ))
exclude<-c(which(df_compare2$activities_compare == "Not on wrist"),
           which(df_compare2$activities_compare == "No data") )

df_compare2 <- df_compare2 [-exclude,]

###############################################################################
ggplot(df_compare2, aes(x = as.factor(year),
                       y = minutes_compare,
                       fill = activities_compare)) + 
  geom_bar(stat = "identity", position = "fill")+
  
  labs(x = "Year of Data Collection", y = "Percentage of total activity measured", size= 18)+
  scale_x_discrete(breaks=c("19","21","211", "212"),
                   labels=c("19", "21", "21_BD","21_D" ))+
  scale_fill_discrete(name="Activity")+
  theme (axis.text.x  = element_text (size=15), 
         axis.text.y = element_text (size = 15),
         axis.title.y = element_text (size = 15),
         panel.background = element_rect(fill = "white"))


