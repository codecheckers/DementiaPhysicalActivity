## summarize and visualize the fitness wristband output

## overview of categories as given by wristbands
## aggregated over 2,5 days 


ActivityDat<-  read.csv("Data24hr/LongFormat-Table 1.csv",header=FALSE)
ActivityDat <- ActivityDat [,-c(6,7)] #redundant empty columns
colnames(ActivityDat) <- c("Date", "Time", "Activity", "Minutes", "ID")
ActivityDat<-ActivityDat[- which(ActivityDat$Activity == ""), ]
ActivityDat<-ActivityDat[- which(ActivityDat$Activity == "-"), ]

levels(ActivityDat) <- c(1,2,4,5,6,7)
factor(ActivityDat$Activity)

ActivityDat$Activity <- factor(ActivityDat$Activity)
# minutes summed up per activity level

########################################################################
#### THIS CHANGES THE ACTIVITY LEVELS IN THE DATA BY HAND!!! 
#### ACCOUNT FOR THIS SOMEWHERE IN THE META DATA DESCRIPTION!
##change 5 to 7 by hand for 1/5 in the morning (measurements started around lunch that day)
index5<-which(ActivityDat$Activity== 5 & ActivityDat$Date== "5/1/19")
ActivityDat[index5,] [c(1,2,3,5,7,8,12,13,14),] $ Activity <-7

#And a few more missing data
ActivityDat[which(ActivityDat$Activity == 5),  ] [c(10,16,17,21,25,26,29),] $ Activity <- 7

#######################################################################



PerActivityLevel<- tapply(as.numeric(ActivityDat$Minutes), ActivityDat$Activity, sum)


# activity summed up per Person
splitdat<- split(ActivityDat, ActivityDat$ID)
#this gives an overview per ID and per factor of Activity
PerPerson<- sapply(splitdat,function(x) tapply(as.numeric(x$Minutes), x$Activity, function(x) sum (x, na.rm=TRUE) ) )

## make barplots with the information

barplot(PerActivityLevel)


### or a little nicer:
df<- data.frame(minutes = PerActivityLevel, 
                activities = c("Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data"))

library(ggplot2)
# Basic barplot
p<-ggplot(data=df, aes(x=df$activities, y= df$minutes )) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=minutes), vjust=-0.3, size=12)+
  labs(x = "", y = "Overall Minutes", size= 18)+
  theme (axis.text.x  = element_text (size=20), 
         axis.text.y = element_text (size = 18),
         axis.title.y = element_text (size = 18),
         panel.background = element_rect(fill = "white"))

p

#############################################################################################

### stacked bar per ID: ###### CHECK dataframe - sth not working with activityLevels #######

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

df <- data.frame(
  Activity=rep(c("Inactive",  "Light" , "Sleeping",  "Not on wrist", "Cycling", "No data"), length(unique(ActivityDat$ID))),
  Ids=rep(unique(colnames(PerPerson)), each= length(unique(activityLevel)) ),
  Minutes=as.numeric(PerPerson) )

plot <- ggplot() + theme_bw()+
  geom_bar(aes(y = Minutes, x = Ids, fill = Activity), 
           data = df, stat="identity") + 
  scale_fill_manual(values = cbp2) +
  labs(x = "Unique ID", y = "Minutes", size= 20)+
  theme (axis.text.x  = element_text (size=15), 
         axis.text.y = element_text (size = 15),
         axis.title.y = element_text (size = 20),
         axis.title.x = element_text (size = 20),
         panel.background = element_rect(fill = "white"),
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 20))


plot
