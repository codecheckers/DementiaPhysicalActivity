#Plotting Scripts

##############################
#Barcharts - MEDLO and 24hrs data

datMedlo <- data.frame(
  
  MovementCategoryMedlo = factor(c("1","2", "3", "4", "5", "6", "7"), 
                                 levels    = c("1","2", "3", "4", "5", "6", "7"),
                                 labels = c("Lying/No Movement","Sitting quietly", "Light to moderate sitting", 
                                            "Standing", "Standing activity/walking around", "Walking Activity/Cycling", 
                                            "Sports/Whole Body Movement")),
  
  TimeSumsMedlo = c(colSums(LinkedData_21_30min [,8:13], na.rm = TRUE),0)
)

p <- ggplot(data=datMedlo, aes(x=MovementCategoryMedlo, y=TimeSumsMedlo, fill=MovementCategoryMedlo)) +
  
  geom_bar(stat="identity")

p + scale_x_discrete(labels=c("Lying/No Movement" = "Lying", 
                              "Sitting quietly" = "Sitting Quiet",
                              "Light to moderate sitting" = "Sitting Moderate",
                              "Standing" = "Standing",
                              "Standing activity/walking around" = "Walking",
                              "Walking Activity/Cycling" = "Active Walk",
                              "Sports/Whole Body Movement" = "Sports")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, angle=80)) +
  theme(legend.position="none")

##########

dat24hrs <- data.frame(
  MovementCategory = factor(c("1","2", "3","4"), 
                            levels=c("1","2","3", "4"),
                            labels = c( "Inactive","Light", "Not on wrist", "Sleeping") ),
  
  TimeSums = colSums(LinkedData_21_30min [,4:7], na.rm = TRUE)
)
q <- ggplot(data=dat24hrs, aes(x=MovementCategory, y=TimeSums, fill=MovementCategory)) +
  
  geom_bar(stat="identity")

q + scale_x_discrete(labels=c( "Inactive","Light", "Not on wrist", "Sleeping")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, angle=80)) +
  theme(legend.position="none")



###############################

library(corrplot)

# Opening the graphical device
pdf("CorrelationMatrix_30min.pdf")
corrplot.mixed(cor(LinkedData_21_30min),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               title = "30minEpochs",
               mar=c(0,0,2,0))
# Closing the graphical device
dev.off() 

# Opening the graphical device
pdf("CorrelationMatrix_5sec.pdf")
corrplot.mixed(cor(LinkedData_21_5sec),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               title = "5secEpochs",
               mar=c(0,0,2,0))
# Closing the graphical device
dev.off() 

# Opening the graphical device
pdf("CorrelationMatrix_60sec.pdf")
corrplot.mixed(cor(LinkedData_21_60sec),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               title = "60secEpochs",
               mar=c(0,0,2,0))

# Closing the graphical device
dev.off() 

###########################

#Plot MAD data against time "Light Active"
plot(LinkedData_21_60sec$MeanMAD, LinkedData_21_60sec$Light)
plot(LinkedData_21_60sec$MeanMAD, LinkedData_21_60sec$Inactive)
plot(LinkedData_21_60sec$Light, LinkedData_21_60sec$Inactive)

plot(LinkedData_21_60sec$MeanMAD, LinkedData_21_60sec$MLO2) #ceiling effect, 3 people with full 30 min on ML02
plot(LinkedData_21_60sec$MeanMAD, LinkedData_21_60sec$MLO3) #floor effect, almost all people with full 0 ML 03
plot(LinkedData_21_60sec$Inactive, LinkedData_21_60sec$MLO2)

pairs(LinkedData_21_60sec[,c(1,4,5,6,7)], pch = 19) #meanMAD/24hrs
pairs(LinkedData_21_60sec[,c(1,8,9,10,11,12,13)], pch = 19) #meanMAD/Medlo
pairs(LinkedData_21_60sec[,c(4,5,6,7,8,9,10,11,12,13)], pch = 19)

###########################

MAD_AllEpochs5sec<-readRDS("epochsPerResident5sec.rds")
allData<-numeric(length=3)

for (i in 1:length(MAD_AllEpochs5sec)){

dat<-data.frame(MAD=MAD_AllEpochs5sec[[i]],
                 time= seq (1: length( MAD_AllEpochs5sec[[i]])),
                 resident=rep(i, length(MAD_AllEpochs5sec[[i]])))

allData<-rbind(allData,dat)
}
library(ggplot2)
allData<- allData [-1,]
ggplot(allData, aes(time, MAD)) +
  geom_point()+
  facet_wrap(~allData$resident, nrow=3)

p <- ggplot(allData, aes(x=as.factor(allData$resident), y=MAD)) + 
  geom_violin()
p

#############################

