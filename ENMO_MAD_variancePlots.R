AveragesPerResident<- load("~/DementiaPhysicalActivity/ENMOAverages.RData")

AveragesPerResident<- ENMO_MAD_Averages
measure<- c(rep("MAD",16),rep("EN",16),rep("ENMO",16) )
value<- c(AveragesPerResident[,1],AveragesPerResident[,2],AveragesPerResident[,3] )

boxplotdat<- data.frame(value,measure)

ggplot(boxplotdat, aes(x=measure,y=value)) +
  geom_boxplot()

#
ggplot(boxplotdat[17:48, ], aes(x=measure,y=value)) +
  geom_boxplot()
