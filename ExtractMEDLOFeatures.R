## Import MEDLO Data

library(haven)
Medlo_2019 <- read_dta("DataMedloADL/Medlo data_2019_def1.dta")
Medlo_2021 <- read_dta("DataMedloADL/Medlo data_2021_def1.dta")

#manually fix a mistake by observer when noting client numbers
Medlo_2019 [which(Medlo_2019 [,8] == "12 P KWA-056-01-01-07"),8] <- "KWA-056-01-01-07"
Medlo_2019 [which(Medlo_2019 [,8] == "11 S KWA-056-01-02-08"),8] <- "KWA-056-01-02-08"


#a5, b5 (until f5) are "mate van fysike inspanning" - per minute observation
Medlo_2019 [,15] #a5
Medlo_2019 [,22] #b5

index5<- seq(from=15, to= 230, by = 7)

#extract physical activity observations (per row one client and observation moment - columns are per minute observations)
fysikeInspanning19<- Medlo_2019 [,index5]
fysikeInspanning21<- Medlo_2021 [,index5]

table(as.numeric(fysikeInspanning19[7,])) #count how often which levels of physical activity were observed

#data has been entered on 31 minutes - if shorter observation, they used 9999999 to indicate missing datapoints

#go through dataset:
#per clientnumber: 
#extract all observation moments (i.e. all rows of that client)
#extract all physical activity observations
#table those 

#how to deal with NA and missing data? -- deal with it when normalizing 
#how to deal with double observations? (i.e. same timeslot, two observers) -- deal with it when normalizing
#-- this is not ideal as observations are not independent (it it the same observation timeslot) !

#add raw counts to data
#normalize counts by valid observed minutes - add those counts


#this function takes subsets per client
extractCounts<- function (subSetResident) {
  ActivTable<- table(as.numeric(unlist(subSetResident)))
  ResLevels<- as.numeric(attributes(ActivTable)$dimnames [[1]])
  ResCounts<- as.data.frame(cbind(ResLevels, as.numeric(ActivTable)))
  #ResNoOfObs<- sum(ResCounts[,2]) #for normalization?? --> better do this on full database/matrix
  return(ResCounts)
}

ResidentCodes21 <- unique(Medlo_2021 [,8])
Counts21 <- list()

for (i in 1: length(ResidentCodes21[[1]])) {
  subTest<- Medlo_2021 [which(Medlo_2021 [,8] == ResidentCodes21[[1]][i] ),]
  index5<- seq(from=15, to= 230, by = 7)
  subActive<-subTest [,index5]
  Counts21[[i]] <- extractCounts(subActive)
}

ResLevelsTotal21<- as.data.frame(seq(1:7))
colnames(ResLevelsTotal21) <- "ResLevels"

#merge counts into one data frame
for (i in 1: length(ResidentCodes21[[1]])) {
  ResLevelsTotal21 <- merge(ResLevelsTotal21, Counts21[[i]],by= "ResLevels",all=TRUE)
}
colnames(ResLevelsTotal21) <- c("ResLevels", as.character(seq(1:13)))
ResLevelsTotal21 <- t(ResLevelsTotal21)
colnames(ResLevelsTotal21) <- ResLevelsTotal21 [1,]
ResLevelsTotal21 <- ResLevelsTotal21 [-1,]

MEDLO_21<-cbind (ResidentCodes21, ResLevelsTotal21)

#################
ResidentCodes19 <- unique(Medlo_2019 [,8])
Counts19 <- list()

for (i in 1: length(ResidentCodes19[[1]])) {
  subTest<- Medlo_2019 [which(Medlo_2019 [,8] == ResidentCodes19[[1]][i] ),]
  index5<- seq(from=15, to= 230, by = 7)
  subActive<-subTest [,index5]
  Counts19[[i]] <- extractCounts(subActive)
}

ResLevelsTotal19<- as.data.frame(seq(1:7))
colnames(ResLevelsTotal19) <- "ResLevels"

#merge counts into one data frame
for (i in 1: length(ResidentCodes19[[1]])) {
  ResLevelsTotal19 <- merge(ResLevelsTotal19, Counts19[[i]],by= "ResLevels",all=TRUE)
}
colnames(ResLevelsTotal19) <- c("ResLevels", as.character(seq(1:11)))
ResLevelsTotal19 <- t(ResLevelsTotal19)
colnames(ResLevelsTotal19) <- ResLevelsTotal19 [1,]
ResLevelsTotal19 <- ResLevelsTotal19 [-1,]

MEDLO_19<-cbind (ResidentCodes19, ResLevelsTotal19)
colnames(MEDLO_19) <- c("codebw",  as.character(seq(1:7)), "99999")
#######################

MEDLO_Total<- rbind(MEDLO_21, MEDLO_19 [,1:8])

TotalObservedMinutes<- rowSums(MEDLO_Total [,2:8],na.rm = TRUE)

Normalized <- matrix(nrow=length(MEDLO_Total[,1]), ncol = 7)
NormalizedTo30min<- matrix(nrow=length(MEDLO_Total[,1]), ncol = 7)
for (i in 1: length(MEDLO_Total[,1])) {
  Normalized [i,] <- round(as.numeric(MEDLO_Total [i,2:8]/ TotalObservedMinutes [i]),3)
  NormalizedTo30min [i,] <- round(as.numeric(MEDLO_Total [i,2:8]/ TotalObservedMinutes [i]),3) * 30
}

#add participant ID
Normalized <- data.frame (bwcode = MEDLO_Total [,1],Normalized)
NormalizedTo30min <- data.frame (bwcode = MEDLO_Total [,1],NormalizedTo30min)
#########################

#Visualize Medlo Data

datMedlo <- data.frame(
  
  MovementCategoryMedlo = factor(c("1","2", "3", "4", "5", "6", "7"), 
                      levels    = c("1","2", "3", "4", "5", "6", "7"),
                     labels = c("Lying/No Movement","Sitting quietly", "Light to moderate sitting", 
                              "Standing", "Standing activity/walking around", "Walking Activity/Cycling", 
                              "Sports/Whole Body Movement")),
  
  TimeSumsMedlo = colSums(NormalizedTo30min [,2:8], na.rm = TRUE)
)

#make proper labels per movement category 
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
 theme(axis.text.x = element_text(size=14, angle=80)) 








