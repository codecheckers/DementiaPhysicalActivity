
#this script removes entries of category 5 as first entry or as second entry of the activities
#this is an artifact of the data collection with the wearables being already on while not yet on the residents' wrists
#

clean2019_24hrs<- function (Data24hrs_19) {
  
  splitdataPerPerson19<- split(Data24hrs_19, Data24hrs_19$ID)
  
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
  
  return (splitdataPerPerson19)
  
}

  
