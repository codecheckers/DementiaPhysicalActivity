#Extract Summary Statistics on Age, Gender, Walking Aids for Participants in 2019 and 2021
##
#library(haven)
#PatientVraaglijst_2018 <- read_dta("DataMedloADL/data set_client vragenlijlst 2018_def1.dta")

#From MEDLO: DOB and GESLACHT ( [, 3:4])

install.packages("eeptools")        # Install eeptools package
library("eeptools")

# Medlo_2019 [,4] date Data Collection
my_df <- as.data.frame(Medlo_2019 [,1:10])

ageAtCollection19<- numeric()

for (i in 1:nrow(Medlo_2019)) {
  date_today <-  as.Date(my_df [i,4], format = "%d/%m/%Y")  #Date Data Collection
  x_birth <- as.Date(as.character(Medlo_2019 [i,10]), format = "%d/%m/%Y")
  
  x_age <- age_calc(x_birth,          # Convert birth to age
                    date_today,
                    units = "years")
  ageAtCollection19[i] <- floor(x_age)  
}

###################

# Medlo_2011 [,4] date Data Collection
my_df <- as.data.frame(Medlo_2021 [,1:10])

ageAtCollection21<- numeric()

for (i in 1:nrow(Medlo_2021)) {
  date_today <-  as.Date(my_df [i,4], format = "%d/%m/%Y")  #Date Data Collection
  x_birth <- as.Date(as.character(Medlo_2021 [i,10]), format = "%d/%m/%Y")
  
  x_age <- age_calc(x_birth,          # Convert birth to age
                    date_today,
                    units = "years")
  ageAtCollection21[i] <- floor(x_age)  
}

#########################

CodeAgeGeslacht21<- cbind(codebw = Medlo_2021 [,8],Age = ageAtCollection21 , Geslacht = Medlo_2021 [,9]) [!duplicated(Medlo_2021 [,8]), ] 
CodeAgeGeslacht19<- cbind(codebw= Medlo_2019 [,8],Age = ageAtCollection19 , Geslacht = Medlo_2019 [,9]) [!duplicated(Medlo_2019 [,8]), ] 
colnames(CodeAgeGeslacht19)<- c("codebw", "Age", "geslacht")


range(rbind(CodeAgeGeslacht19, CodeAgeGeslacht21) [,"Age"])
mean(rbind(CodeAgeGeslacht19, CodeAgeGeslacht21) [,"Age"])
sd(rbind(CodeAgeGeslacht19, CodeAgeGeslacht21) [,"Age"])

rbind(CodeAgeGeslacht19, CodeAgeGeslacht21) [,"geslacht"] #1 = Female, 0 = Male

length(rbind(CodeAgeGeslacht19, CodeAgeGeslacht21) [,"geslacht"]  ) #24 in total, 2 male

#######################
nrow(CodeAgeGeslacht21)




