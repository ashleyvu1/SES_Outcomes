finalData <- merge(W1_COV_F,exposureData, all=TRUE, by="IDME")
finalData <- merge(finalData,outcomeData, all=TRUE, by="IDME")
dim(finalData)

write.csv(finalData,"/mnt/40295D00/Travail/Commun/Ashley/Data/finalData.csv")
View(finalData)



###########################
#### Keeping 2120 only ####
###########################
names(SOCIO101)[names(SOCIO101)=="idme"] <- "IDME"
SOCIO101_vars <- c("IDME", "ALGED00")
long <- SOCIO101[,SOCIO101_vars]
View(long) # gather IDME + var showing longitudinal participants

list <- list(finalData, long)
finalData_A <- reduce(list, full_join, by = "IDME") # adding long variable to table
finalData <- finalData_A[finalData_A$ALGED00 == 1, ] # select 2120 participants
dim(finalData) # check shows 2120 remain! 

write.csv(finalData,"/mnt/40295D00/Travail/Commun/Ashley/Data/finalData.csv") ###All variables with 2120
View(finalData)
