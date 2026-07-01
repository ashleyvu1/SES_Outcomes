library(haven)

##CREATING SOCIOECON OUTCOME TABLES AT AGE 25##

#DEBT#
qelj2601 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Outcome/qelj2601.sav")

View(qelj2601)

var <- c("IDME", "ZSINQ05")
W26_debt <- qelj2601[var]
View(W26_debt)

#To see W26_debt values
lapply(W26_debt, unique)


#EMPLOYMENT STATUS#
qelj2601 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Outcome/qelj2601.sav")
View (qelj2601)

var <- c("IDME", "ZTVNQ01")
W26_empl <- qelj2601[var]
View(W26_empl)

#To see W26_empl values
lapply(W26_empl, unique)

#EDUCATION ATTAINMENT
var <- c("IDME", "ZSCNQ01AA","ZSCNQ03A", "ZSCNQ03B", "ZSCNQ03C", "ZSCNQ03D", "ZSCNQ03E", "ZSCNQ03F")
W26_educ <-qelj2601[var]
View(W26_educ)

#To see W26_educ values
lapply(W26_educ, unique)


#HOUSING AND LIVING CONDITIONS
var <- c("IDME","ZLGNQ02", "ZLGNQ03")
W26_housing <- qelj2601[var]
View(W26_housing)

#To see W26_housing values
lapply(W26_housing, unique)

##NEET##

var <- c("IDME", "ZSCNQ06_M1", "ZSCNQ06_M2", "ZSCNQ06_M3", "ZTVNQ01")
neet <-qelj2601[var]
View(neet)

#To see neet values
lapply(neet, unique)

# merging
dim(W26_debt)
dim(W26_empl)
dim(W26_educ)
dim(W26_housing)
dim(neet)
outcomeData <- merge(W26_debt,W26_empl, by="IDME",all=TRUE)
outcomeData <- merge(outcomeData,W26_educ, by="IDME",all=TRUE)
outcomeData <- merge(outcomeData,W26_housing, by="IDME",all=TRUE)
outcomeData <- merge(outcomeData,neet, by="IDME", all=TRUE)
dim(outcomeData)
outcomeData$okOutcomeData <- 1


###########################
#### Adding Covariates ####
###########################
#### Follow steps we did above for education! + add to data_SES_excl ####
##### Age 5 mos (sex, family SES + structure, parental dep, race) ####
#Sex of the child
SOCIO101 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/SOCIO101.sav")
View(SOCIO101) 

W1varsSOCIO <- c("idme","asexe_2") 
W1_SEX <- SOCIO101[,W1varsSOCIO]
View(W1_SEX)

# Recode sex to be binary (0 = female, 1 = male)
W1_SEX$asexe_2[W1_SEX$asexe_2 == "F"]<-0
W1_SEX$asexe_2[W1_SEX$asexe_2 == "M"]<-1

#SES, family structure, parental depression
INDI101 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/INDI101.sav")
View(INDI101) 

W1varsINDI <- c("idme","AINFD08",#SES
                "afafd02", #family structure 
                "ADPMT01", #maternal depression 
                "ADPJT01") #paternal depression 
W1_5months <- INDI101[,W1varsINDI]

View(W1_5months)

# Recode family structure (0 = not intact, 1 = intact)
W1_5months$afafd02[W1_5months$afafd02 == 2|W1_5months$afafd02 == 3]<-0

#Race
ENFAN101 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/ENFAN101.sav")
View(ENFAN101) 

W1varsENFAN <- c("idme","asdeq4aA",#white
                 "asdeq4aB", #chinese
                 "asdeq4aC", #south asian
                 "asdeq4aD", #black
                 "asdeq4aE", #indigenous
                 "asdeq4aF", #arab
                 "asdeq4aG", #filipino
                 "asdeq4aH", #southeast asian
                 "asdeq4aI", #latin-american
                 "asdeq4aJ", #japanese
                 "asdeq4aK", #korean
                 "asdeq4aL") #other

W1_RACE <- ENFAN101[,W1varsENFAN]
View(W1_RACE)

W1_RACE$race_bin <- apply(W1_RACE[,c("asdeq4aA", "asdeq4aB", "asdeq4aC", "asdeq4aD", "asdeq4aE", "asdeq4aF", "asdeq4aG", "asdeq4aH", "asdeq4aI", "asdeq4aJ", "asdeq4aK", "asdeq4aL")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})


W1_RACE[1:50,c("asdeq4aA", "asdeq4aB", "asdeq4aC", "asdeq4aD", "asdeq4aE", "asdeq4aF", "asdeq4aG", "asdeq4aH", "asdeq4aI", "asdeq4aJ", "asdeq4aK", "asdeq4aL")] #check code worked
table(W1_RACE$race_bin, useNA = "ifany")

##Self-reported child mental health
INDI1311 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/indi1311.sav")
View(INDI1311) 

W1varsINDI1311 <- c("IDME",
                    "MBEET02D", #anxiety
                    "MBEET02O", #hyperactivity-inattention
                    "MBEET02H",#opposition
                    "MQEET10") #depression

W1_MH <- INDI1311[,W1varsINDI1311]
View(W1_MH)

W1_MH <- W1_MH %>% 
  rename (idme = IDME)

View(W1_MH)
####Maternal adolescent antisociality/conduct symptoms####
MERE101 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/MERE101.sav")
View(MERE101) 

antisoc_mat <- c("idme","aqmmq55","aqmmq56","aqmmq57")
W1_antisoc_mat <- MERE101[,antisoc_mat]
View(W1_antisoc_mat)

#Maternal antisociality recode, total score, + binary 
library(dplyr)
#Recode "9" as "NA" + "no" ("2") as "0"
W1_antisoc_mat[W1_antisoc_mat == 9] <- NA
W1_antisoc_mat[W1_antisoc_mat == 2] <- 0
View(W1_antisoc_mat)

#Creating scale by adding total of each column 
library(dplyr)
W1_antisoc_mat$W1_antisoc_mat_bin <- apply(W1_antisoc_mat[,c("aqmmq55","aqmmq56","aqmmq57")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})

W1_antisoc_mat[1:20,c("aqmmq55","aqmmq56","aqmmq57","W1_antisoc_mat_bin")] #check code worked
#Keep needed vars
W1_mere_rm <- c("idme","W1_antisoc_mat_bin")
W1_antisoc_mat_F <- W1_antisoc_mat[,W1_mere_rm]
View(W1_antisoc_mat_F)
####Paternal adolescent antisociality/conduct symptoms####
QAAP101 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Data/Covariates/QAAP101.sav")
View(QAAP101) 

antisoc_pat <- c("idme","aqpjq53","aqpjq54","aqpjq55")
W1_antisoc_pat <- QAAP101[,antisoc_pat]
View(W1_antisoc_pat)

#Paternal antisociality scale#
library(dplyr)
#Recode "9" as "NA" + "no" ("2") as "0"
W1_antisoc_pat[W1_antisoc_pat == 9] <- NA
W1_antisoc_pat[W1_antisoc_pat == 2] <- 0

#Creating scale by adding total of each column 
library(dplyr)
W1_antisoc_pat$W1_antisoc_pat_bin <- apply(W1_antisoc_pat[,c("aqpjq53","aqpjq54","aqpjq55")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})

W1_antisoc_pat[1:20,c("aqpjq53","aqpjq54","aqpjq55","W1_antisoc_pat_bin")] #check code worked

#Keep needed items
W1_pere_rm <- c("idme","W1_antisoc_pat_bin")
W1_antisoc_pat_F <- W1_antisoc_pat[,W1_pere_rm]
View(W1_antisoc_pat_F)
#### Combine W1 Covariates ####
library(dplyr)
library(purrr)
W1_COV <- list(W1_SEX, W1_5months, W1_RACE, W1_MH, W1_antisoc_mat_F, W1_antisoc_pat_F)
W1_COV_F <- reduce(W1_COV, full_join, by = "idme")
View(W1_COV_F)

names(W1_COV_F)
#keeping only needed variables
W1_COV_F <- W1_COV_F %>% 
  select(idme, asexe_2, AINFD08, afafd02, ADPMT01, ADPJT01, race_bin, MBEET02D, MBEET02O, MBEET02H, MQEET10, W1_antisoc_mat_bin, W1_antisoc_pat_bin)

# Rename variables
names(W1_COV_F)<-c("IDME","W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                   "W1_antisoc_mat_bin","W1_antisoc_pat_bin")

View(W1_COV_F)

# SAVE COVARIATE TABLE THEN COMBINE W OTHERS
# ADD TO YOUR LARGER (2120) + ANALYSIS TABLE (1265)
# Save file
write.csv(W1_COV_F, "/mnt/40295D00/Travail/Commun/Ashley/W1_COV_F.csv", row.names=FALSE)

#Use to load and view new file
W1_COV_F<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/W1_COV_F.csv")
View(W1_COV_F)




