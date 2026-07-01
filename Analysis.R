
# next steps:
# 1) perform some data management/cleaning of the outcome variables (see response categories, recode the one that are not relevant, manage missing data)
# 2) descriptive statistics of these variables
# 3) if Alessia hasn't sent you the suicide variables yet, perform some data management/cleaning of the exposure variables (consider suicide-related variables at age 13, 15, and 17 only, in line with Alessia's paper)


#Use to load and view new file
finalData<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/finalData.csv")
View(finalData)

#CHECKING RESPONSE CATEGORIES FOR OUTCOME VARIABLES

##DEBT##

###HOW DO YOU PERCEIVE YOUR DEBT SITUATION?###
library(dplyr)
finalData <- finalData %>%
  rename(W26_debt_perception = ZSINQ05)

finalData$W26_debt_perception[finalData$W26_debt_perception %in% c(-3)] <- NA
table(finalData$W26_debt_perception, useNA = "ifany")

##EMPLOYMENT STATUS

#During the past month, did you have at least one paid job or were self-employed?
finalData <- finalData %>%
  rename(W26_empl_status = ZTVNQ01.x)

table(finalData$W26_empl_status, useNA = "ifany")

finalData$W26_empl_status[finalData$W26_empl_status == -3] <- NA
finalData$W26_empl_status[finalData$W26_empl_status == 1] <- 0
finalData$W26_empl_status[finalData$W26_empl_status == 2] <- 1


###HOUSING CONDITIONS###
#In the past 12 months, in your dwelling (apartment, condo, or house), have you had any problems with mould, persistent musty or earthy odours, or problems with insects or rodents?
finalData <- finalData %>%
  rename(W26_housing_cond = ZLGNQ03)

table(finalData$W26_housing_cond, useNA = "ifany")

finalData$W26_housing_cond[finalData$W26_housing_cond == 2] <- 0
finalData$W26_housing_cond[finalData$W26_housing_cond == 1] <- 1

###HOUSING STABILITY###

#How many times have you moved in the last 5 years?
finalData <- finalData %>%
  rename(W26_housing_moved = ZLGNQ02)

finalData$W26_housing_moved[finalData$W26_housing_moved == -4|finalData$W26_housing_moved ==-3] <- NA


table(finalData$W26_housing_moved, useNA = "ifany")


###EDUCATIONAL ATTAINMENT###

###Highschool diploma
finalData <- finalData %>%
  rename(hs_obtained = ZSCNQ03A) 

table(finalData$hs_obtained, useNA = "ifany")

#DEP obtain
finalData <- finalData %>%
  rename(dep_obtained = ZSCNQ03B)

table(finalData$dep_obtained, useNA = "ifany")

#Other recognized professional qualification
finalData <- finalData %>%
  rename(otherqual_obtained = ZSCNQ03C)

table(finalData$otherqual_obtained, useNA = "ifany")

#DEC/AEC obtain
finalData <- finalData %>%
  rename(dec_obtained = ZSCNQ03D)

table(finalData$dec_obtained, useNA = "ifany")

##Uni degree obtain
finalData <- finalData %>%
  rename(uni_obtained = ZSCNQ03E)

table(finalData$uni_obtained, useNA = "ifany")

###Other diploma/certificate obtain
finalData <- finalData %>%
  rename(otherdiploma_obtained = ZSCNQ03F)

table(finalData$otherdiploma_obtained, useNA = "ifany")

###Highest level of education or training have you taken courses in?
finalData <- finalData %>%
  rename(highesteduc_enrolled = ZSCNQ01AA)

table(finalData$highesteduc_enrolled, useNA = "ifany")

##NEET##
var <- c("IDME", "ZSCNQ06_M1", "ZSCNQ06_M2", "ZSCNQ06_M3", "ZTVNQ01", "ZSCNQ06A_M1", "ZSCNQ06A_M2", "ZSCNQ06A_M3", "ZTVNQ02A_M13")
neet <-qelj2601[var]
View(neet)


##NEET##
#Education question
table(neet$ZSCNQ06_M1, useNA = "ifany")
table(neet$ZSCNQ06_M2, useNA = "ifany")
table(neet$ZSCNQ06_M3, useNA = "ifany")

table(neet$ZSCNQ06A_M1, useNA = "ifany")
table(neet$ZSCNQ06A_M2, useNA = "ifany")
table(neet$ZSCNQ06A_M3, useNA = "ifany")


###Combining educ questions 
library(dplyr)

neet$ZSCNQ06_M1[neet$ZSCNQ06_M1 ==-3] <- NA
neet$ZSCNQ06_M2[neet$ZSCNQ06_M2 ==-3] <- NA
neet$ZSCNQ06_M3[neet$ZSCNQ06_M3 ==-3] <- NA


neet<- neet %>% 
  mutate(
    neet_educ = case_when(
      is.na(ZSCNQ06_M1)& is.na(ZSCNQ06_M2) & is.na(ZSCNQ06_M3) ~ NA_real_, #####if all 3 are NA = NA
      ZSCNQ06_M1 == 1 | ZSCNQ06_M2 == 2 ~ 0, ##If in education = 0
      ZSCNQ06_M3 == 3 |ZSCNQ06_M1 == -4|ZSCNQ06_M2 == -4|ZSCNQ06_M3 == -4 ~ 1, ###if not in education = 1 (bcs NEET). -4 were not asked this question as they indicated in prior question that they did not go to school in the past month
      TRUE ~ 0
    )
  )
View(neet)

table(neet$neet_educ, useNA = "ifany")


neet[1:20,c("ZSCNQ06_M1", "ZSCNQ06_M2", "ZSCNQ06_M3","neet_educ")] 




#Employment question

table(neet$ZTVNQ01, useNA = "ifany") 
neet$ZTVNQ01[neet$ZTVNQ01 ==-3] <- NA ###did not respond to employment questions
neet$ZTVNQ01[neet$ZTVNQ01 == 1] <- 0 ##employed = 0
neet$ZTVNQ01[neet$ZTVNQ01 == 2] <- 1 ##unemployed = 1


#combining
neet<- neet %>% 
  mutate(
    neet = case_when(
      ZTVNQ02A_M13 == 13 ~ 0, ###making ppl on pat/mat leave as employed (not NEET)
      is.na(neet_educ)& is.na(ZTVNQ01) ~ NA_real_, #####if both are NA
      neet_educ == 1 & ZTVNQ01 == 1 ~ 1, ##If not in educ AND not in empl = 1 (NEET)
      neet_educ == 0 |ZTVNQ01 == 0 ~ 0, #covers 0 with NA too
      (is.na(neet_educ) | is.na(ZTVNQ01)) &
        (neet_educ == 1 | ZTVNQ01 == 1) ~ NA_real_,
      TRUE~NA_real_
    )
  )
View(neet)

table(neet$neet, useNA = "ifany")



neet[1:20,c("neet_educ", "ZTVNQ01", "neet")] 


subset(neet, is.na(neet_educ), select = c("neet", "neet_educ", "ZTVNQ01")) # filtering out the NA's for NEET to see what the pattern is (are they true NAs)


subset(neet, ZTVNQ02A_M13 == 13, select = c("ZTVNQ02A_M13", "neet", "neet_educ", "ZTVNQ01")) # filtering out the NA's for NEET to see what the pattern is (are they true NAs)
table(neet$ZTVNQ02A_M13, useNA = "ifany") ##28 on parental leave

table(neet$neet, useNA = "ifany") ##88 neet

## neet = 88

finalData <- merge(finalData, neet, all=TRUE, by="IDME")

table(finalData$neet) # still 88 NEET

# Save file
write.csv(finalData, "/mnt/40295D00/Travail/Commun/Ashley/finalData.csv", row.names=FALSE)

#Use to load and view new file
finalData<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/finalData.csv")
View(finalData)


##SUICIDE EXPOSURE VARIABLE 

###WAVE 13 (AGE 13)###
library(dplyr)
#Suicidal ideation - Age 13
table(finalData$W14_SUI_idea, useNA = "ifany")
finalData$W14_SUI_idea[finalData$W14_SUI_idea == -2|finalData$W14_SUI_idea == -1] <- NA
finalData$W14_SUI_idea[finalData$W14_SUI_idea == 1] <- 0
finalData$W14_SUI_idea[finalData$W14_SUI_idea == 2|finalData$W14_SUI_idea == 3|finalData$W14_SUI_idea == 4] <- 1

table(finalData$W14_SUI_idea,useNA = "ifany") #checks how many NA vars exist in pass_idea
finalData[is.na(finalData$W14_SUI_idea),c("W14_srs_SUI_idea","W14_att")] <- NA #makes these vars NA if SUI_idea = NA

finalData$early_onset <- finalData$W14_SUI_idea
table(finalData$early_onset)

#Serious suicidal ideation - Age 13
table(finalData$W14_srs_SUI_idea, useNA = "ifany")
finalData$W14_srs_SUI_idea[finalData$W14_srs_SUI_idea ==-1] <- NA
finalData[is.na(finalData$W14_srs_SUI_idea), c("W14_att")] <- NA ##makes att NA if srs_SUI_idea is NA
finalData$W14_srs_SUI_idea[finalData$W14_srs_SUI_idea ==-4|finalData$W14_srs_SUI_idea == 2] <- 0

#Suicide attempts - Age 13
table(finalData$W14_att, useNA = "ifany")
finalData$W14_att[finalData$W14_att == -4|finalData$W14_att ==3] <- 0
finalData$W14_att[finalData$W14_att == 1|finalData$W14_att ==2] <- 1

###WAVE 16 (AGE 15)###

#Suicidal ideation - Age 15
table(finalData$W16_SUI_idea, useNA = "ifany")

finalData$W16_SUI_idea[finalData$W16_SUI_idea == -3] <- NA
finalData$W16_SUI_idea[finalData$W16_SUI_idea == 1] <- 0
finalData$W16_SUI_idea[finalData$W16_SUI_idea == 2|finalData$W16_SUI_idea ==3|finalData$W16_SUI_idea ==4] <- 1

#Serious suicidal ideation - Age 15
table(finalData$W16_srs_SUI_idea, useNA = "ifany")

finalData$W16_srs_SUI_idea[finalData$W16_srs_SUI_idea == -3] <- NA
finalData$W16_srs_SUI_idea[finalData$W16_srs_SUI_idea == -4|finalData$W16_srs_SUI_idea ==2] <- 0

#Suicide attempts - Age 15
table(finalData$W16_att, useNA = "ifany")

finalData$W16_att[finalData$W16_att == -3] <- NA
finalData$W16_att[finalData$W16_att == -4|finalData$W16_att == 3] <- 0
finalData$W16_att[finalData$W16_att == 1|finalData$W16_att ==2] <- 1

###WAVE 18 (AGE 17)###

#Suicidal ideation - Age 17 
table(finalData$W18_SUI_idea, useNA = "ifany")

finalData$W18_SUI_idea[finalData$W18_SUI_idea ==-3] <- NA
finalData$W18_SUI_idea[finalData$W18_SUI_idea ==1] <- 0
finalData$W18_SUI_idea[finalData$W18_SUI_idea == 2|finalData$W18_SUI_idea ==3|finalData$W18_SUI_idea ==4] <- 1

table(finalData$W18_SUI_idea,useNA = "ifany") #checks how many NA vars exist in pass_idea
finalData[is.na(finalData$W18_SUI_idea),c("W18_srs_SUI_idea","W18_att")] <- NA #makes these vars NA if pass_idea = NA

#Serious suicidal ideation - Age 17
table(finalData$W18_srs_SUI_idea, useNA = "ifany")

finalData$W18_srs_SUI_idea[finalData$W18_srs_SUI_idea == -3] <-NA
finalData$W18_srs_SUI_idea[finalData$W18_srs_SUI_idea == -4|finalData$W18_srs_SUI_idea == 2] <- 0

table(W18_SUI$W18_srs_SUI_idea,useNA = "ifany") #checks how many NA vars exist in pass_idea
finalData[is.na(W18_SUI$W18_srs_SUI_idea),c("W18_att")] <- NA #makes attempt NA if srs_SUI_idea = NA

#Suicide attempts - Age 17
table(finalData$W18_att, useNA = "ifany")

finalData$W18_att[finalData$W18_att == -4|finalData$W18_att ==3]<- 0
finalData$W18_att[finalData$W18_att ==1|finalData$W18_att == 2] <- 1

##################
###LIFETIME EXP###
##################

####IDEATION LIFE####
finalData$idea_life <- apply(finalData[,c("W14_SUI_idea", "W16_SUI_idea", "W18_SUI_idea")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})

finalData[1:20,c("W14_SUI_idea", "W16_SUI_idea", "W18_SUI_idea","idea_life")] #check code worked
table(finalData$idea_life, useNA = "ifany")
#### SERIOUS IDEA LIFE ####
finalData$idea_srs_life <- apply(finalData[,c("W14_srs_SUI_idea", "W16_srs_SUI_idea", "W18_srs_SUI_idea")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})

finalData[1:50,c("W14_srs_SUI_idea", "W16_srs_SUI_idea", "W18_srs_SUI_idea","idea_srs_life")] #check code worked
table(finalData$idea_srs_life, useNA = "ifany")
#### ATTEMPT LIFE ####
finalData$att_life <- apply(finalData[,c("W14_att", "W16_att", "W18_att")],1, function(x){
  if(all(is.na(x))){
    return(NA) #keep NA if all values are NA
  }else {
    return(ifelse(sum(x==1, na.rm=TRUE)>0,1,0)) #sum and set as one if any one's exist, otherwise code as 0
  }
})

finalData[1:50,c("W14_att", "W16_att", "W18_att","att_life")] #check code worked
table(finalData$att_life, useNA = "ifany")

##RECURRING


#RECURRING SUICIDE IDEATION
finalData <- finalData %>% 
  mutate (idea_recur = if_else(
    if_all(c(W14_SUI_idea, W16_SUI_idea, W18_SUI_idea), is.na),
    NA_real_,
    rowSums(across(c(W14_SUI_idea, W16_SUI_idea, W18_SUI_idea)), na.rm = TRUE)
  ))
table(finalData$idea_recur, useNA = "ifany")

finalData$idea_recur_bin <- finalData$idea_recur

table(finalData$idea_recur_bin, useNA = "ifany")

finalData$idea_recur_bin[finalData$idea_recur_bin == 0] <- NA
finalData$idea_recur_bin[finalData$idea_recur_bin == 1] <- 0 # comparison group with idea only once
finalData$idea_recur_bin[finalData$idea_recur_bin == 2|finalData$idea_recur_bin ==3] <- 1 # people with multiple idea




############
####ONSET###
############


#ONSET OF SUICIDAL IDEATION
finalData <- finalData %>% 
  mutate (late_onset = if_else(
    if_all(c( W16_SUI_idea, W18_SUI_idea), is.na),
    NA_real_,
    rowSums(across(c(W16_SUI_idea, W18_SUI_idea)), na.rm = TRUE)
  ))

table(finalData$late_onset, useNA = "ifany")

# need to remove early onset peeps from late onset (making NA for now)
finalData$late_onset[finalData$late_onset == 1|finalData$late_onset == 2] <-1 
finalData$late_onset[finalData$early_onset == 1] <- NA #if early onset = 1, late onset is NA
finalData$late_onset[finalData$late_onset == 1] <-2 ##320 had late onset, 152 had early onset

# combining early/late onset to make categorical var: none = 0, early = 1, late = 2
finalData <- finalData %>% 
  mutate (onset = if_else(
    if_all(c(early_onset, late_onset), is.na),
    NA_real_,
    rowSums(across(c(early_onset, late_onset)), na.rm = TRUE)
  ))

table(finalData$onset, useNA = "ifany")
table(finalData$onset, useNA = "ifany")
#### SAVE AGAIN ####
write.csv(finalData,"/mnt/40295D00/Travail/Commun/Ashley/Data/finalData.csv")
View(finalData) #N = 2120 (all people)


##Recoding educ
#HS
table(finalData$hs_obtained)

finalData$hs_obtained[finalData$hs_obtained == 2]<- 0

#DEP
table(finalData$dep_obtained)

finalData$dep_obtained[finalData$dep_obtained == 2]<- 0

#other profess
table(finalData$otherqual_obtained)

finalData$otherqual_obtained[finalData$otherqual_obtained == 2]<- 0

#DEC
table(finalData$dec_obtained)

finalData$dec_obtained[finalData$dec_obtained == 2]<- 0

#UNI
table(finalData$uni_obtained)

finalData$uni_obtained[finalData$uni_obtained == 2]<- 0

#other
table(finalData$otherdiploma_obtained)

finalData$otherdiploma_obtained[finalData$otherdiploma_obtained == 2]<- 0


##Creating binary educ variables: 1 = highschool diploma or more, 0 = no highschool

finalData$educ_bin <- apply(
  finalData[, c("hs_obtained", "dep_obtained", "dec_obtained",
               "otherqual_obtained", "uni_obtained", "otherdiploma_obtained")],
  1,
  function(x) {
    x <- as.numeric(x)
    if(all(is.na(x))) {
      return(NA_integer_)
    }else{
      return(ifelse(any(x==1, na.rm = TRUE), 1L, 0L))
    }
    
  }
)

finalData[1:50,c("hs_obtained", "dep_obtained", "dec_obtained", "otherqual_obtained", "uni_obtained", "otherdiploma_obtained", "educ_bin")] #check code worked
table(finalData$educ_bin, useNA = "ifany")

finalData <- finalData %>% 
  mutate(across(c(hs_obtained, dep_obtained, dec_obtained, otherqual_obtained, uni_obtained), ~ na_if(na_if(.x,-3),-4)))

###Educ cat####
library(dplyr)

finalData<- finalData %>% 
  mutate(
    educ_cat = case_when(
      is.na(hs_obtained)& is.na(dep_obtained) & is.na(dec_obtained) & is.na(otherqual_obtained) & is.na(uni_obtained) & is.na(otherdiploma_obtained) ~ NA_real_,
      uni_obtained == 1 ~ 3,
      dep_obtained == 1 | dec_obtained == 1 ~ 2,
      hs_obtained == 1 | otherqual_obtained == 1 | otherdiploma_obtained == 1 ~ 1,
      hs_obtained == 0 & dep_obtained == 0 & dec_obtained == 0 & otherqual_obtained == 0 & uni_obtained == 0 & otherdiploma_obtained == 0 ~ 0,
      TRUE ~ 0
    )
  )
View(finalData)

table(finalData$educ_cat, useNA = "ifany")

check <- finalData %>% 
  filter(otherdiploma_obtained == 1) %>% 
  select(otherdiploma_obtained, hs_obtained, dep_obtained, dec_obtained, otherqual_obtained, uni_obtained, educ_cat)

View(check)
names(finalData)

table(finalData$educ_bin)


##Debt binary - debt vs. no debt ##REPLACE DF BY FINALDATA
finalData <- finalData %>% 
  mutate(
    debt_nodebt = case_when(
      W26_debt_perception == 1 ~ 0,
      W26_debt_perception %in%  c(2, 3, 4, 5) ~ 1,
      TRUE ~ NA
    )
  )

table(finalData$debt_nodebt, useNA = "ifany")
table(finalData$W26_debt_perception, useNA = "ifany")



###Housing stability binary
finalData <- finalData %>% 
  mutate(
    stable_unstable = case_when(
      W26_housing_moved %in%  c(1, 2, 3) ~ 0,
      W26_housing_moved %in%  c(4, 5, 6, 7, 8, 9, 10) ~ 1,
      TRUE ~ NA
    )
  )


table(finalData$stable_unstable, useNA = "ifany")
table(finalData$W26_housing_moved, useNA = "ifany")

###Educational attainment binary
finalData <- finalData %>% 
  mutate(
    hs_highereduc = case_when(
      educ_cat == 1 ~ 0,
      educ_cat %in%  c(2, 3) ~ 1,
      TRUE ~ NA
    )
  )

table(finalData$hs_highereduc, useNA = "ifany")
table(finalData$educ_cat, useNA = "ifany")

write.csv(finalData, "/mnt/40295D00/Travail/Commun/Ashley/Data/finalData.csv", row.names=FALSE)


View(finalData)
names(finalData)


######make copy of finaldata, call it analysis_sample and look for code that drops people with NA for ideation 

#### Determining N for project ####
analysis_sample <- finalData
dim(analysis_sample)
#remove people who did not answer any ideation (because they did not answer the other exposures)
analysis_sample <- analysis_sample[!is.na(analysis_sample$idea_life),] #1540!! (if not remove people with all missing SES)


View(analysis_sample)

names(analysis_sample)
table(analysis_sample$W26_housing_moved, useNA = "ifany")

table(analysis_sample$neet)


#########################################
#### TABLE 1: Sample Characteristics ####
#########################################
#### Assess diffs between kept vs removed + gather group values ####
# Create variable to indicate who was kept v removed

finalData$kept <- ifelse(finalData$IDME %in% analysis_sample$IDME, 1, 0) #0 = removed, 1 = kept (finalData = table w all 2120 peeps, analysis_sample = table w your peeps)

table(finalData$kept)

# Chi-squared test for sex, race, fam structure, paternal/maternal antisocial features (binary variables) - adapt for other covariates by changing "sex"
#Sex
table(finalData$kept, finalData$W1_sex) # 0 = female, 1 = male
chisq.test(finalData$kept, finalData$W1_sex) #DIFF 2.2e-16

#Race
table(finalData$kept, finalData$W1_race) # 0 = non-white, 1 = white
chisq.test(finalData$kept, finalData$W1_race) #DIFF 1.096e-08

#Fam Structure
table(finalData$kept, finalData$W1_fam_str) # 0 = not intact, 1 = intact
chisq.test(finalData$kept, finalData$W1_fam_str) #DIFF 0.1596

#Maternal antisocial features
table(finalData$kept, finalData$W1_antisoc_mat_bin) # 0 = no antisoc, 1 = antisoc
chisq.test(finalData$kept, finalData$W1_antisoc_mat_bin) #DIFF 0.5243

#Paternal antisocial features
table(finalData$kept, finalData$W1_antisoc_pat_bin) # 0 = no antisoc, 1 = antisoc
chisq.test(finalData$kept, finalData$W1_antisoc_pat_bin) #DIFF 0.82


# example output
# table(final_table_V1$kept, final_table_V1$sex) # 0 = female, 1 = male
#   0   1
# 0 190 297 # this line represents those removed (0) - should equal N removed (in your case is 855)
# 1 850 783 # this line represents those kept in the analyses (1) - should equal N kept (in your case is 1265)
# the 0/1 above represents the variable you are assessing (this case = sex where 0 = female + 1 = male)
# add these numbers to table and get percentage for each group as well

# chisq.test(final_table_V1$kept, final_table_V1$sex)
# Pearson's Chi-squared test with Yates' continuity correction
# data:  final_table_V1$kept and final_table_V1$sex
# X-squared = 24.994, df = 1, p-value = 5.752e-07 #P=VALUE IS WHAT IS IMPORTANT HERE

# t-test for family SES, maternal/paternal depression, age 12 mh (continuous variable) - adapt for other covariates by changing "SES"
#Fam SES
aggregate(W1_ses ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_ses ~ kept, data = finalData) # DIFF p = 7.483e-09

#Maternal depression
aggregate(W1_dep_mat ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_dep_mat ~ kept, data = finalData) # DIFF p = 0.0008992

#Paternal depression
aggregate(W1_dep_pat ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_dep_pat ~ kept, data = finalData) # DIFF p = 0.7272

#Age 12 Depression
aggregate(W1_dep ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_dep ~ kept, data = finalData) # DIFF p = 0.3194

#Age 12 Anxiety
aggregate(W1_anx ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_anx ~ kept, data = finalData) # DIFF p = 0.5625

#Age 12 Hyperactivity-Inattention
aggregate(W1_hyper_att ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_hyper_att ~ kept, data = finalData) # DIFF p = 0.0009232

#Age 12 Opposition
aggregate(W1_opposition ~ kept, finalData, 
          function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
t.test(W1_opposition ~ kept, data = finalData) # DIFF p = 0.0011

# example output
#aggregate(SES ~ kept, final_table_V1, function(x) c(mean= mean(x), sd = sd(x), n = length(x)))
#kept      SES.mean        SES.sd         SES.n
#1    0   -0.23216869    0.96999105  476.00000000 # mean, standard deviation, + N of those removed: -0.23(0.97) N = 476
#2    1    0.06042016    1.00261552 1619.00000000 # mean, standard deviation, + N of those kept: 0.06(1.00) N = 1619

#t.test(SES ~ kept, data = final_table_V1) 
#Welch Two Sample t-test
#data:  SES by kept
#t = -5.7409, df = 797.19, p-value = 1.339e-08 # IMPORTANCE HERE IS THE P-VALUE (less than 0.05 = groups sig different)
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -0.3926325 -0.1925452
#sample estimates:
#  mean in group 0 mean in group 1 
#-0.23216869      0.06042016 # MEANS SHOULD BE THE SAME AS ABOVE CODE

# CAN ADD FINDINGS TO TABLE 1!!!

##########################################
#### TABLE 2: Suicide x SES Variables ####
##########################################

#### Included Variables ####
View(df)
names(df)
SES <- c("educ_bin", "stable_unstable", "hs_highereduc", "debt_nodebt", "W26_empl_status", "W26_housing_cond", "neet")
suicide <- c("idea_life","idea_recur_bin", "onset","idea_srs_life","att_life") 

#### Variable descriptives ####
# Summary loop - SES 
for(var in SES){
  cat("\n----------------------------\n")
  cat("Variable:",var,"\n")
  tab <- table(analysis_sample[[var]],useNA = "no") #excludes NAs from table
  pct <- round(100*prop.table(tab), 1)
  total_responses <- sum(tab) #total of non-missing responses
  summary_df<- data.frame(
    Category = names(tab),
    n = as.vector(tab),
    percent = pct
  )
  print(summary_df)
  cat("Total responses (non-missing):", total_responses, "\n")
}

# Summary loop - suicide 
for(var in suicide){
  cat("\n----------------------------\n")
  cat("Variable:",var,"\n")
  tab <- table(analysis_sample[[var]],useNA = "no") #excludes NAs from table
  pct <- round(100*prop.table(tab), 1)
  total_responses <- sum(tab) #total of non-missing responses
  summary_df<- data.frame(
    Category = names(tab),
    n = as.vector(tab),
    percent = pct
  )
  print(summary_df)
  cat("Total responses (non-missing):", total_responses, "\n")
}

#### SES x suicide tables ####

####INCLUDED VARIABLES###
for(ses in SES){
  for (su in suicide){
    cat("\n=============================\n")
    cat("Table:", su, "BY", ses, "\n")
    cat("===========================\n")
    
    tab <- xtabs(as.formula(paste("~", su, "+", ses)), data = analysis_sample)
    print(tab)
  }
}



sex_idea <- analysis_sample %>% 
  count(W1_sex, idea_life)

sex_idea_recur <- analysis_sample %>% 
  count(W1_sex, idea_recur_bin)

sex_idea_onset <- analysis_sample %>% 
  count(W1_sex, onset)

sex_srs_idea <- analysis_sample %>% 
  count(W1_sex, idea_srs_life)


sex_att <- analysis_sample %>% 
  count(W1_sex, att_life)


write.csv(analysis_sample, "/mnt/40295D00/Travail/Commun/Ashley/Data/df.csv", row.names=FALSE)
View(analysis_sample)

#######FINAL VARIABLES#####
finaldf <- analysis_sample   ####if ever want to run non-imputed results, use analysis_sample
View(finaldf)
names(finaldf)
finaldf <- finaldf %>% dplyr::select(IDME, W1_sex, W1_ses, W1_fam_str, W1_dep_mat, W1_dep_pat, W1_race, W1_anx, W1_hyper_att, W1_opposition, W1_dep, W1_antisoc_mat_bin, W1_antisoc_pat_bin,
                                     W26_empl_status, W26_housing_moved, W26_housing_cond, neet, educ_bin, debt_nodebt, stable_unstable, hs_highereduc,
                                     idea_life, idea_srs_life, att_life, idea_recur_bin, onset)

table(finaldf$neet)

####################
#### IMPUTATION ####
####################
#### missForest ####
library(missForest)
# convert vars from int to factor
finaldf <- finaldf %>% 
  mutate_at(vars(W1_sex, W1_ses, W1_fam_str, W1_dep_mat, W1_dep_pat, W1_race, W1_anx, W1_hyper_att, W1_opposition, W1_dep, W1_antisoc_mat_bin, W1_antisoc_pat_bin,
                 W26_empl_status, W26_housing_moved, W26_housing_cond, neet, educ_bin, debt_nodebt, stable_unstable, hs_highereduc,
                 idea_life, idea_srs_life, att_life, idea_recur_bin, onset), factor)

names(finaldf)
# save finaldf #
write.csv(finaldf, "/mnt/40295D00/Travail/Commun/Ashley/Data/finaldf.csv", row.names=FALSE)
#Use to load and view new file
finaldf<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/finaldf.csv")
View(finaldf)

vars_imp <- c("W1_sex", "W1_ses", "W1_fam_str", "W1_dep_mat", "W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att",
              "W1_opposition", "W1_dep", "W1_antisoc_mat_bin", "W1_antisoc_pat_bin", "W26_empl_status", "W26_housing_moved",
              "W26_housing_cond", "neet", "educ_bin", "debt_nodebt", "stable_unstable", "hs_highereduc","idea_life","idea_srs_life", "att_life", "idea_recur_bin", "onset" )

# run missForest on selected vars
set.seed(123)
imp_result <- missForest(finaldf) 
# extract imputed dataset
imp_all <- imp_result$ximp
# recombine with other vars
final_imp <- finaldf
final_imp[ , vars_imp] <- imp_all[ , vars_imp]

#### save imputed table ####
write.csv(final_imp, "/mnt/40295D00/Travail/Commun/Ashley/Data/final_imp.csv", row.names=FALSE)
#Use to load and view new file
final_imp<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/final_imp.csv")
View(final_imp)

table(finaldf$debt_nodebt, useNA = "ifany")
table(final_imp$debt_nodebt, useNA = "ifany")



final_imp[1:20,c("neet", "educ_bin")] 
names(final_imp)

###############################
######## NON-IMPUTED  #########
###############################

#### UNADJUSTED LIFETIME REGRESION MODELS ####

p_values <- c(0.01, 0.04, 0.03, 0.25, 0.07)
p_adjusted <- p.adjust(p_values, method = "bonferroni")
print(p_adjusted)



#UNADJUSTED INDIVIDUAL MODELS#
### Lifetime PCs to lifetime suicide ###
# Creating loop function to view results for each individual regression against lifetime outcomes 
# Defining variables 
exposures_life <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin", "onset")
outcomes_life <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
# List to store results
results_list <- list()
# Loop through exposures + outcomes 
for(exposure in exposures_life) {
  for (outcome in outcomes_life) {
    
    # create unadjusted model formula
    formula_str <- paste(outcome, "~", exposure)
    model <- glm(as.formula(formula_str), data = analysis_sample, family = binomial)
    
    # extract values 
    est <- coef(model)[exposure] # log odds
    OR <- exp(est) # odds ratio
    CI <- exp(confint(model)[exposure, ]) #95% CI for OR
    p_val <- summary(model)$coefficients[exposure, "Pr(>|z|)"]
    
    # determine significance
    sig_flag <- ifelse(p_val < 0.05, "Yes", "No")
    
    # save results
    results_list[[paste(exposure, outcome, sep = "_")]] <- data.frame(
      Exposure = exposure,
      Outcome = outcome,
      Estimate = round(est, 3),
      OR = round(OR, 2),
      CI_Lower = round(CI[1],2),
      CI_Upper = round(CI[2],2),
      p_value = round(p_val, 4),
      Significant = sig_flag
    )
  }
} # warnings, most likely related to variables with separation (e.g., heart - everyone with heart has no attempt) - not worrisome 
# Combine results into one data frame
results_life_unadj_1540 <- do.call(rbind, results_list)
rownames(results_life_unadj_1540) <- NULL
print(results_life_unadj_1540)
View(results_life_unadj_1540)

library(openxlsx)
write.xlsx(results_life_unadj_1540, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_life_unadj_1540.xlsx")



#### Unadjusted Ordinal logistic regression for housing stability ####

#Housing stab/housing moved & att
class(analysis_sample)
str(analysis_sample)
head(analysis_sample)
library(MASS)
analysis_sample$W26_housing_moved <- factor(analysis_sample$W26_housing_moved,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                     ordered = TRUE
                     )
housmoved_att <-polr(W26_housing_moved ~ att_life, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_att)


ci_prof <- confint(housmoved_att)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_att))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_att))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Hous_moved & srs sui idea
analysis_sample$W26_housing_moved <- factor(analysis_sample$W26_housing_moved,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                     ordered = TRUE)
housmoved_srsidea <-polr(W26_housing_moved ~ idea_srs_life, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_srsidea)


ci_prof <- confint(housmoved_srsidea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_srsidea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_srsidea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Hous moved & sui idea
analysis_sample$W26_housing_moved <- factor(analysis_sample$W26_housing_moved,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                     ordered = TRUE)
housmoved_idea <-polr(W26_housing_moved ~ idea_life, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_idea)

ci_prof <- confint(housmoved_idea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_idea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_idea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Housmoved & idea recur
analysis_sample$W26_housing_moved <- factor(analysis_sample$W26_housing_moved,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                     ordered = TRUE)
housmoved_recuridea <-polr(W26_housing_moved ~ idea_recur_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_recuridea)

ci_prof <- confint(housmoved_recuridea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_recuridea))
OR_CI <- exp(ci_prof)

cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_recuridea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Housmoved & onset
table(analysis_sample$W26_housing_moved, useNA = "ifany")

analysis_sample$W26_housing_moved <- factor(analysis_sample$W26_housing_moved,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                     ordered = TRUE)
housmoved_onset <-polr(W26_housing_moved ~ onset, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_onset)

ci_prof <- confint(housmoved_onset)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_onset))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_onset))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#### ADJUSTED INDIVIDUAL MODELS (LIFETIME SUICIDE ONLY) ####
# Lifetime PCs to lifetime suicide #
# Creating loop function to view results for each individual regression against lifetime outcomes 
# Defining variables 
exposures_life <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
outcomes_life <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
covariates <- c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                "W1_antisoc_mat_bin","W1_antisoc_pat_bin")
# List to store results
results_list <- list()
# Loop through exposures + outcomes 
for(exposure in exposures_life) {
  for (outcome in outcomes_life) {
    
    # create adjusted model formula
    formula_str <- paste(outcome, "~", paste(c(exposure, covariates), collapse = "+"))
    model <- glm(as.formula(formula_str), data = analysis_sample, family = binomial)
    
    # extract values 
    est <- coef(model)[exposure] # log odds
    OR <- exp(est) # odds ratio
    CI <- exp(confint(model)[exposure, ]) #95% CI for OR
    p_val <- summary(model)$coefficients[exposure, "Pr(>|z|)"]
    
    # determine significance
    sig_flag <- ifelse(p_val < 0.05, "Yes", "No")
    
    # save results
    results_list[[paste(exposure, outcome, sep = "_")]] <- data.frame(
      Exposure = exposure,
      Outcome = outcome,
      Estimate = round(est, 3),
      OR = round(OR, 2),
      CI_Lower = round(CI[1],2),
      CI_Upper = round(CI[2],2),
      p_value = round(p_val, 4),
      Significant = sig_flag
    )
  }
} # warnings, most likely related to variables with separation (e.g., heart - everyone with heart has no attempt) - not worrisome 
# Combine results into one data frame
results_life_adj_1540 <- do.call(rbind, results_list)
rownames(results_life_adj_1540) <- NULL
print(results_life_adj_1540)
View(results_life_adj_1540)

library(openxlsx)
write.xlsx(results_life_adj_1540, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_life_adj_1540.xlsx")


#### Adjusted Housmoved #####

##Adj housmoved & att
housmoved_att_adj <-polr(W26_housing_moved ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_att_adj)

ci_prof <- confint(housmoved_att_adj)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_att_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_att_adj))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Adj housmoved & srs sui idea
housmoved_srsidea_adj <-polr(W26_housing_moved ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_srsidea_adj)

ci_prof <- confint(housmoved_srsidea_adj) 
scale
OR <- exp(coef(housmoved_srsidea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_srsidea_adj)) 
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)



#Adj housmoved & sui idea
housmoved_idea_adj <-polr(W26_housing_moved ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_idea_adj)

ci_prof <- confint(housmoved_idea_adj)
scale
OR <- exp(coef(housmoved_idea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_idea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj housmoved & idea recur
housmoved_recuridea_adj <-polr(W26_housing_moved ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_recuridea_adj)

ci_prof <- confint(housmoved_recuridea_adj)
scale
OR <- exp(coef(housmoved_recuridea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_recuridea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj housmoved & onset
housmoved_onset_adj <-polr(W26_housing_moved ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(housmoved_onset_adj)

ci_prof <- confint(housmoved_onset_adj)
scale
OR <- exp(coef(housmoved_onset_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_onset_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)









###########################
######## IMPUTED ##########
###########################

#### UNADJUSTED LIFETIME REGRESION MODELS ####
p_values <- c(0.01, 0.04, 0.03, 0.25, 0.07)
p_adjusted <- p.adjust(p_values, method = "bonferroni")
print(p_adjusted)
## UNADJUSTED INDIVIDUAL MODELS##
### Lifetime PCs to lifetime suicide ###
# Creating loop function to view results for each individual regression against lifetime outcomes 
# Defining variables 
exposures_life <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
outcomes_life <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
# List to store results
results_list <- list()
# Loop through exposures + outcomes 
for(exposure in exposures_life) {
  for (outcome in outcomes_life) {
    
    # create unadjusted model formula
    formula_str <- paste(outcome, "~", exposure)
    model <- glm(as.formula(formula_str), data = final_imp, family = binomial)
    
    # extract values 
    est <- coef(model)[exposure] # log odds
    OR <- exp(est) # odds ratio
    CI <- exp(confint(model)[exposure, ]) #95% CI for OR
    p_val <- summary(model)$coefficients[exposure, "Pr(>|z|)"]
    
    # determine significance
    sig_flag <- ifelse(p_val < 0.05, "Yes", "No")
    
    # save results
    results_list[[paste(exposure, outcome, sep = "_")]] <- data.frame(
      Exposure = exposure,
      Outcome = outcome,
      Estimate = round(est, 3),
      OR = round(OR, 2),
      CI_Lower = round(CI[1],2),
      CI_Upper = round(CI[2],2),
      p_value = round(p_val, 4),
      Significant = sig_flag
    )
  }
} 
# Combine results into one data frame
results_life_unadj_impt <- do.call(rbind, results_list)
rownames(results_life_unadj_impt) <- NULL
print(results_life_unadj_impt)
View(results_life_unadj_impt)

library(openxlsx)
write.xlsx(results_life_unadj_impt, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_life_unadj_impt.xlsx")



#### UNADJUSTED NEGATIVE BINOMIAL REGRESSION for housing stability ####
#Housing stab/housing moved & att
class(final_imp)
str(final_imp)
head(final_imp)
library(MASS)
library(broom)
final_imp$W26_housing_moved_num <- as.numeric(as.character(final_imp$W26_housing_moved))
  
  
housmoved_att <-glm.nb(W26_housing_moved_num ~ att_life, data = final_imp)
summary(housmoved_att)

broom::tidy(housmoved_att, exponentiate = TRUE, conf.int = TRUE)


#Hous_moved & srs sui idea
housmoved_srsidea <-glm.nb(W26_housing_moved_num ~ idea_srs_life, data = final_imp)
summary(housmoved_srsidea)

broom::tidy(housmoved_srsidea, exponentiate = TRUE, conf.int = TRUE)

#Hous moved & sui idea
housmoved_idea <-glm.nb(W26_housing_moved_num ~ idea_life, data = final_imp)
summary(housmoved_idea)

broom::tidy(housmoved_idea, exponentiate = TRUE, conf.int = TRUE)

#Housmoved & idea recur
housmoved_recuridea <-glm.nb(W26_housing_moved_num ~ idea_recur_bin, data = final_imp)
summary(housmoved_recuridea)

broom::tidy(housmoved_recuridea, exponentiate = TRUE, conf.int = TRUE)

#### ADJUSTED INDIVIDUAL MODELS (LIFETIME SUICIDE ONLY) ####
# Lifetime PCs to lifetime suicide #
# Creating loop function to view results for each individual regression against lifetime outcomes 
# Defining variables 
exposures_life <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
outcomes_life <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
covariates <- c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                "W1_antisoc_mat_bin","W1_antisoc_pat_bin")
# List to store results
results_list <- list()
# Loop through exposures + outcomes 
for(exposure in exposures_life) {
  for (outcome in outcomes_life) {
    
    # create adjusted model formula
    formula_str <- paste(outcome, "~", paste(c(exposure, covariates), collapse = "+"))
    model <- glm(as.formula(formula_str), data = final_imp, family = binomial)
    
    # extract values 
    est <- coef(model)[exposure] # log odds
    OR <- exp(est) # odds ratio
    CI <- exp(confint(model)[exposure, ]) #95% CI for OR
    p_val <- summary(model)$coefficients[exposure, "Pr(>|z|)"]
    
    # determine significance
    sig_flag <- ifelse(p_val < 0.05, "Yes", "No")
    
    # save results
    results_list[[paste(exposure, outcome, sep = "_")]] <- data.frame(
      Exposure = exposure,
      Outcome = outcome,
      Estimate = round(est, 3),
      OR = round(OR, 2),
      CI_Lower = round(CI[1],2),
      CI_Upper = round(CI[2],2),
      p_value = round(p_val, 4),
      Significant = sig_flag
    )
  }
} 
# Combine results into one data frame
results_life_adj_imp <- do.call(rbind, results_list)
rownames(results_life_adj_imp) <- NULL
print(results_life_adj_imp)
View(results_life_adj_imp)

library(openxlsx)
write.xlsx(results_life_adj_imp, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_life_adj_imp.xlsx")


####ADJUSTED NEGATIVE BINOMIAL REGRESSION for housing stability####

#sui att
housmoved_att_adj <-glm.nb(W26_housing_moved_num ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att
                         + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp)
summary(housmoved_att_adj)

broom::tidy(housmoved_att_adj, exponentiate = TRUE, conf.int = TRUE)

#Adj housmoved & srs sui idea
housmoved_srsidea_adj <-glm.nb(W26_housing_moved_num ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat
                             + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp)
summary(housmoved_srsidea_adj)

broom::tidy(housmoved_srsidea_adj, exponentiate = TRUE, conf.int = TRUE)

#Adj housmoved & sui idea
housmoved_idea_adj <-glm.nb(W26_housing_moved_num ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att 
                          + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp)
summary(housmoved_idea_adj)

broom::tidy(housmoved_idea_adj, exponentiate = TRUE, conf.int = TRUE)

#Adj housmoved & idea recur
housmoved_recuridea_adj <-glm.nb(W26_housing_moved_num ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + 
                                   W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp)
summary(housmoved_recuridea_adj)


broom::tidy(housmoved_recuridea_adj, exponentiate = TRUE, conf.int = TRUE)



########################################
####NONE VS EARLY/NONE VS LATE onset####
########################################

###UNADJUSTED, IMPUTED###

#debt/no debt
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_debtnone <- glm(debt_nodebt~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_debtnone)
exp(cbind(OR = coef(model_debtnone), confint(model_debtnone)))

#high school/no high school (educ_bin)
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_hsnone <- glm(educ_bin~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_hsnone)
exp(cbind(OR = coef(model_hsnone), confint(model_hsnone)))

#hs/higher educ 
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_highereducnone <- glm(hs_highereduc~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_highereducnone)
exp(cbind(OR = coef(model_highereducnone), confint(model_highereducnone)))

#unsanitary housing
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unsanitarynone <- glm(W26_housing_cond~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_unsanitarynone)
exp(cbind(OR = coef(model_unsanitarynone), confint(model_unsanitarynone)))

#unstable housing
names(final_imp)

final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unstablenone <- glm(stable_unstable~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_unstablenone)
exp(cbind(OR = coef(model_unstablenone), confint(model_unstablenone)))

#unemployment
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_emplnone <- glm(W26_empl_status~onset, family = "binomial", data = final_imp)
summary(model_emplnone)
exp(cbind(OR = coef(model_emplnone), confint(model_emplnone)))

#neet
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_neetnone <- glm(neet~onset, family = "binomial", data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_neetnone)
exp(cbind(OR = coef(model_neetnone), confint(model_neetnone)))

#housing stability cat.
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_housstabcontnone <- glm.nb(W26_housing_moved_num~onset, data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_housstabcontnone)

broom::tidy(model_housstabcontnone, exponentiate = TRUE, conf.int = TRUE)


###ADJUSTED, IMPUTED###
names(final_imp)

#debt/no debt
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_debtnone_adj <- glm(debt_nodebt~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_debtnone_adj)
exp(cbind(OR = coef(model_debtnone_adj), confint(model_debtnone_adj)))

#hs/no hs
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_hsnone_adj <- glm(educ_bin~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_hsnone_adj)
exp(cbind(OR = coef(model_hsnone_adj), confint(model_hsnone_adj)))

#hs/higher educ
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_highereducnone_adj <- glm(hs_highereduc~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_highereducnone_adj)
exp(cbind(OR = coef(model_highereducnone_adj), confint(model_highereducnone_adj)))

#unsanitary housing
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unsanitarynone_adj <- glm(W26_housing_cond~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_unsanitarynone_adj)
exp(cbind(OR = coef(model_unsanitarynone_adj), confint(model_unsanitarynone_adj)))

#housing instability
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_housnone_adj <- glm(stable_unstable~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_housnone_adj)
exp(cbind(OR = coef(model_housnone_adj), confint(model_housnone_adj)))

#unemployment
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_emplnone_adj <- glm(W26_empl_status~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_emplnone_adj)
exp(cbind(OR = coef(model_emplnone_adj), confint(model_emplnone_adj)))

#neet
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_neetnone_adj <- glm(neet~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep
                          + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(model_neetnone_adj)
exp(cbind(OR = coef(model_neetnone_adj), confint(model_neetnone_adj)))

#housing stability cat.
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_housstabcontnone <- glm.nb(W26_housing_moved_num~onset, + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep
                                 + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp) ####comparing late/early onset to no suicidal ideation
summary(model_housstabcontnone)

broom::tidy(model_housstabcontnone, exponentiate = TRUE, conf.int = TRUE)


##################################
#### EXP CORRELATION MATRICES ####
##################################
#### Gather vars ####
cor_vars <- analysis_sample[, c("educ_bin", "stable_unstable", "hs_highereduc", "debt_nodebt", "somedebt_lotdebt", "W26_empl_status", "W26_housing_cond")]


#### pairwise complete (no weight) - cor between each pair is computed ####
library(corrplot)
# produces similar results as below
cor_mat <- suppressWarnings(
  cor(cor_vars, use = "pairwise.complete.obs", method = "pearson")
)
corrplot(cor_mat, type = "upper", method = "color", tl.cex = 0.65, cl.cex = 0.65) # heat map
corrplot(cor_mat, type = "upper", method = "color", 
         number.cex = 0.4, tl.cex = 0.65, cl.cex = 0.65,
         addCoef.col = "black") # heat map w numbers



#check
check <- glm( ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att
                        + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp)
summary(check)

#check again
 exposure = c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
covariate = c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                 "W1_antisoc_mat_bin","W1_antisoc_pat_bin")

pairs <- expand.grid(exposure = exposure, covariate = covariate, stringsAsFactors = FALSE)

fitone_pair <- function(y,x,final_imp) {
  fml <- as.formula(paste(y, "~", x))
  fit <- glm(fml, data = final_imp, family = binomial("logit"))
  td <- broom:: tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  td %>% 
    dplyr:: filter(term!="(intercept)", grepl(paste0("^", x), term)) %>% 
          dplyr:: mutate(exposure = y, covariate = x, effect = "OR") %>% 
      dplyr:: select(exposure, covariate, term, effect, estimate, conf.low, conf.high, p.value)
}
out_tbl <- purrr:: pmap_dfr(pairs, ~ fitone_pair(..1,..2,final_imp))

print(out_tbl, n = I)

#check again again
outcome = c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
covariate = c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
              "W1_antisoc_mat_bin","W1_antisoc_pat_bin")

pairs <- expand.grid(outcome = outcome, covariate = covariate, stringsAsFactors = FALSE)

fitone_pair <- function(y,x,final_imp) {
  fml <- as.formula(paste(y, "~", x))
  fit <- glm(fml, data = final_imp, family = binomial("logit"))
  td <- broom:: tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  td %>% 
    dplyr:: filter(term!="(intercept)", grepl(paste0("^", x), term)) %>% 
    dplyr:: mutate(outcome = y, covariate = x, effect = "OR") %>% 
    dplyr:: select(outcome, covariate, term, effect, estimate, conf.low, conf.high, p.value)
}
out_tbl <- purrr:: pmap_dfr(pairs, ~ fitone_pair(..1,..2,final_imp))

print(out_tbl, n = Inf)

View(finalData)

##########################
#### SAMPLE WEIGHTING ####
##########################
##imputing full 2120
library(missForest)
finalData <- as.data.frame(finalData)
# convert vars from int to factor
finalData_imp <- finalData %>% dplyr::select(IDME, W1_sex, W1_ses, W1_fam_str, W1_dep_mat, W1_dep_pat, W1_race, W1_anx, W1_hyper_att, W1_opposition, W1_dep, W1_antisoc_mat_bin, W1_antisoc_pat_bin,
                                     W26_empl_status, W26_housing_moved, W26_housing_cond, neet, educ_bin, debt_nodebt, stable_unstable, hs_highereduc,
                                     idea_life, idea_srs_life, att_life, idea_recur_bin, onset)
finalData_imp <- finalData_imp %>% 
  mutate_at(vars(W1_sex, W1_ses, W1_fam_str, W1_dep_mat, W1_dep_pat, W1_race, W1_anx, W1_hyper_att, W1_opposition, W1_dep, W1_antisoc_mat_bin, W1_antisoc_pat_bin,
                 W26_empl_status, W26_housing_moved, W26_housing_cond, neet, educ_bin, debt_nodebt, stable_unstable, hs_highereduc,
                 idea_life, idea_srs_life, att_life, idea_recur_bin, onset), factor)
View(finalData_imp)
vars_imp <- c("W1_sex", "W1_ses", "W1_fam_str", "W1_dep_mat", "W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att",
              "W1_opposition", "W1_dep", "W1_antisoc_mat_bin", "W1_antisoc_pat_bin", "W26_empl_status", "W26_housing_moved",
              "W26_housing_cond", "neet", "educ_bin", "debt_nodebt", "stable_unstable", "hs_highereduc","idea_life","idea_srs_life", "att_life", "idea_recur_bin", "onset" )

# run missForest on selected vars
set.seed(123)
imp_result <- missForest(finalData_imp)
# extract imputed dataset
imp_all <- imp_result$ximp
# recombine with other vars
finalData_imp[ , vars_imp] <- imp_all[ , vars_imp]

#### save imputed table ####
write.csv(finalData_imp, "/mnt/40295D00/Travail/Commun/Ashley/Data/finalData_imp.csv", row.names=FALSE)
#Use to load and view new file
finalData_imp<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/finalData_imp.csv")
View(finalData_imp)

table(finalData$debt_nodebt, useNA = "ifany")
table(finalData_imp$debt_nodebt, useNA = "ifany")

#### Adding var for those in v. not in sample ####
analysis_sample <- read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/df.csv")
finalData_imp$kept <- ifelse(finalData_imp$IDME %in% analysis_sample$IDME, 1, 0) 

table(finalData_imp$kept, useNA = "always") #check it works

#### Creating IPCW model ####
#impute full 2120, derive the weights from imputed 2120, then apply included weights to my imputed data (sum of weight should be initial 2120)
ipcw_model <- glm(kept ~ W1_sex+W1_ses,
                  data = finalData_imp,
                  family = binomial,
                  na.action = na.exclude) #addresses NAs being dropped (fixes next line)
finalData_imp$prob_keep <- predict(ipcw_model, type = "response")
finalData_imp$IPCW <- 1 / finalData_imp$prob_keep

sum(finalData_imp$IPCW, na.rm = TRUE) #2945.731 

#### Adding it to analysis ####
final_imp <- merge(final_imp,
                   finalData_imp[, c("IDME", "IPCW")],
                   by = "IDME",
                   all.x = TRUE)
sum(final_imp$IPCW, na.rm = TRUE) #2119.462 (great as this matches initial sample size!!!)

#### save imputed table ####
write.csv(final_imp, "/mnt/40295D00/Travail/Commun/Ashley/Data/final_imp.csv", row.names=FALSE)
#Use to load and view new file
final_imp<-read.csv("/mnt/40295D00/Travail/Commun/Ashley/Data/final_imp.csv")
View(final_imp)


#### Rerun analyses with weights #### 
# included variables
expo <-  c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
outcomes <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
covariates <- c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                "W1_antisoc_mat_bin","W1_antisoc_pat_bin")
#### Rerun analyses with weights ####
model_list_IPCW <- list(
Model_A = "~ %EXPOSURE%",
  Model_B = paste(" ~ %EXPOSURE% +", paste(covariates, collapse = " + "))
)

results_IPCW <- data.frame(
  Outcome = character(),
  Exposure = character(), 
  Model = character(),
  OR_CI = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (o in outcomes){
  for (x in expo){ #GLM (looser assumptions)
    for (m in names(model_list_IPCW)){
      
      rhs <- sub("%EXPOSURE%", x, model_list_IPCW[[m]])
      form <- as.formula(paste0(o, " ", rhs))
      
      fit <- glm(
        formula = form,
        data = final_imp,
        family = binomial(link = "logit"),
        weights = IPCW,
        na.action = na.omit
      )
      
      sm <- summary(fit)$coefficients
      if (!x %in% rownames(sm)) {
        next
      }
      
      est <- sm[x, "Estimate"]
      se <- sm[x, "Std. Error"]
      p <- sm[x, "Pr(>|z|)"]
      
      OR <- exp(est) # odds ratio
      CI_lower <- exp(est - 1.96*se)
      CI_upper <- exp(est + 1.96*se)
      OR_CI <- sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
      
      results_IPCW <- rbind(results_IPCW, data.frame(
        Outcome = o,
        Exposure = x,
        Model = m,
        OR_CI = OR_CI,
        p_value = round(p, 3),
        stringsAsFactors = FALSE))
    }
  }
}

options(max.print = 10000)
print(results_IPCW)
View(results_IPCW)

library(openxlsx)
write.xlsx(results_IPCW, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_IPCW.xlsx")
View(final_imp)
# Onset: unadjusted, imputed, weights - continue w this for rest of outcomes, for adjusted just add your covariates (debt_nodebt + sex + sex...)
#debt/no debt
final_imp$debt_nodebt <- factor(final_imp$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_debtnone <- glm(debt_nodebt ~ onset, family = quasibinomial("logit"), 
                      data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_debtnone)
exp(cbind(OR = coef(model_debtnone), confint(model_debtnone)))

#hs/no hs (educ_bin)
final_imp$educ_bin <- factor(final_imp$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_educbin <- glm(educ_bin ~ onset, family = quasibinomial("logit"), 
                      data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_educbin)
exp(cbind(OR = coef(model_educbin), confint(model_educbin)))


#hs/ higher educ
final_imp$hs_highereduc <- factor(final_imp$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_highereduc <- glm(hs_highereduc ~ onset, family = quasibinomial("logit"), 
                     data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_highereduc)
exp(cbind(OR = coef(model_highereduc), confint(model_highereduc)))

#unemployment
final_imp$W26_empl_status <- factor(final_imp$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unempl <- glm(W26_empl_status ~ onset, family = quasibinomial("logit"), 
                        data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unempl)
exp(cbind(OR = coef(model_unempl), confint(model_unempl)))

#unsanitary housing
final_imp$W26_housing_cond <- factor(final_imp$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unsanitary <- glm(W26_housing_cond ~ onset, family = quasibinomial("logit"), 
                        data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unsanitary)
exp(cbind(OR = coef(model_unsanitary), confint(model_unsanitary)))

#unstable housing
final_imp$stable_unstable <- factor(final_imp$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unstablebin <- glm(stable_unstable ~ onset, family = quasibinomial("logit"), 
                         data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unstablebin)
exp(cbind(OR = coef(model_unstablebin), confint(model_unstablebin)))

#neet
final_imp$neet <- factor(final_imp$neet, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_neet <- glm(neet ~ onset, family = quasibinomial("logit"), 
                        data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_neet)
exp(cbind(OR = coef(model_neet), confint(model_neet)))


#Onset, ADJUSTED
#debt/no debt
final_imp$debt_nodebt <- factor(final_imp$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_debtnone <- glm(debt_nodebt ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                      data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_debtnone)
exp(cbind(OR = coef(model_debtnone), confint(model_debtnone)))

#hs/no hs (educ_bin)
final_imp$educ_bin <- factor(final_imp$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_educbin <- glm(educ_bin ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                        W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                      data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_educbin)
exp(cbind(OR = coef(model_educbin), confint(model_educbin)))

#hs/higher educ 
final_imp$hs_highereduc <- factor(final_imp$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_highereduc <- glm(hs_highereduc ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                       W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                     data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_highereduc)
exp(cbind(OR = coef(model_highereduc), confint(model_highereduc)))

#unempl
final_imp$W26_empl_status <- factor(final_imp$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unempl <- glm(W26_empl_status ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                          W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                        data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unempl)
exp(cbind(OR = coef(model_unempl), confint(model_unempl)))

#unsanitary hous
final_imp$W26_housing_cond <- factor(final_imp$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unsanitary <- glm(W26_housing_cond ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                    data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unsanitary)
exp(cbind(OR = coef(model_unsanitary), confint(model_unsanitary)))

#unstable hous (bin)
final_imp$stable_unstable <- factor(final_imp$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unstablebin <- glm(stable_unstable ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                          W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                        data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_unstablebin)
exp(cbind(OR = coef(model_unstablebin), confint(model_unstablebin)))

#neet
final_imp$neet <- factor(final_imp$neet, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_neet <- glm(neet ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                           W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = quasibinomial("logit"), 
                         data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation
summary(model_neet)
exp(cbind(OR = coef(model_neet), confint(model_neet)))



# Continuous housing: unadjusted, imputed, weights
library(MASS)
library(broom)
final_imp$W26_housing_moved_num <- as.numeric(as.character(final_imp$W26_housing_moved))

#att
theta <- glm.nb(W26_housing_moved_num ~ att_life, data = final_imp)$theta

housmoved_att <- glm(W26_housing_moved_num ~ att_life, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                     data = final_imp, weights = final_imp$IPCW)
summary(housmoved_att)

broom::tidy(housmoved_att, exponentiate = TRUE, conf.int = TRUE)

#srs ideation
housmoved_srsidea <- glm(W26_housing_moved_num ~ idea_srs_life, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                     data = final_imp, weights = final_imp$IPCW)
summary(housmoved_srsidea)

broom::tidy(housmoved_srsidea, exponentiate = TRUE, conf.int = TRUE)

#idea
housmoved_idea <- glm(W26_housing_moved_num ~ idea_life, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                         data = final_imp, weights = final_imp$IPCW)
summary(housmoved_idea)

broom::tidy(housmoved_idea, exponentiate = TRUE, conf.int = TRUE)

#recur idea
housmoved_recuridea <- glm(W26_housing_moved_num ~ idea_recur_bin, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                      data = final_imp, weights = final_imp$IPCW)
summary(housmoved_recuridea)

broom::tidy(housmoved_recuridea, exponentiate = TRUE, conf.int = TRUE)


#onset unstable housing, continuous
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unstablecont <- glm(W26_housing_moved_num ~ onset, family = negative.binomial(theta), 
                          data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation


summary(model_unstablecont)
broom::tidy(model_unstablecont, exponentiate = TRUE, conf.int = TRUE)



###Continous housing: Adjusted##
housmoved_att <- glm(W26_housing_moved_num ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                       W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                     data = final_imp, weights = final_imp$IPCW)
summary(housmoved_att)

broom::tidy(housmoved_att, exponentiate = TRUE, conf.int = TRUE)

#srs ideation
housmoved_srsidea <- glm(W26_housing_moved_num ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                           W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                     data = final_imp, weights = final_imp$IPCW)
summary(housmoved_srsidea)

broom::tidy(housmoved_srsidea, exponentiate = TRUE, conf.int = TRUE)

#idea
housmoved_idea <- glm(W26_housing_moved_num ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                        W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                         data = final_imp, weights = final_imp$IPCW)
summary(housmoved_idea)

broom::tidy(housmoved_idea, exponentiate = TRUE, conf.int = TRUE)

#recur idea
housmoved_recuridea <- glm(W26_housing_moved_num ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                             W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                      data = final_imp, weights = final_imp$IPCW)
summary(housmoved_recuridea)

broom::tidy(housmoved_recuridea, exponentiate = TRUE, conf.int = TRUE)

#onset
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_unstablecont <- glm(W26_housing_moved_num ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                            W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = negative.binomial(theta), 
                          data = final_imp, weights = final_imp$IPCW) ####comparing late/early onset to no suicidal ideation


summary(model_unstablecont)
broom::tidy(model_unstablecont, exponentiate = TRUE, conf.int = TRUE)


########################
##### MIGHT DELETE #####
########################

####Some debt/A lot of debt X suicide exposures ####

#Some debt/A lot of debt & att
somelotdebt_att <- glm(somedebt_lotdebt ~ att_life, family = "binomial", data = df)
summary(somelotdebt_att)

#Some debt/A lot of debt & srs sui idea
somelotdebt_srsidea <- glm(somedebt_lotdebt ~ idea_srs_life, family = "binomial", data = df)
summary(somelotdebt_srsidea)

#Some debt/A lot of debt & recur srs sui idea
somelotdebt_srsidearecur <- glm(somedebt_lotdebt ~ srs_idea_recur_bin, family = "binomial", data = df)
summary(somelotdebt_srsidearecur)

#Some debt/A lot of debt & sui idea
somelotdebt_idealife <- glm(somedebt_lotdebt ~ idea_life, family = "binomial", data = df)
summary(somelotdebt_idealife)

#Some debt/A lot of debt & idea recur
somelotdebt_idearecur <- glm(somedebt_lotdebt ~ idea_recur, family = "binomial", data = df)
summary(somelotdebt_idearecur)

#Some debt/A lot of debt & onset
somelot_onset <- glm(somedebt_lotdebt ~ onset, family = "binomial", data = df)
summary(somelot_onset)

####Ordinal logistic regression for debtcat####

#Debtcat & att
class(df)
str(df)
head(df)
library(MASS)
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_att <-polr(debtcat ~ att_life, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_att)

ci_prof <- confint(debtcat_att)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_att))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_att))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Debtcat & srs sui idea
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_srsidea <-polr(debtcat ~ idea_srs_life, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_srsidea)


ci_prof <- confint(debtcat_srsidea)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_srsidea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_srsidea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Debtcat & recur srs sui idea
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_recursrs <-polr(debtcat ~ srs_idea_recur_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_recursrs)

ci_prof <- confint(debtcat_recursrs)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_recursrs))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_recursrs))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Debtcat & sui idea
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_idea <-polr(debtcat ~ idea_life, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_idea)

ci_prof <- confint(debtcat_idea)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_idea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_idea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Debtcat & idea recur
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_recuridea <-polr(debtcat ~ idea_recur_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_recuridea)

ci_prof <- confint(debtcat_recuridea)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_recuridea))
OR_CI <- exp(ci_prof)

cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_recuridea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Debtcat & onset
df$debtcat <- factor(df$debtcat,
                     levels = c(0, 1, 2),
                     ordered = TRUE)
debtcat_onset <-polr(debtcat ~ onset, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_onset)

ci_prof <- confint(debtcat_onset)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_onset))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_onset))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

####ADJUSTED DEBTCAT####

##Adj debtcat & att
debtcat_att_adj <-polr(debtcat ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = analysis_sample, method = "logistic", Hess = TRUE)
summary(debtcat_idea_adj)

ci_prof <- confint(debtcat_att_adj)   ###to find OR and CI
scale
OR <- exp(coef(debtcat_att_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_att_adj))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Adj debtcat & srs sui idea
debtcat_srsidea_adj <-polr(debtcat ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_srsidea_adj)

ci_prof <- confint(debtcat_srsidea_adj) 
scale
OR <- exp(coef(debtcat_srsidea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_srsidea_adj)) 
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Adj debtcat & recur srs sui idea
debtcat_recursrsidea_adj <-polr(debtcat ~ srs_idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_recursrsidea_adj)

ci_prof <- confint(debtcat_recursrsidea_adj) 
scale
OR <- exp(coef(debtcat_recursrsidea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_recursrsidea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Adj debtcat& sui idea
debtcat_idea_adj <-polr(debtcat ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_idea_adj)

ci_prof <- confint(debtcat_idea_adj)
scale
OR <- exp(coef(debtcat_idea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_idea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj debtcat & idea recur
debtcat_recuridea_adj <-polr(debtcat ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_recuridea_adj)

ci_prof <- confint(debtcat_recuridea_adj)
scale
OR <- exp(coef(debtcat_recuridea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_recuridea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj debtcat & onset
debtcat_onset_adj <-polr(debtcat ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = df, method = "logistic", Hess = TRUE)
summary(debtcat_onset_adj)

ci_prof <- confint(debtcat_onset_adj)
scale
OR <- exp(coef(debtcat_onset_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(debtcat_onset_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

####EARLY VS LATE onset, unadjusted####
#unstable housing
df$onset_el <- relevel(df$onset_f, ref = "1")
model_late_early <- glm(stable_unstable~onset_el, family = "binomial", data = df)
summary(model_late_early)
exp(cbind(OR = coef(model_late_early), confint(model_late_early)))

#unemployment
df$onset_el <- relevel(df$onset_f, ref = "1")
model_le <- glm(W26_empl_status~onset_el, family = "binomial", data = df)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))



####EARLY VS LATE onset, adjusted####

#unstable housing
nrow(df)
df$onset_f<- as.factor(df$onset_f)
df$onset_el <- relevel(df$onset_f, ref = "1")
model_late_early_adj <- glm(stable_unstable~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = df)
summary(model_late_early_adj)
exp(cbind(OR = coef(model_late_early_adj), confint(model_late_early_adj)))

#unemployment
df$onset_el <- relevel(df$onset_f, ref = "1")
model_le_adj <- glm(W26_empl_status~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = df)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))





#####################################################
#### Housing stability (as category, using polr) ####
#####################################################

#Housing stab/housing moved & att
class(final_imp)
str(final_imp)
head(final_imp)
library(MASS)
final_imp$W26_housing_moved <- factor(final_imp$W26_housing_moved,
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                                      ordered = TRUE
)
housmoved_att <-polr(W26_housing_moved ~ att_life, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_att)


ci_prof <- confint(housmoved_att)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_att))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_att))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Hous_moved & srs sui idea
final_imp$W26_housing_moved <- factor(final_imp$W26_housing_moved,
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                                      ordered = TRUE)
housmoved_srsidea <-polr(W26_housing_moved ~ idea_srs_life, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_srsidea)


ci_prof <- confint(housmoved_srsidea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_srsidea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_srsidea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Hous moved & sui idea
final_imp$W26_housing_moved <- factor(final_imp$W26_housing_moved,
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                                      ordered = TRUE)
housmoved_idea <-polr(W26_housing_moved ~ idea_life, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_idea)

ci_prof <- confint(housmoved_idea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_idea))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_idea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Housmoved & idea recur
final_imp$W26_housing_moved <- factor(final_imp$W26_housing_moved,
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                                      ordered = TRUE)
housmoved_recuridea <-polr(W26_housing_moved ~ idea_recur_bin, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_recuridea)

ci_prof <- confint(housmoved_recuridea)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_recuridea))
OR_CI <- exp(ci_prof)

cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_recuridea))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


####Adj ordinal logistic regression for housing stability####

#sui att
housmoved_att_adj <-polr(W26_housing_moved ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att
                         + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_att_adj)

ci_prof <- confint(housmoved_att_adj)   ###to find OR and CI
scale
OR <- exp(coef(housmoved_att_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_att_adj))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#Adj housmoved & srs sui idea
housmoved_srsidea_adj <-polr(W26_housing_moved ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_srsidea_adj)

ci_prof <- confint(housmoved_srsidea_adj) 
scale
OR <- exp(coef(housmoved_srsidea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_srsidea_adj)) 
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj housmoved & sui idea
housmoved_idea_adj <-polr(W26_housing_moved ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_idea_adj)

ci_prof <- confint(housmoved_idea_adj)
scale
OR <- exp(coef(housmoved_idea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_idea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

#Adj housmoved & idea recur
housmoved_recuridea_adj <-polr(W26_housing_moved ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, data = final_imp, method = "logistic", Hess = TRUE)
summary(housmoved_recuridea_adj)

ci_prof <- confint(housmoved_recuridea_adj)
scale
OR <- exp(coef(housmoved_recuridea_adj))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(housmoved_recuridea_adj))  
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)











####ONSET - Housing stability with POLR####
#unadjusted housing stability cat.
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_housstabcontnone <- polr(W26_housing_moved~onset, method = "logistic", data = final_imp, Hess = TRUE) ####comparing late/early onset to no suicidal ideation
summary(model_housstabcontnone)
exp(cbind(OR = coef(model_housstabcontnone), confint(model_housstabcontnone)))


ci_prof <- confint(model_housstabcontnone)   ###to find OR and CI
scale
OR <- exp(coef(model_housstabcontnone))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(model_housstabcontnone))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)


#adjusted housing instability cat.
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
model_housstabcontnone <- polr(W26_housing_moved~onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att
                               + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, method = "logistic", data = final_imp, Hess = TRUE) ####comparing late/early onset to no suicidal ideation
summary(model_housstabcontnone)
exp(cbind(OR = coef(model_housstabcontnone), confint(model_housstabcontnone)))


ci_prof <- confint(model_housstabcontnone)   ###to find OR and CI
scale
OR <- exp(coef(model_housstabcontnone))
OR_CI <- exp(ci_prof)
cbind(OR = OR, OR_CI)

ct <- coef(summary(model_housstabcontnone))    ###To find P value
pvals <- 2*pnorm(abs(ct[,"t value"]), lower.tail = FALSE)
cbind(ct, "p value" = pvals)

