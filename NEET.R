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

table(neet$neet, useNA = "ifany") ##88 neet



neet[1:20,c("neet_educ", "ZTVNQ01", "neet")] 


subset(neet, is.na(neet_educ), select = c("neet", "neet_educ", "ZTVNQ01")) # filtering out the NA's for NEET to see what the pattern is (are they true NAs)


subset(neet, ZTVNQ02A_M13 == 13, select = c("ZTVNQ02A_M13", "neet", "neet_educ", "ZTVNQ01")) # filtering out the NA's for NEET to see what the pattern is (are they true NAs)
table(neet$ZTVNQ02A_M13, useNA = "ifany") ##28 on parental leave

table(neet$neet, useNA = "ifany")
## neet = 88