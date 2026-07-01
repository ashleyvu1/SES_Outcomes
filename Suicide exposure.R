library(haven)

##CREATING SUICIDE EXPOSURE TABLES (13-20 y/o) (W14 -W20)##


####WAVE 14 (13y/o)####
QIE1401 <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Outcome/QIE1401.sav")

View(QIE1401) 

var <- c("IDME","NPINQ3","NPINQ1","NPINQ2")
W14_SUI <- QIE1401[var]

#Renaming suicide variable - Wave 14
names(W14_SUI)<-c("IDME","W14_att","W14_SUI_idea","W14_srs_SUI_idea")   

###WAVE 16 (15 y/o)###
qelj1601f <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Outcome/qelj1601f.sav")

View(qelj1601f)
var <- c("IDME", "PPINQ3","PPINQ1","PPINQ2")
W16_SUI <- qelj1601f[var]

names(W16_SUI)<-c("IDME","W16_att","W16_SUI_idea","W16_srs_SUI_idea")


#WAVE 18 (17 y/o)
qelj1801f <- read_sav ("/mnt/40295D00/Travail/Commun/Ashley/Outcome/qelj1801f.sav")

#Suicide attempts
View(qelj1801f)
var <- c("IDME", "RPINQ3","RPINQ1","RPINQ2")
W18_SUI <- qelj1801f[var]

names(W18_SUI)<-c("IDME", "W18_att","W18_SUI_idea","W18_srs_SUI_idea")


# merging
dim(W14_SUI)
dim(W16_SUI)
dim(W18_SUI)

exposureData <- merge(W14_SUI,W16_SUI, by="IDME",all=TRUE)
dim(exposureData)
head(exposureData)
exposureData <- merge(exposureData,W18_SUI, by="IDME",all=TRUE)
dim(exposureData)
head(exposureData)




