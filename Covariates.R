#Sex of the child
SOCIO101 <- read_sav ("/mnt/40295D00/Travail/Commun/Zoey/Data/Covariates/SOCIO101.sav")
View(SOCIO101) 

W1varsSOCIO <- c("idme","asexe_2") 
W1_SEX <- SOCIO101[,W1varsSOCIO]
View(W1_SEX)

# Recode sex to be binary (0 = female, 1 = male)
W1_SEX$asexe_2[W1_SEX$asexe_2 == "F"]<-0
W1_SEX$asexe_2[W1_SEX$asexe_2 == "M"]<-1

#SES
INDI101 <- read_sav ("/mnt/40295D00/Travail/Commun/Zoey/Data/Covariates/INDI101.sav")
View(INDI101) 

W1varsINDI <- c("idme","AINFD08",#SES
                "afafd02") #family structure 

W1_COVARS <- INDI101[,W1varsINDI]
View(W1_COVARS)


# merging
dim(W1_SEX)
dim(W1_COVARS)
covariateData <- merge(W1_SEX,W1_COVARS, by="idme",all=TRUE)
dim(covariateData)
colnames(covariateData)[1] = "IDME"



