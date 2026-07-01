###################################
#### LIFETIME REGRESION MODELS ####
###################################
p_values <- c(0.01, 0.04, 0.03, 0.25, 0.07)
p_adjusted <- p.adjust(p_values, method = "bonferroni")
print(p_adjusted)
#### UNADJUSTED INDIVIDUAL MODELS####
#### Lifetime PCs to lifetime suicide ####
# Creating loop function to view results for each individual regression against lifetime outcomes 
# Defining variables 
exposures_life <- c("att_life", "idea_srs_life", "idea_life","idea_recur")
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
write.xlsx(results_life_unadj_1540, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_life_unadj_1540.xlsx")





#####################
####### EXTRA #######
#####################

######################################
####NON-IMPUTED Binary regressions####
######################################

###Education binary X suicide exposures###

glm(dependent_variable~independent_variable, family = "binomial", data)

#Educ bin & att
educbin_att <- glm(educ_bin ~ att_life, family = "binomial", data = analysis_sample)
summary(educbin_att)


est <- coef(summary(educbin_att))
OR <- exp(est[,"Estimate"])
CI <- exp(confint(educbin_att))
p <- est[,"Pr(>|z|)"]


#Educ bin & srs sui idea
educbin_srsidea <- glm(educ_bin ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(educbin_srsidea)

#Educ bin & recur srs sui idea 
educbin_srsidearecur <- glm(educ_bin ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(educbin_srsidearecur)

#Educ bin & sui idea
educbin_idealife <- glm(educ_bin ~ idea_life, family = "binomial", data = analysis_sample)
summary(educbin_idealife)

#Educ bin & idea recur
educbin_idearecur <- glm(educ_bin ~ idea_recur, family = "binomial", data = analysis_sample)
summary(educbin_idearecur)

#Educ bin & onset
educbin_onset <- glm(educ_bin ~ onset, family = "binomial", data = analysis_sample)
summary(educbin_onset)


###hs_highereduc X suicide exposures###

#hs_highereduc & att
highereduc_att <- glm(hs_highereduc ~ att_life, family = "binomial", data = analysis_sample)
summary(highereduc_att)

#hs_highereduc & srs sui idea
highereduc_srsidea <- glm(hs_highereduc ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(highereduc_srsidea)

#hs_highereduc & recur srs sui idea
highereduc_srsidearecur <- glm(hs_highereduc ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(highereduc_srsidearecur)

#hs_highereduc & sui idea
highereduc_idealife <- glm(hs_highereduc ~ idea_life, family = "binomial", data = analysis_sample)
summary(highereduc_idealife)

#hs_highereduc & idea recur
highereduc_idearecur <- glm(hs_highereduc ~ idea_recur, family = "binomial", data = analysis_sample)
summary(highereduc_idearecur)

#hs_highereduc & onset
highereduc_onset <- glm(hs_highereduc ~ onset, family = "binomial", data = analysis_sample)
summary(highereduc_onset)


###Employ status X suicide exposures ###

#W26_empl & att
empl_att <- glm(W26_empl_status ~ att_life, family = "binomial", data = analysis_sample)
summary(empl_att)

#W26_empl & srs sui idea
empl_srsidea <- glm(W26_empl_status ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(empl_srsidea)

#W26_empl & recur srs sui idea
empl_srsidearecur <- glm(W26_empl_status ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(empl_srsidearecur)

#W26_empl & sui idea
empl_idealife <- glm(W26_empl_status ~ idea_life, family = "binomial", data = analysis_sample)
summary(empl_idealife)

#W26_empl & idea recur
empl_idearecur <- glm(W26_empl_status ~ idea_recur, family = "binomial", data = analysis_sample)
summary(empl_idearecur)

#W26_empl & onset
empl_onset <- glm(W26_empl_status ~ onset, family = "binomial", data = analysis_sample)
summary(empl_onset)


###Housing conditions X suicide exposures

#Housing cond & att
houscond_att <- glm(W26_housing_cond ~ att_life, family = "binomial", data = analysis_sample)
summary(houscond_att)

#Housing cond & srs sui idea
houscond_srsidea <- glm(W26_housing_cond ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(houscond_srsidea)

#Housing cond & recur srs sui idea
houscond_srsidearecur <- glm(W26_housing_cond ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(houscond_srsidearecur)

#Housing cond & sui idea
houscond_idealife <- glm(W26_housing_cond ~ idea_life, family = "binomial", data = analysis_sample)
summary(houscond_idealife)

#Housing cond & idea recur
houscond_idearecur <- glm(W26_housing_cond ~ idea_recur, family = "binomial", data = analysis_sample)
summary(houscond_idearecur)

#Housing cond & onset
houscond_onset <- glm(W26_housing_cond ~ onset, family = "binomial", data = analysis_sample)
summary(houscond_onset)


###Housing stability X suicide exposures ###

#Housing stability & att
housstab_att <- glm(stable_unstable ~ att_life, family = "binomial", data = analysis_sample)
summary(housstab_att)

#Housing stability & srs sui idea
housstab_srsidea <- glm(stable_unstable ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(housstab_srsidea)

#Housing stability & recur srs sui idea
housstab_srsidearecur <- glm(stable_unstable ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(housstab_srsidearecur)

#Housing stability & sui idea
housstab_idealife <- glm(stable_unstable ~ idea_life, family = "binomial", data = final_imp)
summary(housstab_idealife)

est <- coef(summary(housstab_idealife))
OR <- exp(est[,"Estimate"])
CI <- exp(confint(housstab_idealife))
p <- est[,"Pr(>|z|)"]

#Housing stability & idea recur
housstab_idearecur <- glm(stable_unstable ~ idea_recur, family = "binomial", data = analysis_sample)
summary(housstab_idearecur)

#Housing stability & onset
housstab_onset <- glm(stable_unstable ~ onset, family = "binomial", data = analysis_sample)
summary(housstab_onset)


###Debt/No debt X suicide exposures ###

#Debt/no debt & att
debtno_att <- glm(debt_nodebt ~ att_life, family = "binomial", data = analysis_sample)
summary(debtno_att)

#Debt/no debt & srs sui idea
debtno_srsidea <- glm(debt_nodebt ~ idea_srs_life, family = "binomial", data = analysis_sample)
summary(debtno_srsidea)

#Debt/no debt& recur srs sui idea
debtno_srsidearecur <- glm(debt_nodebt ~ srs_idea_recur_bin, family = "binomial", data = analysis_sample)
summary(debtno_srsidearecur)

#Debt/no debt & sui idea
debtno_idealife <- glm(debt_nodebt ~ idea_life, family = "binomial", data = analysis_sample)
summary(debtno_idealife)

#Debt/no debt & idea recur
debtno_idearecur <- glm(debt_nodebt ~ idea_recur, family = "binomial", data = analysis_sample)
summary(debtno_idearecur)

#Debt/no debt & onset
debtno_onset <- glm(debt_nodebt ~ onset, family = "binomial", data = analysis_sample)
summary(debtno_onset)



