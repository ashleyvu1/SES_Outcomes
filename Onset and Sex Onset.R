
####EARLY VS LATE ONSET, UNADJUSTED ######
#unstable housing
final_imp_test <- final_imp
final_imp_test <- final_imp %>% 
  filter(onset %in% c(1, 2))

final_imp_test$onset<- as.factor(final_imp_test$onset)
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_late_early <- glm(stable_unstable~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_late_early)
exp(cbind(OR = coef(model_late_early), confint(model_late_early)))

#unemployment
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(W26_empl_status~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

#debt
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(debt_nodebt~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

#highschool diploma
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(educ_bin~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

#hs higher educ
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(hs_highereduc~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

#housing cond
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(W26_housing_cond~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

#neet
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le <- glm(neet~onset_el, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le)
exp(cbind(OR = coef(model_le), confint(model_le)))

###LOOP
earlylate_unadj <-function(model, outcome, analysis = "Unadjusted") {
  or_ci <- exp(cbind(
    OR = coef(model),
    confint(model)
  ))
  df <- as.data.frame(or_ci)
  df$p_value <-
    summary(model)$coefficients[,4]
  
  df$Variable <- rownames(df)
  df$Outcome <- outcome
  df$Analysis <- analysis
  
  rownames(df) <- NULL
  df
}

outcomes <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")

results_unadj <- lapply(outcomes, function(var) {
  formula <- as.formula(paste(var, "~onset_el"))
  
  model <- glm(formula, family = "binomial",
               data = final_imp_test, 
               weights = IPCW)
  earlylate_unadj(model, var, "Unadjusted")
  
})

##Make into excel
earlylate_unadj <- do.call(rbind, results_unadj)
rownames(earlylate_unadj) <- NULL
print(earlylate_unadj)
View(earlylate_unadj)

library(openxlsx)
write.xlsx(earlylate_unadj, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/earlylate_unadj.xlsx")



####EARLY VS LATE onset, adjusted####
#unstable housing
final_imp_test <- final_imp
final_imp_test <- final_imp %>% 
  filter(onset %in% c(1, 2))

final_imp_test$onset<- as.factor(final_imp_test$onset)
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_late_early_adj <- glm(stable_unstable~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_late_early_adj)
exp(cbind(OR = coef(model_late_early_adj), confint(model_late_early_adj)))

#unemployment
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(W26_empl_status~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))

#debt
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(debt_nodebt~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))

#highschool diploma
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(educ_bin~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))

#hs higher educ
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(hs_highereduc~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))

#housing cond
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(W26_housing_cond~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))

#neet
final_imp_test$onset_el <- relevel(final_imp_test$onset, ref = "1")
model_le_adj <- glm(neet~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, family = "binomial", data = final_imp_test, weights = IPCW)
summary(model_le_adj)
exp(cbind(OR = coef(model_le_adj), confint(model_le_adj)))


#LOOP, adjusted
earlylate_adj <-function(model, outcome, analysis = "Adjusted") {
  or_ci <- exp(cbind(
    OR = coef(model),
    confint(model)
  ))
  df <- as.data.frame(or_ci)
  df$p_value <-
    summary(model)$coefficients[,4]
  
  df$Variable <- rownames(df)
  df$Outcome <- outcome
  df$Analysis <- analysis
  
  rownames(df) <- NULL
  df
}

outcomes <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")

results_adj <- lapply(outcomes, function(var) {
  formula <- as.formula(paste(var, "~onset_el + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att + W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin"))
  
  model <- glm(formula, family = "binomial",
               data = final_imp_test, 
               weights = IPCW)
  earlylate_adj(model, var, "Adjusted")
  
})

##Make into excel
earlylate_adj <- do.call(rbind, results_adj)
rownames(earlylate_adj) <- NULL
print(earlylate_adj)
View(earlylate_adj)

library(openxlsx)
write.xlsx(earlylate_adj, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/earlylate_adj.xlsx")


######### SEX STRAT ##########
# Onset: unadjusted, imputed, weights - continue w this for rest of outcomes, for adjusted just add your covariates (debt_nodebt + sex + sex...)
#debt/no debt
final_imp_test$debt_nodebt <- factor(final_imp_test$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset_el, levels = c(1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "1")
results_sex_debtonset <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running sex =", s, ",n =", nrow(df_sub))
  
  
  model_debtnone <- glm(
    debt_nodebt ~ onset, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_debtnone))
  ci <- suppressMessages(confint(model_debtnone))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_debtonset[[length(results_sex_debtonset) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_debtonset <- do.call(rbind, results_sex_debtonset)
options(max.print = 10000)
print(results_sex_debtonset)
View(results_sex_debtonset)

#hs/no hs (educ_bin)
final_imp_test$educ_bin <- factor(final_imp_test$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_educbin <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("educ_bin: sex =", s, ", n =", nrow(df_sub))
  model_educbin <- glm(
    educ_bin ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_educbin))
  ci <- suppressMessages(confint(model_educbin))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_educbin[[length(results_sex_educbin) + 1]] <- data.frame(
    Outcome = "educ_bin",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_educbin <- do.call(rbind, results_sex_educbin)
options(max.print = 10000)
print(results_sex_educbin)
View(results_sex_educbin)


#hs/ higher educ
final_imp_test$hs_highereduc <- factor(final_imp_test$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_highereduc <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("highereduc: sex =", s, ", n =", nrow(df_sub))
  model_highereduc <- glm(
    hs_highereduc ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_highereduc))
  ci <- suppressMessages(confint(model_highereduc))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_highereduc[[length(results_sex_highereduc) + 1]] <- data.frame(
    Outcome = "highereduc",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_highereduc <- do.call(rbind, results_sex_highereduc)
options(max.print = 10000)
print(results_sex_highereduc)
View(results_sex_highereduc)


#unemployment
final_imp_test$W26_empl_status <- factor(final_imp_test$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_unempl <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("W26_empl_status: sex =", s, ", n =", nrow(df_sub))
  model_unempl <- glm(
    W26_empl_status ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_unempl))
  ci <- suppressMessages(confint(model_unempl))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_unempl[[length(results_sex_unempl) + 1]] <- data.frame(
    Outcome = "W26_empl_status",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_unempl <- do.call(rbind, results_sex_unempl)
options(max.print = 10000)
print(results_sex_unempl)
View(results_sex_unempl)


#unsanitary housing
final_imp_test$W26_housing_cond <- factor(final_imp_test$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_housingcond <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("W26_housing_cond: sex =", s, ", n =", nrow(df_sub))
  model_housingcond <- glm(
    W26_housing_cond ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_housingcond))
  ci <- suppressMessages(confint(model_housingcond))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_housingcond[[length(results_sex_housingcond) + 1]] <- data.frame(
    Outcome = "W26_housing_cond",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_housingcond <- do.call(rbind, results_sex_housingcond)
options(max.print = 10000)
print(results_sex_housingcond)
View(results_sex_housingcond)


#unstable housing
final_imp_test$stable_unstable <- factor(final_imp_test$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_housingstab <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("stable_unstable: sex =", s, ", n =", nrow(df_sub))
  model_housingstab <- glm(
    stable_unstable ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_housingstab))
  ci <- suppressMessages(confint(model_housingstab))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_housingstab[[length(results_sex_housingstab) + 1]] <- data.frame(
    Outcome = "stable_unstable",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_housingstab <- do.call(rbind, results_sex_housingstab)
options(max.print = 10000)
print(results_sex_housingstab)
View(results_sex_housingstab)

#neet
final_imp_test$neet <- factor(final_imp_test$neet, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(0, 1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "0")
results_sex_neet <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message("neet: sex =", s, ", n =", nrow(df_sub))
  model_neet <- glm(
    neet ~ onset,
    family = quasibinomial("logit"),
    data = df_sub,
    weights = IPCW
  )
  est <- coef(summary(model_neet))
  ci <- suppressMessages(confint(model_neet))
  
  OR <- exp(est[, "Estimate"])
  CI_1 <- exp(ci[, 1])
  CI_u <- exp(ci[, 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_neet[[length(results_sex_neet) + 1]] <- data.frame(
    Outcome = "neet",
    Sex = ifelse(s == 1, "Male", "Female"),
    Term = rownames(est),
    OR_CI = sprintf("%.2f(%.2f-%.2f)", OR, CI_1, CI_u),
    p_value = round(pval, 3), 
    stringAsFactors = FALSE
  )
}

results_sex_neet <- do.call(rbind, results_sex_neet)
options(max.print = 10000)
print(results_sex_neet)
View(results_sex_neet)


###combining
library(dplyr)
sex_earlylate_unadj <- bind_rows(
  results_sex_debtonset,
  results_sex_educbin,
  results_sex_highereduc,
  results_sex_housingcond,
  results_sex_housingstab,
  results_sex_neet,
  results_sex_unempl
)
print(sex_earlylate_unadj)
View(sex_earlylate_unadj)

library(openxlsx)
write.xlsx(sex_earlylate_unadj, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/sex_earlylate_unadj.xlsx")



### Onset, ADJUSTED ###
#debt/no debt
final_imp_test$debt_nodebt <- factor(final_imp_test$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_debtadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_debtadj <- glm(
    debt_nodebt ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_debtadj))
  ci <- suppressMessages(confint(model_debtadj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_debtadj[[length(results_sex_debtadj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_debtadj <- do.call(rbind, results_sex_debtadj)
options(max.print = 10000)
print(results_sex_debtadj)
View(results_sex_debtadj)



#hs/no hs (educ_bin)
final_imp_test$educ_bin <- factor(final_imp_test$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_educbinadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_educbinadj <- glm(
    educ_bin ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_educbinadj))
  ci <- suppressMessages(confint(model_educbinadj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_educbinadj[[length(results_sex_educbinadj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_educbinadj <- do.call(rbind, results_sex_educbinadj)
options(max.print = 10000)
print(results_sex_educbinadj)
View(results_sex_educbinadj)



#hs/higher educ 
final_imp_test$hs_highereduc <- factor(final_imp_test$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_highereducadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_highereducadj <- glm(
    hs_highereduc ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_highereducadj))
  ci <- suppressMessages(confint(model_highereducadj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_highereducadj[[length(results_sex_highereducadj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_highereducadj <- do.call(rbind, results_sex_highereducadj)
options(max.print = 10000)
print(results_sex_highereducadj)
View(results_sex_highereducadj)


#unempl
final_imp_test$W26_empl_status <- factor(final_imp_test$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_unempladj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_unempladj <- glm(
    W26_empl_status ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_unempladj))
  ci <- suppressMessages(confint(model_unempladj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_unempladj[[length(results_sex_unempladj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_unempladj <- do.call(rbind, results_sex_unempladj)
options(max.print = 10000)
print(results_sex_unempladj)
View(results_sex_unempladj)


#unsanitary hous
final_imp_test$W26_housing_cond <- factor(final_imp_test$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_houscondadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_houscondadj <- glm(
    W26_housing_cond ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_houscondadj))
  ci <- suppressMessages(confint(model_houscondadj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_houscondadj[[length(results_sex_houscondadj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_houscondadj <- do.call(rbind, results_sex_houscondadj)
options(max.print = 10000)
print(results_sex_houscondadj)
View(results_sex_houscondadj)


#unstable hous (bin)
final_imp_test$stable_unstable <- factor(final_imp_test$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_housstab_adj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_housstab_adj <- glm(
    stable_unstable ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_housstab_adj))
  ci <- suppressMessages(confint(model_housstab_adj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_housstab_adj[[length(results_sex_housstab_adj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_housstab_adj <- do.call(rbind, results_sex_housstab_adj)
options(max.print = 10000)
print(results_sex_housstab_adj)
View(results_sex_housstab_adj)

#neet
final_imp_test$neet <- factor(final_imp_test$neet, levels = c(0, 1)) # do for each outcome
final_imp_test$onset <- factor(final_imp_test$onset, levels = c(1, 2))
final_imp_test$onset <- relevel(final_imp_test$onset, ref = "1")
results_sex_neet_adj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp_test, W1_sex == s)
  message ("Running =", s, ",n =", nrow(df_sub))
  
  
  model_neet_adj <- glm(
    neet ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
      W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
    family = quasibinomial("logit"), 
    data = df_sub, 
    weights = IPCW ####comparing late/early onset to no suicidal ideation
  )
  
  est <- coef(summary(model_neet_adj))
  ci <- suppressMessages(confint(model_neet_adj))
  
  OR <- exp(est[,"Estimate"])
  CI_1 <- exp(ci[rownames(est), 1])
  CI_u <- exp(ci[rownames(est), 2])
  pval <- est[, "Pr(>|t|)"]
  
  results_sex_neet_adj[[length(results_sex_neet_adj) + 1]] <-
    data.frame(
      Sex = ifelse(s == 1, "Male", "Female"),
      Term = rownames(est),
      OR_CI = sprintf("%.2f(%.2f-%.2f", OR, CI_1, CI_u),
      p_value = round(pval, 3),
      stringAsFactors = FALSE
    )
}
results_sex_neet_adj <- do.call(rbind, results_sex_neet_adj)
options(max.print = 10000)
print(results_sex_neet_adj)
View(results_sex_neet_adj)


#combining
###combining
library(dplyr)
sex_earlylate_adj <- bind_rows(
  results_sex_debtadj,
  results_sex_educbinadj,
  results_sex_highereducadj,
  results_sex_houscondadj,
  results_sex_housstab_adj,
  results_sex_neet_adj,
  results_sex_unempladj
)

print(sex_earlylate_adj)
View(sex_earlylate_adj)

library(openxlsx)
write.xlsx(sex_earlylate_adj, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/sex_earlylate_adj.xlsx")