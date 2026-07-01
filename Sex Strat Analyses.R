#SEX STRAT

#### Rerun analyses with weights #### 
# included variables
expo <-  c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")
outcomes <- c("educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")
covariates <- c("W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx", "W1_hyper_att", "W1_opposition", "W1_dep",
                "W1_antisoc_mat_bin","W1_antisoc_pat_bin")
#### Rerun analyses with weights ####
model_list_sex <- list(
  Model_A = "~ %EXPOSURE%",
  Model_B = paste(" ~ %EXPOSURE% +", paste(covariates, collapse = " + "))
)

results_sex <- data.frame(
  Outcome = character(),
  Exposure = character(), 
  Model = character(),
  OR_CI = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_list <- list()
for (o in outcomes){
  for (x in expo){ #GLM (looser assumptions)
    for (m in names(model_list_sex)){
      for(s in c(0, 1)){
        df_sub <- subset(final_imp, W1_sex == s)
        
        
        rhs <- sub("%EXPOSURE%", x, model_list_sex[[m]])
        form <- as.formula(paste0(o, " ", rhs))
        
        fit <- glm(
          formula = form,
          data = df_sub,
          family = binomial(link = "logit"),
          weights = IPCW,
          na.action = na.omit
        )
        
        sm <- summary(fit)$coefficients
        if (!x %in% rownames(sm)) {
          results_list[[length(results_list) +1]] <- data.frame(
            Outcome = o,
            Exposure = x,
            Model = m,
            Sex = ifelse(s == 1, "Male", "Female"),
            OR_CI = OR_CI,
            p_value = round(p, 3),
            stringsAsFactors = FALSE)
          
          message("Dropped: sex =", s,
                  ", n =", nrow(df_sub))
          next
        }
        message("Running sex =", s,
                ",n =", nrow(df_sub))
        
        est <- sm[x, "Estimate"]
        se <- sm[x, "Std. Error"]
        p <- sm[x, "Pr(>|z|)"]
        
        OR <- exp(est) # odds ratio
        CI_lower <- exp(est - 1.96*se)
        CI_upper <- exp(est + 1.96*se)
        OR_CI <- sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
        
        results_list[[length(results_list) +1]] <- data.frame(
          Outcome = o,
          Exposure = x,
          Model = m,
          Sex = ifelse(s == 1, "Male", "Female"),
          OR_CI = OR_CI,
          p_value = round(p, 3),
          stringsAsFactors = FALSE)
      }
    }
  }
}

results_sex <- do.call(rbind, results_list)
options(max.print = 10000)
print(results_sex)
View(results_sex)

library(openxlsx)
write.xlsx(results_sex, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/results_sex.xlsx")
View(final_imp)

# Onset: unadjusted, imputed, weights - continue w this for rest of outcomes, for adjusted just add your covariates (debt_nodebt + sex + sex...)
#debt/no debt
final_imp$debt_nodebt <- factor(final_imp$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_debtonset <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$educ_bin <- factor(final_imp$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_educbin <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$hs_highereduc <- factor(final_imp$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_highereduc <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$W26_empl_status <- factor(final_imp$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_unempl <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$W26_housing_cond <- factor(final_imp$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_housingcond <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$stable_unstable <- factor(final_imp$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_housingstab <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$neet <- factor(final_imp$neet, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_neet <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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

#Onset, ADJUSTED
#debt/no debt
final_imp$debt_nodebt <- factor(final_imp$debt_nodebt, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_debtadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$educ_bin <- factor(final_imp$educ_bin, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_educbinadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$hs_highereduc <- factor(final_imp$hs_highereduc, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_highereducadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$W26_empl_status <- factor(final_imp$W26_empl_status, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_unempladj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$W26_housing_cond <- factor(final_imp$W26_housing_cond, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_houscondadj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$stable_unstable <- factor(final_imp$stable_unstable, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_housstab_adj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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
final_imp$neet <- factor(final_imp$neet, levels = c(0, 1)) # do for each outcome
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")
results_sex_neet_adj <- list()

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex == s)
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


# Continuous housing: unadjusted, imputed, weights
library(MASS)
library(broom)
final_imp$W26_housing_moved_num <- as.numeric(as.character(final_imp$W26_housing_moved))

#att
results_sex_housmov_att <- list()
theta <- glm.nb(W26_housing_moved_num ~ att_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_att <- glm(W26_housing_moved_num ~ att_life, 
                       family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                       data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_att, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_att[[length(results_sex_housmov_att) + 1]] <- tmp
}

results_sex_housmov_att <- do.call(rbind, results_sex_housmov_att)
options(max.print = 10000)
print(results_sex_housmov_att)
View(results_sex_housmov_att)


#srs ideation
results_sex_housmov_srsidea <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_srs_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_srsidea <- glm(W26_housing_moved_num ~ idea_srs_life, 
                           family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                           data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_srsidea, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_srsidea[[length(results_sex_housmov_srsidea) + 1]] <- tmp
}

results_sex_housmov_srsidea <- do.call(rbind, results_sex_housmov_srsidea)
options(max.print = 10000)
print(results_sex_housmov_srsidea)
View(results_sex_housmov_srsidea)



#idea
results_sex_housmov_idea <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_idea <- glm(W26_housing_moved_num ~ idea_life, 
                        family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                        data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_idea, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_idea[[length(results_sex_housmov_idea) + 1]] <- tmp
}

results_sex_housmov_idea <- do.call(rbind, results_sex_housmov_idea)
options(max.print = 10000)
print(results_sex_housmov_idea)
View(results_sex_housmov_idea)


#recur idea
results_sex_housmov_recuridea <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_recur_bin, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_recuridea <- glm(W26_housing_moved_num ~ idea_recur_bin, 
                             family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                             data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_recuridea, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_recuridea[[length(results_sex_housmov_recuridea) + 1]] <- tmp
}

results_sex_housmov_recuridea <- do.call(rbind, results_sex_housmov_recuridea)
options(max.print = 10000)
print(results_sex_housmov_recuridea)
View(results_sex_housmov_recuridea)


#onset unstable housing, continuous
results_sex_housmov_onset <- list()
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")

theta <- glm.nb(W26_housing_moved_num ~ onset, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmov_sex_onset <- glm(W26_housing_moved_num ~ onset, 
                           family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                           data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmov_sex_onset, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_onset[[length(results_sex_housmov_onset) + 1]] <- tmp
}

results_sex_housmov_onset <- do.call(rbind, results_sex_housmov_onset)
options(max.print = 10000)
print(results_sex_housmov_onset)
View(results_sex_housmov_onset)


###Continous housing: Adjusted###

library(MASS)
library(broom)
final_imp$W26_housing_moved_num <- as.numeric(as.character(final_imp$W26_housing_moved))


#Att
results_sex_housmove_attadj <- list()
theta <- glm.nb(W26_housing_moved_num ~att_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_att_adj <- glm(W26_housing_moved_num ~ att_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                             W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
                           family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                           data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_att_adj, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmove_attadj[[length(results_sex_housmove_attadj) + 1]] <- tmp
}
results_sex_housmove_attadj <- do.call(rbind, results_sex_housmove_attadj)
options(max.print = 10000)
print(results_sex_housmove_attadj)
View(results_sex_housmove_attadj)


#srs ideation
results_sex_housmov_srsideaadj <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_srs_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_srsidea_adj <- glm(W26_housing_moved_num ~ idea_srs_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                                 W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin,
                               family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                               data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_srsidea_adj, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_srsideaadj[[length(results_sex_housmov_srsideaadj) + 1]] <- tmp
}

results_sex_housmov_srsideaadj <- do.call(rbind, results_sex_housmov_srsideaadj)
options(max.print = 10000)
print(results_sex_housmov_srsideaadj)
View(results_sex_housmov_srsideaadj)


#idea
results_sex_housmov_idea_adj <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_life, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_idea_adj <- glm(W26_housing_moved_num ~ idea_life + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                              W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
                            family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                            data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_idea_adj, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_idea_adj[[length(results_sex_housmov_idea_adj) + 1]] <- tmp
}

results_sex_housmov_idea_adj <- do.call(rbind, results_sex_housmov_idea_adj)
options(max.print = 10000)
print(results_sex_housmov_idea_adj)
View(results_sex_housmov_idea_adj)


#recur idea
results_sex_housmov_recuridea_adj <- list()
theta <- glm.nb(W26_housing_moved_num ~ idea_recur_bin, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmoved_recuridea_adj <- glm(W26_housing_moved_num ~ idea_recur_bin + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                                   W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin, 
                                 family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                                 data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmoved_recuridea_adj, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_recuridea_adj[[length(results_sex_housmov_recuridea_adj) + 1]] <- tmp
}

results_sex_housmov_recuridea_adj <- do.call(rbind, results_sex_housmov_recuridea_adj)
options(max.print = 10000)
print(results_sex_housmov_recuridea_adj)
View(results_sex_housmov_recuridea_adj)


#onset
results_sex_housmov_onsetadj <- list()
final_imp$onset <- factor(final_imp$onset, levels = c(0, 1, 2))
final_imp$onset <- relevel(final_imp$onset, ref = "0")

theta <- glm.nb(W26_housing_moved_num ~ onset, data = final_imp)$theta

for(s in c(0, 1)) {
  df_sub <- subset(final_imp, W1_sex ==s)
  message("Running sex =", s, ", n =", nrow(df_sub))
  
  housmov_sex_onsetadj <- glm(W26_housing_moved_num ~ onset + W1_sex + W1_ses + W1_fam_str + W1_dep_mat + W1_dep_pat + W1_race + W1_anx + W1_hyper_att +
                                W1_opposition + W1_dep + W1_antisoc_mat_bin + W1_antisoc_pat_bin,
                              family = negative.binomial(theta), #copy here down + complete for the rest of the exposures; for adj just add covars in
                              data = df_sub, weights = IPCW)
  
  tmp <- broom::tidy(housmov_sex_onsetadj, exponentiate = TRUE, conf.int = TRUE)
  tmp$Sex <- ifelse(s == 1, "Male", "Female")
  
  results_sex_housmov_onsetadj[[length(results_sex_housmov_onsetadj) + 1]] <- tmp
}

results_sex_housmov_onsetadj <- do.call(rbind, results_sex_housmov_onsetadj)
options(max.print = 10000)
print(results_sex_housmov_onsetadj)
View(results_sex_housmov_onsetadj)


### Prevalence ###

#### Outcomes x Sex ####
##filter = 0 (educ_bin & hs_higher educ; 0 = no hs, 0 = only hs diploma/no higher educ)
outcome_vars <- c("educ_bin", "hs_highereduc")

library(dplyr)
library(tidyr)
library(purrr)

results_sex <- map_dfr(outcome_vars, function(var){
  df <- analysis_sample %>% 
    filter(.data[[var]] == 0)
  
  total_n <- nrow(df)
  
  sex_counts <- df %>% 
    group_by(W1_sex) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(percent = round(100 * n / sum(n), 1),
           formatted = paste0(n, " (", percent, "%)"))
  
  out <- sex_counts %>% 
    mutate(W1_sex = ifelse(W1_sex == 0, "Female", "Male")) %>% 
    dplyr::select(W1_sex, formatted) %>% 
    pivot_wider(
      names_from = W1_sex,
      values_from = formatted,
      values_fill = "0 (0%)")
  
  sex_table <- table(df$W1_sex)
  p_val <- if (min(sex_table)< 5){
    fisher.test(sex_table)$p.value
  } else{
    chisq.test(sex_table, correct = FALSE)$p.value
  }
  out$Condition <- var
  out$Total_N <- total_n
  out$p_value <- round(p_val, 3)
  out
})

results_sex <- results_sex %>% 
  dplyr::select(Condition, Female, Male, Total_N, p_value)

print(results_sex, n = Inf)

table(final_imp$neet)

###filter = 1 (empl, housing cond, housing stability, debt_nodebt, neet)
outcome_vars <- c("W26_empl_status", "W26_housing_cond", "stable_unstable", "debt_nodebt", "neet")

library(dplyr)
library(tidyr)
library(purrr)

results_sex <- map_dfr(outcome_vars, function(var){
  df <- analysis_sample %>% 
    filter(.data[[var]] == 1)
  
  total_n <- nrow(df)
  
  sex_counts <- df %>% 
    group_by(W1_sex) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(percent = round(100 * n / sum(n), 1),
           formatted = paste0(n, " (", percent, "%)"))
  
  out <- sex_counts %>% 
    mutate(W1_sex = ifelse(W1_sex == 0, "Female", "Male")) %>% 
    dplyr::select(W1_sex, formatted) %>% 
    pivot_wider(
      names_from = W1_sex,
      values_from = formatted,
      values_fill = "0 (0%)")
  
  sex_table <- table(df$W1_sex)
  p_val <- if (min(sex_table)< 5){
    fisher.test(sex_table)$p.value
  } else{
    chisq.test(sex_table, correct = FALSE)$p.value
  }
  out$Condition <- var
  out$Total_N <- total_n
  out$p_value <- round(p_val, 3)
  out
})

results_sex <- results_sex %>% 
  dplyr::select(Condition, Female, Male, Total_N, p_value)

print(results_sex, n = Inf)

table(final_imp$neet)

##Exposure vars
expos_vars <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin")

library(dplyr)
library(tidyr)
library(purrr)

results_sex <- map_dfr(expos_vars, function(var){
  df <- analysis_sample %>% 
    filter(.data[[var]] == 1)
  
  total_n <- nrow(df)
  
  sex_counts <- df %>% 
    group_by(W1_sex) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(percent = round(100 * n / sum(n), 1),
           formatted = paste0(n, " (", percent, "%)"))
  
  out <- sex_counts %>% 
    mutate(W1_sex = ifelse(W1_sex == 0, "Female", "Male")) %>% 
    dplyr::select(W1_sex, formatted) %>% 
    pivot_wider(
      names_from = W1_sex,
      values_from = formatted,
      values_fill = "0 (0%)")
  
  sex_table <- table(df$W1_sex)
  p_val <- if (min(sex_table)< 5){
    fisher.test(sex_table)$p.value
  } else{
    chisq.test(sex_table, correct = FALSE)$p.value
  }
  out$Condition <- var
  out$Total_N <- total_n
  out$p_value <- round(p_val, 3)
  out
})

results_sex <- results_sex %>% 
  dplyr::select(Condition, Female, Male, Total_N, p_value)

print(results_sex, n = Inf)

#onset
analysis_sample %>% count(W1_sex, onset)