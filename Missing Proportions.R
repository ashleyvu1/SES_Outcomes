###########################
#### Missing Data Prop ####
###########################
propmiss <- function(dataframe, order = TRUE){
  m <- sapply(dataframe, function(x){
    data.frame(
      Ntot = length(x),
      nMiss = sum(is.na(x)),
      nValid = length(x) - sum(is.na(x)),
      propMiss = round(100*sum(is.na(x))/length(x), digit = 2)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)], d[-ncol(d)])
  if(order == FALSE){
    return(d[,])
  } else{
    return(d[order(d$propMiss),])
  }
}

vars <- c("att_life", "idea_srs_life", "idea_life","idea_recur_bin", "educ_bin", "hs_highereduc", "W26_empl_status", "W26_housing_cond", 
          "stable_unstable", "debt_nodebt", "neet", "W1_sex","W1_ses","W1_fam_str","W1_dep_mat","W1_dep_pat", "W1_race", "W1_anx",
          "W1_hyper_att", "W1_opposition", "W1_dep", "W1_antisoc_mat_bin","W1_antisoc_pat_bin")
propmiss <-  propmiss(analysis_sample[, vars])


library(openxlsx)
write.xlsx(propmiss, file = "/mnt/40295D00/Travail/Commun/Ashley/Data/propmiss.xlsx")