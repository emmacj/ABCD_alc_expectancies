
### 07/27/2022
### prepping ABCD data w/ updated FU1 items

library(lme4)
library(lmerTest)
library(metafor)
library(ggplot2)
library(tidyr)
library(psych)

setwd("~/Dropbox/ABCD_alcexp_project/Summer2022_update/")

aeq <- read.table("aeq.txt",header=T, na.strings=c('777','888','999','NA'))
ACE <- read.csv("ACE_sumscores_1year.csv", header=T, na.strings=c('777','888','999','NA'))
baseline <- read.csv("Alc_Expectancies_Data_Baseline.csv",header=T,na.strings=c('777','888','999','NA'))
FU1 <- read.csv("Alc_Expectancies_Data_FU1.csv", header=T, na.strings=c('777','888','999','NA'))
sipping <- read.csv("../aeq_with_covars_20220412.csv",header=T, na.strings=c('777','888','999','NA'))

aeq1 <- aeq[aeq$eventname == "1_year_follow_up_y_arm_1",]

aeq_full <- merge(aeq1,ACE,by.x="subjectkey",by.y="id_redcap")
aeq_full <- merge(aeq_full, baseline[,-c(2,3)], by="subjectkey")
aeq_full <- merge(aeq_full, FU1, by="subjectkey")

table(aeq_full$peer_deviance_2_l)

## recode race var
aeq_full$race[aeq_full$demo_race_a_p___10 == 1] <- "White"
aeq_full$race[aeq_full$demo_race_a_p___11 == 1] <- "Black"
aeq_full$race[aeq_full$demo_race_a_p___12 == 1 | aeq_full$demo_race_a_p___13 == 1] <- "AIAN"
aeq_full$race[aeq_full$demo_race_a_p___14 == 1 | aeq_full$demo_race_a_p___15 == 1 | aeq_full$demo_race_a_p___16 == 1 | aeq_full$demo_race_a_p___17 == 1] <- "NHPI"
aeq_full$race[aeq_full$demo_race_a_p___18 == 1 | aeq_full$demo_race_a_p___19 == 1 | aeq_full$demo_race_a_p___20 == 1 | aeq_full$demo_race_a_p___21 == 1 | aeq_full$demo_race_a_p___22 == 1 | aeq_full$demo_race_a_p___23 == 1 | aeq_full$demo_race_a_p___24 == 1] <- "Asian"
aeq_full$race[aeq_full$demo_race_a_p___25 == 1] <- "Other"
aeq_full$nraces <- rowSums(aeq_full[,c("demo_race_a_p___10","demo_race_a_p___11","demo_race_a_p___12","demo_race_a_p___13","demo_race_a_p___14","demo_race_a_p___15","demo_race_a_p___16","demo_race_a_p___17","demo_race_a_p___18","demo_race_a_p___19","demo_race_a_p___20","demo_race_a_p___21","demo_race_a_p___22","demo_race_a_p___23","demo_race_a_p___24")])
aeq_full$race[aeq_full$nraces > 1] <- "Multiracial"

### recode marriage variable
aeq_full$marriage[aeq_full$demo_prnt_marital_v2 == 1] <- "Married"
aeq_full$marriage[aeq_full$demo_prnt_marital_v2 == 2 | aeq_full$demo_prnt_marital_v2 == 3 | aeq_full$demo_prnt_marital_v2 == 4 | aeq_full$demo_prnt_marital_v2 == 5 | aeq_full$demo_prnt_marital_v2 == 6] <- "Not Married" 

### recode income variable
aeq_full$income[aeq_full$demo_comb_income_v2 == 1 | aeq_full$demo_comb_income_v2 == 2 | aeq_full$demo_comb_income_v2 == 3 | aeq_full$demo_comb_income_v2 == 4 | aeq_full$demo_comb_income_v2 == 5] <- "Less than $35k"
aeq_full$income[aeq_full$demo_comb_income_v2 == 6 | aeq_full$demo_comb_income_v2 == 7 | aeq_full$demo_comb_income_v2 == 8] <- "$35k - 100k"
aeq_full$income[aeq_full$demo_comb_income_v2 == 9 | aeq_full$demo_comb_income_v2 == 10] <- "Greater than 100k"

### create average edu variable from caregiver and partner
aeq_full <- transform(aeq_full, avgedu = rowMeans(aeq_full[,c("demo_prnt_ed_v2","demo_prtnr_ed_v2")], na.rm = TRUE))

### going to use two religious variables:
### 1: create new  religiosity binary var, if denomination -> 1, if atheist or agnostic or "nothing in particular" -> -1 
aeq_full$relig[aeq_full$demo_relig_v2 %in% c(1:13,16)] <- 1
aeq_full$relig[aeq_full$demo_relig_v2 == 14 | aeq_full$demo_relig_v2 == 15 | aeq_full$demo_relig_v2 == 17] <- -1

### 2: is religion important to child (demo_yrs_2)
aeq_full$religimp <- aeq_full$demo_yrs_2

### recode self-reported parent alcohol problems
aeq_full$parentalc[aeq_full$asr_q90_p == "0"] <- 0
aeq_full$parentalc[aeq_full$asr_q90_p == "1"] <- 1
aeq_full$parentalc[aeq_full$asr_q90_p == "2"] <- 2

### recode aeq sum scores 
aeq_full$aeq_pos_sum <- rowSums(aeq_full[,c("aeq_section_q01", "aeq_section_q02", "aeq_section_q04", "aeq_section_q06")])
aeq_full$aeq_neg_sum <- rowSums(aeq_full[,c("aeq_section_q03", "aeq_section_q05", "aeq_section_q07")])

## merge in sipping data
aeq_full <- merge(aeq_full,sipping[,c("subjectkey","tlfb_alc_sip_l")],by="subjectkey")

## merge in site ID
site <- read.table("abcd_lt01.txt",header=T)
site2 <- unique(site[,c("subjectkey","site_id_l")])

aeq_full <- merge(aeq_full,site2[,c("subjectkey","site_id_l")],by="subjectkey")

## merge in family IDs (from ABCD)
famid <- read.csv("abcd3_fam_ids.csv",header=T)

aeq_full <- merge(aeq_full,famid,by="subjectkey")

## only keep kids who have not sipped
aeq_full_nosip <- subset(aeq_full, tlfb_alc_sip_l == 0)

## code ethnicity
aeq_full_nosip$ethnicity[aeq_full_nosip$demo_ethn_v2 == 1] <- 1
aeq_full_nosip$ethnicity[aeq_full_nosip$demo_ethn_v2 == 2] <- 0


######### environmental vars all ready
######### merge in PRS now 

EA_PRS <- read.table("../aeq_PRS_covs_dataset.txt",header=T)

AA_PCs <- read.table("ABCD3_AA_PCA.txt",header=T)
AA_risk <- read.table("risk_abcd_afr_prs_complete.txt.profile",header=T)
AA_risk$Riskscaled <- scale(AA_risk$SCORE)
AA_dep <- read.table("dep_abcd_afr_prs_complete.txt.profile",header=T)
AA_dep$MDDscaled <- scale(AA_dep$SCORE)
AA_aud <- read.table("aud_abcd_afr_prs_complete.txt.profile",header=T)
AA_aud$AUDscaled <- scale(AA_aud$SCORE)

EA_fid <- read.table("EUR_genetic_FIDs.txt",header=T)
AA_fid <- read.table("AFR_genetic_FIDs.txt",header=T)

EA_PRS_aeq <- merge(aeq_full,EA_PRS[,-c(2,3)],by.x="subjectkey",by.y="ID")
EA_PRS_aeq_full <- merge(EA_PRS_aeq,EA_fid,by.x="subjectkey",by.y="ID")
## only keep kids who have not sipped
EA_PRS_aeq_full_nosip <- subset(EA_PRS_aeq_full, tlfb_alc_sip_l == 0)
## code ethnicity
EA_PRS_aeq_full_nosip$ethnicity[EA_PRS_aeq_full_nosip$demo_ethn_v2 == 1] <- 1
EA_PRS_aeq_full_nosip$ethnicity[EA_PRS_aeq_full_nosip$demo_ethn_v2 == 2] <- 0



AA_PRS_aeq <- merge(aeq_full,AA_PCs[,-c(1)],by.x="subjectkey",by.y="IID")
AA_PRS_aeq2 <- merge(AA_PRS_aeq,AA_risk[,c(2,7)],by.x="subjectkey",by.y="IID")
AA_PRS_aeq3 <- merge(AA_PRS_aeq2,AA_dep[,c(2,7)],by.x="subjectkey",by.y="IID")
AA_PRS_aeq4 <- merge(AA_PRS_aeq3,AA_aud[,c(2,7)],by.x="subjectkey",by.y="IID")
AA_PRS_aeq_full <- merge(AA_PRS_aeq4,AA_fid,by.x="subjectkey",by.y="ID")
## only keep kids who have not sipped
AA_PRS_aeq_full_nosip <- subset(AA_PRS_aeq_full, tlfb_alc_sip_l == 0)
## code ethnicity
AA_PRS_aeq_full_nosip$ethnicity[AA_PRS_aeq_full_nosip$demo_ethn_v2 == 1] <- 1
AA_PRS_aeq_full_nosip$ethnicity[AA_PRS_aeq_full_nosip$demo_ethn_v2 == 2] <- 0


######### genetic vars all ready
######### primary datasets: 
######### aeq_full_nosip = everyone, all vars except PRS
######### EA_PRS_aeq_full_nosip = all genetically-defined EA ppl, all vars
######### AA_PRS_aeq_full_nosip = all genetically-defined AA ppl, all vars

######### analyses to run: 
## Positive alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
## Negative alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
## Positive alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + covariates → in models run separately in European-ancestry and African-ancestry children who have not sipped alcohol, then meta-analyze results
## Negative alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + covariates → in models run separately in European-ancestry and African-ancestry children who have not sipped alcohol, then meta-analyze results
## Positive alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in models run separately in European-ancestry and African-ancestry children who have not sipped alcohol, then meta-analyze results
## Negative alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in models run separately in European-ancestry and African-ancestry children who have not sipped alcohol, then meta-analyze results

## first, look at correlations b/w PRS in EAs
cor.test(EA_PRS_aeq_full_nosip$DPWscaled,EA_PRS_aeq_full_nosip$PAUscaled) ## r ~ 0.614
cor.test(EA_PRS_aeq_full_nosip$DPWscaled,EA_PRS_aeq_full_nosip$MDDscaled) ## r ~ 0.024
cor.test(EA_PRS_aeq_full_nosip$DPWscaled,EA_PRS_aeq_full_nosip$Riskscaled) ## r ~ 0.214
cor.test(EA_PRS_aeq_full_nosip$PAUscaled,EA_PRS_aeq_full_nosip$MDDscaled) ## r ~ 0.172
cor.test(EA_PRS_aeq_full_nosip$PAUscaled,EA_PRS_aeq_full_nosip$Riskscaled) ## r ~ 0.208
cor.test(EA_PRS_aeq_full_nosip$MDDscaled,EA_PRS_aeq_full_nosip$Riskscaled) ## r ~ 0.165

cor.test(AA_PRS_aeq_full_nosip$AUDscaled,AA_PRS_aeq_full_nosip$MDDscaled) ## r ~ 0.073
cor.test(AA_PRS_aeq_full_nosip$AUDscaled,AA_PRS_aeq_full_nosip$Riskscaled) ## r ~ 0.010
cor.test(AA_PRS_aeq_full_nosip$MDDscaled,AA_PRS_aeq_full_nosip$Riskscaled) ## r ~ 0.019

## also look at correlations b/w friend vars 
cor.test(aeq_full_nosip$ptu_a_y,aeq_full_nosip$ptu_b_y) ## r ~ 0.602
cor.test(aeq_full_nosip$ptu_a_y,aeq_full_nosip$ptu_c_y) ## r ~ 0.554
cor.test(aeq_full_nosip$ptu_b_y,aeq_full_nosip$ptu_c_y) ## r ~ 0.751
cor.test(aeq_full_nosip$peer_deviance_2_l,aeq_full_nosip$peer_deviance_3_l) ## r ~ 0.505
cor.test(aeq_full_nosip$peer_deviance_2_l,aeq_full_nosip$peer_deviance_4_l) ## r ~ 0.394
cor.test(aeq_full_nosip$peer_deviance_3_l,aeq_full_nosip$peer_deviance_4_l) ## r ~ 0.424
cor.test(aeq_full_nosip$ptu_a_y,aeq_full_nosip$peer_deviance_2_l) ## r ~ -0.073
cor.test(aeq_full_nosip$ptu_a_y,aeq_full_nosip$peer_deviance_3_l) ## r ~ -0.017
cor.test(aeq_full_nosip$ptu_a_y,aeq_full_nosip$peer_deviance_4_l) ## r ~ -0.023
cor.test(aeq_full_nosip$ptu_b_y,aeq_full_nosip$peer_deviance_2_l) ## r ~ -0.061
cor.test(aeq_full_nosip$ptu_b_y,aeq_full_nosip$peer_deviance_3_l) ## r ~ -0.027
cor.test(aeq_full_nosip$ptu_b_y,aeq_full_nosip$peer_deviance_4_l) ## r ~ -0.012
cor.test(aeq_full_nosip$ptu_c_y,aeq_full_nosip$peer_deviance_2_l) ## r ~ -0.041
cor.test(aeq_full_nosip$ptu_c_y,aeq_full_nosip$peer_deviance_3_l) ## r ~ -0.025
cor.test(aeq_full_nosip$ptu_c_y,aeq_full_nosip$peer_deviance_4_l) ## r ~ -0.027

### Model 1
## Positive alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
pos_env <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env)

### Model 2
## Negative alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
neg_env <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_env)

### Model 3.a 
## Positive alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + covariates → in European-ancestry  children who have not sipped alcohol
pos_ea_prs <- lmer(aeq_pos_sum ~ DPWscaled + PAUscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_prs)

## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + covariates → in European-ancestry  children who have not sipped alcohol - fewer PRS to match AAs
pos_ea_prsv2 <- lmer(aeq_pos_sum ~ PAUscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_prsv2)

### Model 3.b 
## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + covariates → in African-ancestry  children who have not sipped alcohol
pos_aa_prs <- lmer(aeq_pos_sum ~ AUDscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(pos_aa_prs)

### meta-analysis
pos_prs_res_pau <- rma(yi = c(9.260e-02,-0.08394),sei=c(4.650e-02,0.09063),method="FE")
pos_prs_res_pau
## Test for Heterogeneity:
##  Q(df = 1) = 3.0037, p-val = 0.0831

##Model Results:
##  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0558  0.0414  1.3490  0.1773  -0.0253  0.1369

pos_prs_res_mdd <- rma(yi = c(-4.130e-02,0.01661),sei=c(4.551e-02,0.09083),method="FE")
pos_prs_res_mdd

## Test for Heterogeneity:
##   Q(df = 1) = 0.3249, p-val = 0.5687

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0297  0.0407  -0.7294  0.4657  -0.1094  0.0501 

pos_prs_res_risk <- rma(yi = c(7.554e-02,-0.07694),sei=c(4.564e-02,0.09142),method="FE")
pos_prs_res_risk

## Test for Heterogeneity:
##   Q(df = 1) = 2.2269, p-val = 0.1356

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0451  0.0408  1.1049  0.2692  -0.0349  0.1252    

pos_prs_res_age <- rma(yi = c(3.010e-02,0.03924),sei=c(5.676e-03,0.01173),method="FE")
pos_prs_res_age

## Test for Heterogeneity:
##   Q(df = 1) = 0.4920, p-val = 0.4831

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
##  0.0318  0.0051  6.2306  <.0001  0.0218  0.0418  ***
## pval = 4.645055e-10

pos_prs_res_sex <- rma(yi = c(1.650e-01,0.06643),sei=c(8.823e-02,0.17610),method="FE")
pos_prs_res_sex

## Test for Heterogeneity:
## Q(df = 1) = 0.2504, p-val = 0.6168

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1452  0.0789  1.8410  0.0656  -0.0094  0.2998  

### Model 4.a 
## Negative alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + covariates → in European-ancestry children who have not sipped alcohol
neg_ea_prs <- lmer(aeq_neg_sum ~ DPWscaled + PAUscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_prs)

## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + covariates → in European-ancestry children who have not sipped alcohol - fewer PRS to match AAs
neg_ea_prsv2 <- lmer(aeq_neg_sum ~ PAUscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_prsv2)

### Model 4.b 
## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + covariates → in African-ancestry children who have not sipped alcohol
neg_aa_prs <- lmer(aeq_neg_sum ~ AUDscaled + MDDscaled + Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(neg_aa_prs)

### meta-analysis
neg_prs_res_pau <- rma(yi = c(-2.375e-02,-4.817e-02),sei=c(4.355e-02,9.742e-02),method="FE")
neg_prs_res_pau

## Test for Heterogeneity:
## Q(df = 1) = 0.0524, p-val = 0.8190

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0278  0.0398  -0.6997  0.4841  -0.1057  0.0501 

neg_prs_res_mdd <- rma(yi = c(8.939e-03,2.406e-02),sei=c(4.263e-02,9.778e-02),method="FE")
neg_prs_res_mdd

## Test for Heterogeneity:
## Q(df = 1) = 0.0201, p-val = 0.8873

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0114  0.0391  0.2906  0.7714  -0.0652  0.0879

neg_prs_res_risk <- rma(yi = c(1.149e-01,1.205e-01),sei=c(4.275e-02,9.839e-02),method="FE")
neg_prs_res_risk

## Test for Heterogeneity:
## Q(df = 1) = 0.0027, p-val = 0.9584

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.1158  0.0392  2.9531  0.0031  0.0389  0.1926

neg_prs_res_age <- rma(yi = c(3.018e-02,3.557e-02),sei=c(5.343e-03,1.260e-02),method="FE")
neg_prs_res_age

## Test for Heterogeneity:
## Q(df = 1) = 0.1551, p-val = 0.6937

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0310  0.0049  6.3024  <.0001  0.0214  0.0406
## pval = 2.93108e-10

neg_prs_res_sex <- rma(yi = c(1.451e-01,1.665e-01),sei=c(8.272e-02,1.897e-01),method="FE")
neg_prs_res_sex

## Test for Heterogeneity:
##  Q(df = 1) = 0.0107, p-val = 0.9176

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1485  0.0758  1.9587  0.0501  -0.0001  0.2971


### Model 5.a 
## Positive alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
pos_ea_env_prs <- lmer(aeq_pos_sum ~ DPWscaled + PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_env_prs)

## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
pos_ea_env_prsv2 <- lmer(aeq_pos_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_env_prsv2)

### Model 5.b
## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in African-ancestry children who have not sipped alcohol
pos_aa_env_prs <- lmer(aeq_pos_sum ~ AUDscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(pos_aa_env_prs)

### meta-analysis
pos_prs_env_res_age <- rma(yi = c(3.194e-02,0.04526),sei=c(6.189e-03,0.01496),method="FE")
pos_prs_env_res_age

## Test for Heterogeneity:
## Q(df = 1) = 0.6769, p-val = 0.4107

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0339  0.0057  5.9253  <.0001  0.0227  0.0451
## pval = 3.116483e-09

pos_prs_env_res_sex <- rma(yi = c(9.868e-02,-0.07850),sei=c(9.671e-02,0.22040),method="FE")
pos_prs_env_res_sex

## Test for Heterogeneity:
## Q(df = 1) = 0.5419, p-val = 0.4616

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0701  0.0886  0.7913  0.4288  -0.1035  0.2436 

pos_prs_env_res_marriage <- rma(yi = c(9.692e-02,-0.01982),sei=c(1.498e-01,0.27834),method="FE")
pos_prs_env_res_marriage

## Test for Heterogeneity:
## Q(df = 1) = 0.1364, p-val = 0.7119

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0707  0.1319  0.5360  0.5920  -0.1878  0.3292   

pos_prs_env_res_income100 <- rma(yi = c(4.379e-03,-0.09207),sei=c(1.154e-01,0.39414),method="FE")
pos_prs_env_res_income100

## Test for Heterogeneity:
##   Q(df = 1) = 0.0552, p-val = 0.8143

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0032  0.1108  -0.0292  0.9767  -0.2203  0.2138

pos_prs_env_res_income35 <- rma(yi = c(-1.452e-02,-0.21466),sei=c(2.233e-01,0.26752),method="FE")
pos_prs_env_res_income35

## Test for Heterogeneity:
## Q(df = 1) = 0.3299, p-val = 0.5657

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0967  0.1714  -0.5641  0.5727  -0.4327  0.2393    

pos_prs_env_res_edu <- rma(yi = c(8.148e-02,0.10106),sei=c(3.107e-02,0.05546),method="FE")
pos_prs_env_res_edu

## Test for Heterogeneity:
##   Q(df = 1) = 0.0949, p-val = 0.7581

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0862  0.0271  3.1785  0.0015  0.0330  0.1393  ** 

pos_prs_env_res_relig <- rma(yi = c(-6.142e-02,-0.04904),sei=c(7.675e-02,0.15857),method="FE")
pos_prs_env_res_relig

## Test for Heterogeneity:
## Q(df = 1) = 0.0049, p-val = 0.9440

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0591  0.0691  -0.8551  0.3925  -0.1945  0.0763    

pos_prs_env_res_religimp <- rma(yi = c(-2.111e-01,-0.07327),sei=c(6.451e-02,0.13005),method="FE")
pos_prs_env_res_religimp

## Test for Heterogeneity:
## Q(df = 1) = 0.9014, p-val = 0.3424

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.1839  0.0578  -3.1819  0.0015  -0.2972  -0.0706

pos_prs_env_res_parentalc <- rma(yi = c(2.654e-01,0.33495),sei=c(1.441e-01,0.37871),method="FE")
pos_prs_env_res_parentalc

## Test for Heterogeneity:
## Q(df = 1) = 0.0295, p-val = 0.8637

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.2742  0.1347  2.0359  0.0418  0.0102  0.5382

pos_prs_env_res_ace <- rma(yi = c(1.455e-01,0.13565),sei=c(2.462e-02,0.04131),method="FE")
pos_prs_env_res_ace

## Test for Heterogeneity:
## Q(df = 1) = 0.0420, p-val = 0.8377

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.1429  0.0211  6.7577  <.0001  0.1015  0.1844 
## pval = 1.401734e-11

pos_prs_env_res_ptu_a_y <- rma(yi = c(-6.914e-01,-0.57525),sei=c(1.246e-01,0.27743),method="FE")
pos_prs_env_res_ptu_a_y

## Test for Heterogeneity:
## Q(df = 1) = 0.1459, p-val = 0.7025

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.6719  0.1137  -5.9114  <.0001  -0.8947  -0.4491 
## pval = 3.392475e-09

pos_prs_env_res_ptu_b_y <- rma(yi = c(6.917e-02,-0.05709),sei=c(2.404e-01,0.41397),method="FE")
pos_prs_env_res_ptu_b_y

## Test for Heterogeneity:
## Q(df = 1) = 0.0696, p-val = 0.7920

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0373  0.2079  0.1796  0.8575  -0.3701  0.4448 

pos_prs_env_res_ptu_c_y <- rma(yi = c(3.998e-01,0.09170),sei=c(2.533e-01,0.42880),method="FE")
pos_prs_env_res_ptu_c_y

## Test for Heterogeneity:
## Q(df = 1) = 0.3827, p-val = 0.5362

## Model Results:
  
##  estimate      se    zval    pval    ci.lb   ci.ub 
## 0.3201  0.2181  1.4677  0.1422  -0.1074  0.7476 

pos_prs_env_res_peer_deviance_2_l <- rma(yi = c(2.769e-01,0.49290),sei=c(4.491e-01,0.90905),method="FE")
pos_prs_env_res_peer_deviance_2_l

## Test for Heterogeneity:
## Q(df = 1) = 0.0454, p-val = 0.8313

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.3193  0.4026  0.7929  0.4278  -0.4699  1.1084  

pos_prs_env_res_peer_deviance_3_l <- rma(yi = c(-4.490e-01,0.66255),sei=c(7.124e-01,1.49781),method="FE")
pos_prs_env_res_peer_deviance_3_l

## Test for Heterogeneity:
## Q(df = 1) = 0.4491, p-val = 0.5027

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.2439  0.6433  -0.3792  0.7046  -1.5049  1.0170

pos_prs_env_res_peer_deviance_4_l <- rma(yi = c(5.506e-02,-0.08163),sei=c(4.664e-01,0.85575),method="FE")
pos_prs_env_res_peer_deviance_4_l

## Test for Heterogeneity:
## Q(df = 1) = 0.0197, p-val = 0.8885

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0238  0.4095  0.0580  0.9537  -0.7789  0.8264 

pos_prs_env_res_pau <- rma(yi = c(1.048e-01,-0.16810),sei=c(5.064e-02,0.11396),method="FE")
pos_prs_env_res_pau

## Test for Heterogeneity:
## Q(df = 1) = 4.7890, p-val = 0.0286

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0598  0.0463  1.2922  0.1963  -0.0309  0.1505    

pos_prs_env_res_mdd <- rma(yi = c(-2.425e-02,0.02807),sei=c(4.969e-02,0.11667),method="FE")
pos_prs_env_res_mdd

## Test for Heterogeneity:
## Q(df = 1) = 0.1702, p-val = 0.6799

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0162  0.0457  -0.3547  0.7228  -0.1058  0.0734  

pos_prs_env_res_risk <- rma(yi = c(3.374e-02,-0.04093),sei=c(4.989e-02,0.11605),method="FE")
pos_prs_env_res_risk

## Test for Heterogeneity:
## Q(df = 1) = 0.3494, p-val = 0.5544

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0221  0.0458  0.4820  0.6298  -0.0677  0.1119 



### Model 6.a 
## Negative alc exp ~ DPW PRS + PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
neg_ea_env_prs <- lmer(aeq_neg_sum ~ DPWscaled + PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_env_prs)

## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
neg_ea_env_prsv2 <- lmer(aeq_neg_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_env_prsv2)

### Model 6.b
## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in African-ancestry children who have not sipped alcohol
neg_aa_env_prs <- lmer(aeq_neg_sum ~ AUDscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) +  (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(neg_aa_env_prs)

### meta-analysis
neg_prs_env_res_age <- rma(yi = c(2.482e-02,0.038826),sei=c(5.907e-03,0.016413),method="FE")
neg_prs_env_res_age

## Test for Heterogeneity:
## Q(df = 1) = 0.6447, p-val = 0.4220

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0264  0.0056  4.7546  <.0001  0.0155  0.0373  
## pval = 1.988345e-06

neg_prs_env_res_sex <- rma(yi = c(7.674e-02,0.173714),sei=c(9.174e-02,0.243252),method="FE")
neg_prs_env_res_sex

## Test for Heterogeneity:
## Q(df = 1) = 0.1391, p-val = 0.7091

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0888  0.0858  1.0347  0.3008  -0.0794  0.2571    

neg_prs_env_res_marriage <- rma(yi = c(-3.719e-03,-0.108158),sei=c(1.419e-01,0.307289),method="FE")
neg_prs_env_res_marriage

## Test for Heterogeneity:
## Q(df = 1) = 0.0952, p-val = 0.7577

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0221  0.1288  -0.1714  0.8639  -0.2746  0.2304    

neg_prs_env_res_income100 <- rma(yi = c(-1.913e-01,-0.348484),sei=c(1.094e-01,0.433295),method="FE")
neg_prs_env_res_income100

## Test for Heterogeneity:
## Q(df = 1) = 0.1237, p-val = 0.7250

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.2007  0.1061  -1.8923  0.0584  -0.4086  0.0072 

neg_prs_env_res_income35 <- rma(yi = c(-4.227e-01,-0.438499),sei=c(2.118e-01,0.293356),method="FE")
neg_prs_env_res_income35

## Test for Heterogeneity:
## Q(df = 1) = 0.0019, p-val = 0.9652

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.4281  0.1717  -2.4931  0.0127  -0.7647  -0.0915    

neg_prs_env_res_edu <- rma(yi = c(9.341e-02,0.119879),sei=c(2.946e-02,0.060979),method="FE")
neg_prs_env_res_edu

## Test for Heterogeneity:
## Q(df = 1) = 0.1528, p-val = 0.6959

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0984  0.0265  3.7102  0.0002  0.0464  0.1504

neg_prs_env_res_relig <- rma(yi = c(-8.892e-03,-0.272786),sei=c(7.281e-02,0.174351),method="FE")
neg_prs_env_res_relig

## Test for Heterogeneity:
## Q(df = 1) = 1.9507, p-val = 0.1625

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0481  0.0672  -0.7156  0.4742  -0.1798  0.0836

neg_prs_env_res_religimp <- rma(yi = c(-6.124e-02,0.171492),sei=c(6.143e-02,0.142745),method="FE")
neg_prs_env_res_religimp

## Test for Heterogeneity:
## Q(df = 1) = 2.2428, p-val = 0.1342

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0249  0.0564  -0.4408  0.6594  -0.1355  0.0857

neg_prs_env_res_parentalc <- rma(yi = c(-4.575e-02,-0.553439),sei=c(1.365e-01,0.416958),method="FE")
neg_prs_env_res_parentalc

## Test for Heterogeneity:
## Q(df = 1) = 1.3390, p-val = 0.2472

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0949  0.1297  -0.7315  0.4645  -0.3492  0.1594 

neg_prs_env_res_ace <- rma(yi = c(8.604e-02,0.022546),sei=c(2.340e-02,0.045391),method="FE")
neg_prs_env_res_ace

## Test for Heterogeneity:
## Q(df = 1) = 1.5459, p-val = 0.2137

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0727  0.0208  3.4958  0.0005  0.0319  0.1135

neg_prs_env_res_ptu_a_y <- rma(yi = c(1.692e-01,0.252610),sei=c(1.184e-01,0.306194),method="FE")
neg_prs_env_res_ptu_a_y

## Test for Heterogeneity:
## Q(df = 1) = 0.0646, p-val = 0.7994

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1800  0.1104  1.6304  0.1030  -0.0364  0.3965 

neg_prs_env_res_ptu_b_y <- rma(yi = c(2.510e-01,0.592378),sei=c(2.284e-01,0.457324),method="FE")
neg_prs_env_res_ptu_b_y

## Test for Heterogeneity:
## Q(df = 1) = 0.4460, p-val = 0.5043

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.3192  0.2043  1.5619  0.1183  -0.0813  0.7196

neg_prs_env_res_ptu_c_y <- rma(yi = c(1.780e-02,0.006924),sei=c(2.407e-01,0.473754),method="FE")
neg_prs_env_res_ptu_c_y

## Test for Heterogeneity:
## Q(df = 1) = 0.0004, p-val = 0.9837

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0156  0.2146  0.0725  0.9422  -0.4050  0.4362

neg_prs_env_res_peer_deviance_2_l <- rma(yi = c(1.358e-01,-0.030080),sei=c(4.266e-01,1.005532),method="FE")
neg_prs_env_res_peer_deviance_2_l

## Test for Heterogeneity:
## Q(df = 1) = 0.0231, p-val = 0.8793

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1105  0.3927  0.2814  0.7784  -0.6592  0.8802 

neg_prs_env_res_peer_deviance_3_l <- rma(yi = c(-7.729e-01,-1.219726),sei=c(6.767e-01,1.652152),method="FE")
neg_prs_env_res_peer_deviance_3_l

## Test for Heterogeneity:
## Q(df = 1) = 0.0626, p-val = 0.8024

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.8371  0.6262  -1.3368  0.1813  -2.0644  0.3903 

neg_prs_env_res_peer_deviance_4_l <- rma(yi = c(-4.045e-01,0.903428),sei=c(4.429e-01,0.945702),method="FE")
neg_prs_env_res_peer_deviance_4_l

## Test for Heterogeneity:
## Q(df = 1) = 1.5687, p-val = 0.2104

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.1692  0.4011  -0.4219  0.6731  -0.9554  0.6169 

neg_prs_env_res_pau <- rma(yi = c(2.895e-03,-0.053813),sei=c(4.800e-02,0.125899),method="FE")
neg_prs_env_res_pau

## Test for Heterogeneity:
## Q(df = 1) = 0.1771, p-val = 0.6738

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0043  0.0449  -0.0959  0.9236  -0.0922  0.0836    

neg_prs_env_res_mdd <- rma(yi = c(6.078e-02,0.046762),sei=c(4.709e-02,0.128938),method="FE")
neg_prs_env_res_mdd

## Test for Heterogeneity:
## Q(df = 1) = 0.0104, p-val = 0.9187

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0591  0.0442  1.3368  0.1813  -0.0276  0.1458

neg_prs_env_res_risk <- rma(yi = c(7.952e-02,0.033996),sei=c(4.726e-02,0.128101),method="FE")
neg_prs_env_res_risk

## Test for Heterogeneity:
## Q(df = 1) = 0.1112, p-val = 0.7388

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0741  0.0443  1.6705  0.0948  -0.0128  0.1610 


#### follow-up models: test friend vars individually
## ptu_a_y
pos_ptu_a <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_ptu_a)

neg_ptu_a <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_ptu_a)

## ptu_b_y
pos_ptu_b <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_b_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_ptu_b)

neg_ptu_b <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_b_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_ptu_b)

## ptu_c_y
pos_ptu_c <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_ptu_c)

neg_ptu_c <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_c_y + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_ptu_c)

## peer_deviance_2_l 
pos_peer_deviance_2_l <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_peer_deviance_2_l)

neg_peer_deviance_2_l <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_2_l  + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_peer_deviance_2_l)

## peer_deviance_3_l 
pos_peer_deviance_3_l <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_3_l +  (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_peer_deviance_3_l)

neg_peer_deviance_3_l <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_3_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_peer_deviance_3_l)

## peer_deviance_4_l 
pos_peer_deviance_4_l <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_peer_deviance_4_l)

neg_peer_deviance_4_l <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_a_y + ptu_b_y + ptu_c_y + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_peer_deviance_4_l)


## make peer tolerance sum score 
aeq_full_nosip <- transform(aeq_full_nosip, ptu_sum = rowSums(aeq_full_nosip[,c("ptu_a_y","ptu_b_y","ptu_c_y")], na.rm = FALSE))

pos_ptu_sum <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_sum + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_ptu_sum)

neg_ptu_sum <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_sum + peer_deviance_2_l + peer_deviance_3_l + peer_deviance_4_l + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_ptu_sum)

## make peer deviance sum score 
aeq_full_nosip <- transform(aeq_full_nosip, peerdev_sum = rowSums(aeq_full_nosip[,c("peer_deviance_2_l","peer_deviance_3_l","peer_deviance_4_l")], na.rm = FALSE))

pos_peer_sum <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_peer_sum)

neg_peer_sum <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + parentalc + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_peer_sum)

### now look at PRS assoc. one at a time
## neg aeq
neg_ea_pauprs <- lmer(aeq_neg_sum ~ PAUscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_pauprs)

neg_ea_riskprs <- lmer(aeq_neg_sum ~ Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_riskprs)

neg_ea_mddprs <- lmer(aeq_neg_sum ~ MDDscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_mddprs)

## pos aeq
pos_ea_pauprs <- lmer(aeq_pos_sum ~ PAUscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_pauprs)

pos_ea_riskprs <- lmer(aeq_pos_sum ~ Riskscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_riskprs)

pos_ea_mddprs <- lmer(aeq_pos_sum ~ MDDscaled + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_mddprs)

##### NEW MODELS
##### include sum scores for peer tolerance and peer use
##### and family history measures for alcohol probs.


table(aeq_full_nosip$famhx_ss_fath_prob_alc_p,aeq_full_nosip$famhx_ss_moth_prob_alc_p) ## weird NA value
aeq_full_nosip$fath_alc_prob[aeq_full_nosip$famhx_ss_fath_prob_alc_p == 1] <- 1
aeq_full_nosip$fath_alc_prob[aeq_full_nosip$famhx_ss_fath_prob_alc_p == 0] <- 0
aeq_full_nosip$moth_alc_prob[aeq_full_nosip$famhx_ss_moth_prob_alc_p == 1] <- 1
aeq_full_nosip$moth_alc_prob[aeq_full_nosip$famhx_ss_moth_prob_alc_p == 0] <- 0
table(aeq_full_nosip$fath_alc_prob,aeq_full_nosip$moth_alc_prob)
cor.test(aeq_full_nosip$fath_alc_prob,aeq_full_nosip$moth_alc_prob)


### Model 1
## Positive alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
pos_env_fin <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env_fin)

### Model 2
## Negative alc exp ~ env predictors + covariates → in sample of children who have not sipped alcohol
neg_env_fin <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_env_fin)

### need to update PRS datasets as well 
EA_PRS_aeq_full_nosip$fath_alc_prob[EA_PRS_aeq_full_nosip$famhx_ss_fath_prob_alc_p == 1] <- 1
EA_PRS_aeq_full_nosip$fath_alc_prob[EA_PRS_aeq_full_nosip$famhx_ss_fath_prob_alc_p == 0] <- 0
EA_PRS_aeq_full_nosip$moth_alc_prob[EA_PRS_aeq_full_nosip$famhx_ss_moth_prob_alc_p == 1] <- 1
EA_PRS_aeq_full_nosip$moth_alc_prob[EA_PRS_aeq_full_nosip$famhx_ss_moth_prob_alc_p == 0] <- 0
table(EA_PRS_aeq_full_nosip$fath_alc_prob,EA_PRS_aeq_full_nosip$moth_alc_prob)
AA_PRS_aeq_full_nosip$fath_alc_prob[AA_PRS_aeq_full_nosip$famhx_ss_fath_prob_alc_p == 1] <- 1
AA_PRS_aeq_full_nosip$fath_alc_prob[AA_PRS_aeq_full_nosip$famhx_ss_fath_prob_alc_p == 0] <- 0
AA_PRS_aeq_full_nosip$moth_alc_prob[AA_PRS_aeq_full_nosip$famhx_ss_moth_prob_alc_p == 1] <- 1
AA_PRS_aeq_full_nosip$moth_alc_prob[AA_PRS_aeq_full_nosip$famhx_ss_moth_prob_alc_p == 0] <- 0
table(AA_PRS_aeq_full_nosip$fath_alc_prob,AA_PRS_aeq_full_nosip$moth_alc_prob)

EA_PRS_aeq_full_nosip <- transform(EA_PRS_aeq_full_nosip, ptu_sum = rowSums(EA_PRS_aeq_full_nosip[,c("ptu_a_y","ptu_b_y","ptu_c_y")], na.rm = FALSE))
AA_PRS_aeq_full_nosip <- transform(AA_PRS_aeq_full_nosip, ptu_sum = rowSums(AA_PRS_aeq_full_nosip[,c("ptu_a_y","ptu_b_y","ptu_c_y")], na.rm = FALSE))

EA_PRS_aeq_full_nosip <- transform(EA_PRS_aeq_full_nosip, peerdev_sum = rowSums(EA_PRS_aeq_full_nosip[,c("peer_deviance_2_l","peer_deviance_3_l","peer_deviance_4_l")], na.rm = FALSE))
AA_PRS_aeq_full_nosip <- transform(AA_PRS_aeq_full_nosip, peerdev_sum = rowSums(AA_PRS_aeq_full_nosip[,c("peer_deviance_2_l","peer_deviance_3_l","peer_deviance_4_l")], na.rm = FALSE))


### Model 5.a 
## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
pos_ea_env_prs_fin <- lmer(aeq_pos_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(pos_ea_env_prs_fin)

### Model 5.b
## Positive alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in African-ancestry children who have not sipped alcohol
pos_aa_env_prs_fin <- lmer(aeq_pos_sum ~ AUDscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(pos_aa_env_prs_fin)

### meta-analysis
pos_prs_env_res_age_fin <- rma(yi = c(3.279e-02,0.04662),sei=c(6.261e-03,0.01538),method="FE")
pos_prs_env_res_age_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.6936, p-val = 0.4049

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0348  0.0058  5.9936  <.0001  0.0234  0.0461
## pval = 2.053064e-09

pos_prs_env_res_sex_fin <- rma(yi = c(9.657e-02,-0.08186),sei=c(9.818e-02,0.22793),method="FE")
pos_prs_env_res_sex_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.5419, p-val = 0.4616

## Model Results:

##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0701  0.0886  0.7913  0.4288  -0.1035  0.2436 

pos_prs_env_res_marriage <- rma(yi = c(9.692e-02,-0.01982),sei=c(1.498e-01,0.27834),method="FE")
pos_prs_env_res_marriage

## Test for Heterogeneity:
## Q(df = 1) = 0.5169, p-val = 0.4722

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0686  0.0902  0.7613  0.4465  -0.1081  0.2454  

pos_prs_env_res_income100_fin <- rma(yi = c(-3.107e-02,-0.01346),sei=c(1.171e-01,0.40731),method="FE")
pos_prs_env_res_income100_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0017, p-val = 0.9669

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0297  0.1125  -0.2641  0.7917  -0.2503  0.1909  

pos_prs_env_res_income35_fin <- rma(yi = c(1.721e-02,-0.20985),sei=c(2.282e-01,0.27229),method="FE")
pos_prs_env_res_income35_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.4085, p-val = 0.5227

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0765  0.1749  -0.4372  0.6619  -0.4193  0.2663    


pos_prs_env_res_edu_fin <- rma(yi = c(9.092e-02,0.07754),sei=c(3.159e-02,0.05702),method="FE")
pos_prs_env_res_edu_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0421, p-val = 0.8374

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0878  0.0276  3.1766  0.0015  0.0336  0.1419  ** 

pos_prs_env_res_relig_fin <- rma(yi = c(-5.081e-02,-0.07081),sei=c(7.823e-02,0.13336),method="FE")
pos_prs_env_res_relig_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0167, p-val = 0.8971

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0559  0.0675  -0.8289  0.4072  -0.1882  0.0763    

pos_prs_env_res_religimp <- rma(yi = c(-2.440e-01,-0.08082),sei=c(6.539e-02,0.13336),method="FE")
pos_prs_env_res_religimp

## Test for Heterogeneity:
## Q(df = 1) = 1.2070, p-val = 0.2719

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.2124  0.0587  -3.6172  0.0003  -0.3274  -0.0973 

pos_prs_env_res_fathalc_fin <- rma(yi = c(1.370e-01,0.46279),sei=c(1.613e-01,0.35752),method="FE")
pos_prs_env_res_fathalc_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.6899, p-val = 0.4062

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1921  0.1470  1.3065  0.1914  -0.0961  0.4803

pos_prs_env_res_mothalc_fin <- rma(yi = c(-2.264e-01,0.23251),sei=c(2.802e-01,0.57053),method="FE")
pos_prs_env_res_mothalc_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.5213, p-val = 0.4703

## Model Results:
  
##  estimate      se     zval    pval    ci.lb   ci.ub 
## -0.1372  0.2515  -0.5456  0.5853  -0.6302  0.3557 


pos_prs_env_res_ace_fin <- rma(yi = c(1.492e-01,0.13574),sei=c(2.518e-02,0.04241),method="FE")
pos_prs_env_res_ace_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0745, p-val = 0.7849

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.1457  0.0217  6.7290  <.0001  0.1033  0.1881
## pval = 1.708387e-11

pos_prs_env_res_ptu_sum_fin <- rma(yi = c(-1.945e-01,-0.19710),sei=c(5.055e-02,0.10007),method="FE")
pos_prs_env_res_ptu_sum_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0005, p-val = 0.9815

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.1950  0.0451  -4.3224  <.0001  -0.2835  -0.1066 
## pval = 1.543139e-05

pos_prs_env_res_peer_dev_sum_fin <- rma(yi = c(2.930e-02,0.50598),sei=c(1.535e-01,0.34266),method="FE")
pos_prs_env_res_peer_dev_sum_fin

## Test for Heterogeneity:
#3 Q(df = 1) = 1.6118, p-val = 0.2042

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.1090  0.1401  0.7779  0.4366  -0.1656  0.3835


pos_prs_env_res_pau_fin <- rma(yi = c(1.183e-01,-0.19637),sei=c(5.145e-02,0.11655),method="FE")
pos_prs_env_res_pau_fin

## Test for Heterogeneity:
## Q(df = 1) = 6.1005, p-val = 0.0135

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0670  0.0471  1.4231  0.1547  -0.0253  0.1592 

pos_prs_env_res_mdd_fin <- rma(yi = c(-1.437e-02,0.03244),sei=c(5.049e-02,0.11938),method="FE")
pos_prs_env_res_mdd_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.1304, p-val = 0.7180

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0073  0.0465  -0.1563  0.8758  -0.0984  0.0839    


pos_prs_env_res_risk_fin <- rma(yi = c(1.353e-02,-0.02595),sei=c(5.051e-02,0.11997),method="FE")
pos_prs_env_res_risk_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0920, p-val = 0.7617

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0076  0.0466  0.1629  0.8706  -0.0837  0.0988 

### Model 6.a 
## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in European-ancestry children who have not sipped alcohol
neg_ea_env_prs_fin <- lmer(aeq_neg_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip)
summary(neg_ea_env_prs_fin)

### Model 6.b
## Negative alc exp ~ PAU PRS + Risk PRS + Dep PRS + env predictors + covariates → in African-ancestry children who have not sipped alcohol
neg_aa_env_prs_fin <- lmer(aeq_neg_sum ~ AUDscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum+ interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) +  (1 | FID) + (1 | site_id_l), data = AA_PRS_aeq_full_nosip)
summary(neg_aa_env_prs_fin)

### meta-analysis
neg_prs_env_res_age_fin <- rma(yi = c(2.507e-02,0.04574),sei=c(5.943e-03,0.01700),method="FE")
neg_prs_env_res_age_fin

## Test for Heterogeneity:
## Q(df = 1) = 1.3174, p-val = 0.2511

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0273  0.0056  4.8700  <.0001  0.0163  0.0383  
## pval = 1.116005e-06

neg_prs_env_res_sex_fin <- rma(yi = c(5.768e-02,0.22509),sei=c(9.247e-02,0.25200),method="FE")
neg_prs_env_res_sex_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.3890, p-val = 0.5328

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0775  0.0868  0.8933  0.3717  -0.0926  0.2477 

neg_prs_env_res_marriage_fin <- rma(yi = c(-3.420e-02,-0.03881),sei=c(1.464e-01,0.31635),method="FE")
neg_prs_env_res_marriage_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0002, p-val = 0.9894

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0350  0.1329  -0.2635  0.7921  -0.2954  0.2254   

neg_prs_env_res_income100_fin <- rma(yi = c(-1.515e-01,-0.36503),sei=c(1.102e-01,0.44970),method="FE")
neg_prs_env_res_income100_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.2127, p-val = 0.6447

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.1636  0.1070  -1.5285  0.1264  -0.3734  0.0462    

neg_prs_env_res_income35_fin <- rma(yi = c(-3.926e-01,-0.47317),sei=c(2.149e-01,0.30055),method="FE")
neg_prs_env_res_income35_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0476, p-val = 0.8274

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.4199  0.1748  -2.4018  0.0163  -0.7625  -0.0772   

neg_prs_env_res_edu_fin <- rma(yi = c(9.929e-02,0.13524),sei=c(2.974e-02,0.06298),method="FE")
neg_prs_env_res_edu_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.2664, p-val = 0.6057

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.1058  0.0269  3.9359  <.0001  0.0531  0.1586
## pval = 8.290164e-05

neg_prs_env_res_relig_fin <- rma(yi = c(2.962e-03,-0.32290),sei=c(7.369e-02,0.17968),method="FE")
neg_prs_env_res_relig_fin

## Test for Heterogeneity:
## Q(df = 1) = 2.8155, p-val = 0.0934

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0440  0.0682  -0.6447  0.5191  -0.1776  0.0897    

neg_prs_env_res_religimp_fin <- rma(yi = c(-6.067e-02,0.20987),sei=c(6.188e-02,0.14739),method="FE")
neg_prs_env_res_religimp_fin

## Test for Heterogeneity:
## Q(df = 1) = 2.8643, p-val = 0.0906

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0201  0.0571  -0.3528  0.7242  -0.1320  0.0917

neg_prs_env_res_fathalc_fin <- rma(yi = c(1.595e-01,-0.36271),sei=c(1.515e-01,0.39505),method="FE")
neg_prs_env_res_fathalc_fin

## Test for Heterogeneity:
## Q(df = 1) = 1.5233, p-val = 0.2171

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0925  0.1415  0.6542  0.5130  -0.1847  0.3698 

neg_prs_env_res_mothalc_fin <- rma(yi = c(1.825e-01,-0.60936),sei=c(2.631e-01,0.63080),method="FE")
neg_prs_env_res_mothalc_fin

## Test for Heterogeneity:
## Q(df = 1) = 1.3423, p-val = 0.2466

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0652  0.2428  0.2683  0.7884  -0.4108  0.5411 

neg_prs_env_res_ace_fin <- rma(yi = c(8.291e-02,0.02993),sei=c(2.378e-02,0.04688),method="FE")
neg_prs_env_res_ace_fin

## Test for Heterogeneity:
## Q(df = 1) = 1.0158, p-val = 0.3135

## Model Results:
  
##   estimate      se    zval    pval   ci.lb   ci.ub 
## 0.0721  0.0212  3.3982  0.0007  0.0305  0.1136

neg_prs_env_res_ptu_sum_fin <- rma(yi = c(1.518e-01,0.30651),sei=c(4.770e-02,0.11079),method="FE")
neg_prs_env_res_ptu_sum_fin

## Test for Heterogeneity:
## Q(df = 1) = 1.6451, p-val = 0.1996

## Model Results:
  
##  estimate      se    zval    pval   ci.lb   ci.ub 
## 0.1760  0.0438  4.0170  <.0001  0.0901  0.2619
## pval = 5.893596e-05

neg_prs_env_res_peer_dev_sum <- rma(yi = c(-3.120e-01,-0.02286),sei=c(1.451e-01,0.37888),method="FE")
neg_prs_env_res_peer_dev_sum

## Test for Heterogeneity:
## Q(df = 1) = 0.5079, p-val = 0.4761

## Model Results:
  
##   estimate      se     zval    pval    ci.lb    ci.ub 
## -0.2750  0.1355  -2.0296  0.0424  -0.5406  -0.0094 


neg_prs_env_res_pau_fin <- rma(yi = c(-1.371e-02,-0.02903),sei=c(4.841e-02,0.12880),method="FE")
neg_prs_env_res_pau_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0124, p-val = 0.9113

## Model Results:
  
##   estimate      se     zval    pval    ci.lb   ci.ub 
## -0.0156  0.0453  -0.3444  0.7305  -0.1044  0.0732 

neg_prs_env_res_mdd_fin <- rma(yi = c(6.734e-02,0.05977),sei=c(4.750e-02,0.13202),method="FE")
neg_prs_env_res_mdd_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.0029, p-val = 0.9570

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0665  0.0447  1.4872  0.1370  -0.0211  0.1541

neg_prs_env_res_risk_fin <- rma(yi = c(9.165e-02,0.02324),sei=c(4.750e-02,0.13260),method="FE")
neg_prs_env_res_risk_fin

## Test for Heterogeneity:
## Q(df = 1) = 0.2359, p-val = 0.6272

## Model Results:
  
##   estimate      se    zval    pval    ci.lb   ci.ub 
## 0.0839  0.0447  1.8756  0.0607  -0.0038  0.1715 


### calculate corr b/w relig and relig-imp
# 1st, recode relig to 0/1 (for point-biserial correlation)

aeq_full_nosip$religbin[aeq_full_nosip$relig == -1] <- 0
aeq_full_nosip$religbin[aeq_full_nosip$relig == 1] <- 1

cor.test(aeq_full_nosip$religbin,aeq_full_nosip$religimp)


### see if religiosity is predictive when importance not included

pos_env_fin_noimp <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig  + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env_fin_noimp)

### see if relig importance is MORE predictive when religiosity not included

pos_env_fin_norelig <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env_fin_norelig)

## difference in AEs by sex or race or ethnicity?
t.test(aeq_pos_sum ~ sex, data = aeq_full_nosip)
t.test(aeq_neg_sum ~ sex, data = aeq_full_nosip)
t.test(aeq_pos_sum ~ ethnicity, data = aeq_full_nosip)
t.test(aeq_neg_sum ~ ethnicity, data = aeq_full_nosip)
summary(aov(aeq_pos_sum ~ race, data = aeq_full_nosip))
TukeyHSD(aov(aeq_pos_sum ~ race, data = aeq_full_nosip))
describe(aeq_full_nosip$aeq_pos_sum[aeq_full_nosip$race == "Multiracial"])
describe(aeq_full_nosip$aeq_pos_sum[aeq_full_nosip$race == "Other"])
describe(aeq_full_nosip$aeq_pos_sum[aeq_full_nosip$race == "Black"])
summary(aov(aeq_neg_sum ~ race, data = aeq_full_nosip))
TukeyHSD(aov(aeq_neg_sum ~ race, data = aeq_full_nosip))
describe(aeq_full_nosip$aeq_neg_sum[aeq_full_nosip$race == "Multiracial"])
describe(aeq_full_nosip$aeq_neg_sum[aeq_full_nosip$race == "Other"])
describe(aeq_full_nosip$aeq_neg_sum[aeq_full_nosip$race == "Black"])
describe(aeq_full_nosip$aeq_neg_sum[aeq_full_nosip$race == "White"])
describe(aeq_full_nosip$aeq_neg_sum[aeq_full_nosip$race == "AIAN"])

## histograms
hist(aeq_full_nosip$aeq_pos_sum)
hist(aeq_full_nosip$aeq_neg_sum)

## ggplot version
aeq_scores_nosip <- aeq_full_nosip[,c("subjectkey","aeq_pos_sum","aeq_neg_sum")]
aeq_scores_nosip_long <- pivot_longer(data=aeq_scores_nosip,cols=aeq_pos_sum:aeq_neg_sum,names_to="valence",values_to="score")
aeq_scores_nosip_long$Valence[aeq_scores_nosip_long$valence == "aeq_neg_sum"] <- "Negative AEs"
aeq_scores_nosip_long$Valence[aeq_scores_nosip_long$valence == "aeq_pos_sum"] <- "Positive AEs"

p <- ggplot(aeq_scores_nosip_long, aes(x=score,fill=Valence))+
      geom_histogram(aes(group=Valence), position = "identity", col="black", 
                     binwidth = 1.5, alpha=0.5)+
  scale_fill_manual(values=c("dodgerblue", "firebrick"))+
  scale_x_continuous(name="Score")+
  scale_y_continuous(name="Count")+
  theme_bw()+
  theme(legend.position = "bottom")
p


## corrplot
corrs <- cor(aeq_full_nosip[,c("aeq_section_q01","aeq_section_q02","aeq_section_q03","aeq_section_q04","aeq_section_q05","aeq_section_q06","aeq_section_q07")],use="pairwise.complete.obs")

corrplot(corrs, order="hclust",col=colorRampPalette(c("dodgerblue","white","firebrick"))(200))



## testing to see what variables "knock out" risk taking PRS
summary(lmer(aeq_neg_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ptu_sum + peerdev_sum + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip))
## ACEs is one
summary(lmer(aeq_neg_sum ~ PAUscaled + MDDscaled + Riskscaled + marriage + income + avgedu + relig + religimp + fath_alc_prob + moth_alc_prob + ple_y_ss_total_bad + interview_age + sex + scale(C1) + scale(C2) + scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + (1 | FID) + (1 | site_id_l), data = EA_PRS_aeq_full_nosip))
## peer disapproval & use are more

## look at corr b/w positive and negative AEs
cor.test(aeq_full_nosip$aeq_pos_sum,aeq_full_nosip$aeq_neg_sum)
cor.test(aeq_full_nosip$aeq_pos_sum,aeq_full_nosip$aeq_neg_sum, exact=FALSE,method="spearman") ## try spearman

## testing fath & moth alc problems individually 
## Positive alc exp ~ fath alc problems
pos_env_fath <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + fath_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env_fath)
## Positive alc exp ~ moth alc problems
pos_env_moth <- lmer(aeq_pos_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(pos_env_moth)

### Model 2
## Negative alc exp ~ fath alc problems
neg_env_fath <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + fath_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_env_fath)

neg_env_moth <- lmer(aeq_neg_sum ~ interview_age + sex + race + ethnicity + marriage + income + avgedu + relig + religimp + moth_alc_prob + ple_y_ss_total_bad + ptu_sum + peerdev_sum + (1 | rel_family_id) + (1 | site_id_l), data = aeq_full_nosip)
summary(neg_env_moth)
