rm(list = ls())
library(rms)
library(survival)
library(Hmisc)
library(dplyr)

mergedata_depression_omit_cov<-read.csv("/Users/yujiezhao/Desktop/cox/new_depression_survival_data_addgprinteraction.csv")

##Association of each lifestyle factor with depression risk
res.cox <- coxph(Surv(depression_days,depression_status) ~ healthy_alcohol +healthy_diet + healthy_PA + healthy_smoke + healthy_sleep +healthy_sb +healthy_sc + Sex + Townsend_index + BMI  + Age +Qualifications, data = mergedata_depression_omit_cov)
cox.zph(res.cox)
summary(res.cox)


##Association of lifestyle class with depression risk
mergedata_depression_omit_cov$lifeclassify<-mergedata_depression_omit_cov$lifescore
mergedata_depression_omit_cov$lifeclassify[which(mergedata_depression_omit_cov$lifescore<=2)] = "unfavourable"
mergedata_depression_omit_cov$lifeclassify[which(mergedata_depression_omit_cov$lifescore ==3|mergedata_depression_omit_cov$lifescore==4)] = "intermediate"
mergedata_depression_omit_cov$lifeclassify[which(mergedata_depression_omit_cov$lifescore>=5)] = "favourable"
mergedata_depression_omit_cov$lifeclassify=factor(mergedata_depression_omit_cov$lifeclassify,levels=c("unfavourable","intermediate","favourable"))

res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify)+ Sex + Townsend_index + BMI  + Age +Qualifications, data = mergedata_depression_omit_cov)
cox.zph(res.cox)
summary(res.cox)


##Association of lifestyle score with depression risk
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifescore)+ Sex + Townsend_index + BMI  + Age +Qualifications, data = mergedata_depression_omit_cov)
cox.zph(res.cox)
summary(res.cox)



##Add PRS interaction
library(data.table)
PRS<- fread('/Users/yujiezhao/Desktop/cox/depression_PRS.all_score',header = TRUE)
Quan<-quantile(PRS$PRS)

PRS$class<-NA
PRS$class[which(PRS$PRS<Quan[2])]<-1
PRS$class[which(PRS$PRS>Quan[4])]<-3
PRS$class[which(PRS$PRS>=Quan[2]&PRS$PRS<=Quan[4])]<-2

mergedata_depression_omit_cov_interaction<-merge(mergedata_depression_omit_cov,PRS,by = "eid")

mergedata_depression_omit_cov_interaction$interactiongroup<-NA
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==1&mergedata_depression_omit_cov_interaction$lifeclassify=="unfavourable")]<-7
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==1&mergedata_depression_omit_cov_interaction$lifeclassify=="intermediate")]<-8
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==1&mergedata_depression_omit_cov_interaction$lifeclassify=="favourable")]<-9
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==2&mergedata_depression_omit_cov_interaction$lifeclassify=="unfavourable")]<-4
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==2&mergedata_depression_omit_cov_interaction$lifeclassify=="intermediate")]<-5
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==2&mergedata_depression_omit_cov_interaction$lifeclassify=="favourable")]<-6
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==3&mergedata_depression_omit_cov_interaction$lifeclassify=="unfavourable")]<-1
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==3&mergedata_depression_omit_cov_interaction$lifeclassify=="intermediate")]<-2
mergedata_depression_omit_cov_interaction$interactiongroup[which(mergedata_depression_omit_cov_interaction$class==3&mergedata_depression_omit_cov_interaction$lifeclassify=="favourable")]<-3

res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(interactiongroup) + Sex + Townsend_index + BMI  + Age +Qualifications, data = mergedata_depression_omit_cov_interaction)
cox.zph(res.cox)
summary(res.cox)



