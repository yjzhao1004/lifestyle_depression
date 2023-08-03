rm(list = ls())
setwd("/Users/yujiezhao/Desktop/Lifestyle_Depression/Cox/")
library(rms)
library(survival)
library(mice)
library(mitools)
library(Hmisc)
library(dplyr)
library(data.table)

load("/Users/yujiezhao/Desktop/Lifestyle_Depression/Cox/20230403/depression_sur_lifestyle_20230407.RData")

###Association of each lifestyle factor with risk of depression
res.cox <- coxph(Surv(depression_days,depression_status) ~ healthy_alcohol +healthy_diet + healthy_PA + healthy_smoke + healthy_sleep +healthy_sb +healthy_sc + Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle)
cox.zph(res.cox)
summary(res.cox)


###results output
lifestyle_cox = list()
j=1
for (i in c(1:7))
{
  lifestyle_cox$variable[j]<-names(depression_sur_lifestyle)[i+3]
  lifestyle_cox$outcome[j]<-names(depression_sur_lifestyle)[2]
  lifestyle_cox$HR[j]<-X$conf.int[i]
  lifestyle_cox$lowerCI[j]<-X$conf.int[i,3]
  lifestyle_cox$upperCI[j]<-X$conf.int[i,4]
  lifestyle_cox$num[j]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
  ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle[,i+3]==1)]
  lifestyle_cox$num2[j]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
  lifestyle_cox$hrci[j]<-paste(round(X$conf.int[i,1],2),"(",round(X$conf.int[i,3],2),",",round(X$conf.int[i,4],2),")",sep = "",collapse = "")
  lifestyle_cox$pvalue[j]<-X$coefficients[i,5]
  j = j+1
}
lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_factor_20230408.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)



##Association of lifestyle classes with risk of depression
depression_sur_lifestyle$lifeclassify<-depression_sur_lifestyle$lifestylescore
depression_sur_lifestyle$lifeclassify[which(depression_sur_lifestyle$lifestylescore<=1)] = 1
depression_sur_lifestyle$lifeclassify[which(depression_sur_lifestyle$lifestylescore ==3|depression_sur_lifestyle$lifestylescore==4|depression_sur_lifestyle$lifestylescore==2)] = 2
depression_sur_lifestyle$lifeclassify[which(depression_sur_lifestyle$lifestylescore>=5)] = 3
depression_sur_lifestyle$lifeclassify=factor(depression_sur_lifestyle$lifeclassify,levels=c(1,2,3))
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify,levels = c(3,2,1))+ Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle)
cox.zph(res.cox)
X<-summary(res.cox)
X

##results output
lifestyle_cox = list()
t=1
i=17
lifestyle_cox$variable[t]<-"favourable"
lifestyle_cox$outcome[t]<-"depression"
lifestyle_cox$HR[t]<-1
lifestyle_cox$lowerCI[t]<-" "
lifestyle_cox$upperCI[t]<-" "
lifestyle_cox$num[t]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle[,i]=="1")]
lifestyle_cox$num2[t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[t]<-" "
lifestyle_cox$pvalue[t]<-" "

zz=1
z=1
lifestyle_cox$variable[z+t]<-"intermediate"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle[,i]=="2")]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

zz = zz+1
z = z+1    
lifestyle_cox$variable[z+t]<-"unfavourable"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle[,i]=="3")]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_class_20230421.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)



###Association of lifestyle score with risk of depression
res.cox <- coxph(Surv(depression_days,depression_status) ~ lifestylescore + Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle)
cox.zph(res.cox)
X<-summary(res.cox)
X
X$coefficients

res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifestylescore)+ Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle)
cox.zph(res.cox)
X<-summary(res.cox)
X

##results output
lifestylefactor_cox=list()
j=1
lifestylefactor_cox$variable[j]<-"0"
lifestylefactor_cox$outcome[j]<-"depression"
lifestylefactor_cox$num_total[j]<-X$n
lifestylefactor_cox$num_events[j]<-X$nevent
ss<-depression_sur_lifestyle$depression_status[which((depression_sur_lifestyle$lifestylescore==0))]
lifestylefactor_cox$num[j]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
lifestylefactor_cox$HR[j]<-1
lifestylefactor_cox$lowerCI[j]<-" "
lifestylefactor_cox$upperCI[j]<-" "
lifestylefactor_cox$pvalue[j]<-" "

rm(ss)

for (j in c(1:7))
{
  lifestylefactor_cox$variable[j+1]<-as.character(j)
  lifestylefactor_cox$outcome[j+1]<-"depression"
  lifestylefactor_cox$num_total[j+1]<-X$n
  lifestylefactor_cox$num_events[j+1]<-X$nevent
  ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle$lifestylescore==j)]
  lifestylefactor_cox$num[j+1]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
  lifestylefactor_cox$HR[j+1]<-X$conf.int[j]
  lifestylefactor_cox$lowerCI[j+1]<-X$conf.int[j,3]
  lifestylefactor_cox$upperCI[j+1]<-X$conf.int[j,4]
  
  lifestylefactor_cox$hrci[j+1]<-paste(round(X$conf.int[j,1],2),"(",round(X$conf.int[j,3],2),",",round(X$conf.int[j,4],2),")",sep = "",collapse = "")
  lifestylefactor_cox$pvalue[j+1]<-X$coefficients[j,5]
  
  rm(ss)
}

lifestylefactor_cox<-as.data.frame(lifestylefactor_cox)
write.csv(lifestylefactor_cox,file = "lifestylescore_cox_20230408.csv")


depression_sur_lifestyle$lifeclassify2<-depression_sur_lifestyle$lifestylescore
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore<=1)] = "0-1"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore==2)] = "2"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore ==3)] = "3"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore ==4)] = "4"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore ==5)] = "5"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore ==6)] = "6"
depression_sur_lifestyle$lifeclassify2[which(depression_sur_lifestyle$lifestylescore ==7)] = "7"

res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify2)+ Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle)
cox.zph(res.cox)
X<-summary(res.cox)
X

lifestylefactor_cox=list()
j=1
lifestylefactor_cox$variable[j]<-"0-1"
lifestylefactor_cox$outcome[j]<-"depression"
lifestylefactor_cox$num_total[j]<-X$n
lifestylefactor_cox$num_events[j]<-X$nevent
ss<-depression_sur_lifestyle$depression_status[which((depression_sur_lifestyle$lifeclassify2=="0-1"))]
lifestylefactor_cox$num[j]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
lifestylefactor_cox$HR[j]<-1
lifestylefactor_cox$lowerCI[j]<-" "
lifestylefactor_cox$upperCI[j]<-" "
lifestylefactor_cox$pvalue[j]<-" "
rm(ss)

for (j in c(1:6))
{
  lifestylefactor_cox$variable[j+1]<-as.character(j+1)
  lifestylefactor_cox$outcome[j+1]<-"depression"
  lifestylefactor_cox$num_total[j+1]<-X$n
  lifestylefactor_cox$num_events[j+1]<-X$nevent
  ss<-depression_sur_lifestyle$depression_status[which(depression_sur_lifestyle$lifeclassify2==as.character(j+1))]
  lifestylefactor_cox$num[j+1]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
  lifestylefactor_cox$HR[j+1]<-X$conf.int[j]
  lifestylefactor_cox$lowerCI[j+1]<-X$conf.int[j,3]
  lifestylefactor_cox$upperCI[j+1]<-X$conf.int[j,4]
  lifestylefactor_cox$hrci[j+1]<-paste(round(X$conf.int[j,1],2),"(",round(X$conf.int[j,3],2),",",round(X$conf.int[j,4],2),")",sep = "",collapse = "")
  lifestylefactor_cox$pvalue[j+1]<-X$coefficients[j,5]
  rm(ss)
}

lifestylefactor_cox<-as.data.frame(lifestylefactor_cox)
write.csv(lifestylefactor_cox,file = "lifestylescore_0-1_cox_20230408.csv")

write.csv(depression_sur_lifestyle,file = "depression_sur_lifestyle_class_20230408.csv")


####ADD PRS INTERACTION
rm(lifestyle_cox,lifestylefactor_cox,res.cox,X)
library(data.table)
setwd("/Users/yujiezhao/Desktop/Lifestyle_Depression/Cox/20230403/PRS")
PRS<- fread('depression_PRS.all_score',header = TRUE)
names(PRS)[1]<-"eid"
PRS<-PRS[,c(1,13)]
names(PRS)[2]<-"PRS"
Quan<-quantile(PRS$PRS)

PRS$class<-NA
PRS$class[which(PRS$PRS<Quan[2])]<-1
PRS$class[which(PRS$PRS>Quan[4])]<-3
PRS$class[which(PRS$PRS>=Quan[2]&PRS$PRS<=Quan[4])]<-2

depression_sur_lifestyle_PRS<-merge(depression_sur_lifestyle,PRS,by = "eid")

depression_sur_lifestyle_PRS$interactiongroup<-NA
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==1&depression_sur_lifestyle_PRS$lifeclassify=="1")]<-7
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==1&depression_sur_lifestyle_PRS$lifeclassify=="2")]<-8
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==1&depression_sur_lifestyle_PRS$lifeclassify=="3")]<-9
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==2&depression_sur_lifestyle_PRS$lifeclassify=="1")]<-4
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==2&depression_sur_lifestyle_PRS$lifeclassify=="2")]<-5
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==2&depression_sur_lifestyle_PRS$lifeclassify=="3")]<-6
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==3&depression_sur_lifestyle_PRS$lifeclassify=="1")]<-1
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==3&depression_sur_lifestyle_PRS$lifeclassify=="2")]<-2
depression_sur_lifestyle_PRS$interactiongroup[which(depression_sur_lifestyle_PRS$class==3&depression_sur_lifestyle_PRS$lifeclassify=="3")]<-3

depression_sur_lifestyle_PRS$lifeclassify<-as.numeric(depression_sur_lifestyle_PRS$lifeclassify)


res.cox <- coxph(Surv(depression_days,depression_status) ~ lifeclassify*class + Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle_PRS)
cox.zph(res.cox)
X<-summary(res.cox)
X
save(X,file = "lifeclassify*class_cox_20230410.RData")


###PRS_lifestyle group interaction
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(interactiongroup, level = c(1:9)) + Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle_PRS)
cox.zph(res.cox)
X<-summary(res.cox)
X

##results output
lifestylefactor_cox=list()
j=1
lifestylefactor_cox$variable[j]<-as.character(j)
lifestylefactor_cox$outcome[j]<-"depression"
lifestylefactor_cox$num_total[j]<-X$n
lifestylefactor_cox$num_events[j]<-X$nevent
ss<-depression_sur_lifestyle_PRS$depression_status[which((depression_sur_lifestyle_PRS$interactiongroup==1))]
lifestylefactor_cox$num[j]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
lifestylefactor_cox$HR[j]<-1
lifestylefactor_cox$lowerCI[j]<-1
lifestylefactor_cox$upperCI[j]<-1
lifestylefactor_cox$pvalue[j]<-" "
rm(ss)

for (j in c(1:8))
{
  lifestylefactor_cox$variable[j+1]<-as.character(j+1)
  lifestylefactor_cox$outcome[j+1]<-"depression"
  lifestylefactor_cox$num_total[j+1]<-X$n
  lifestylefactor_cox$num_events[j+1]<-X$nevent
  ss<-depression_sur_lifestyle_PRS$depression_status[which(depression_sur_lifestyle_PRS$interactiongroup==j+1)]
  lifestylefactor_cox$num[j+1]<-as.character(paste(length(ss[which(ss==1)]),"/",length(ss)))
  lifestylefactor_cox$HR[j+1]<-X$conf.int[j]
  lifestylefactor_cox$lowerCI[j+1]<-X$conf.int[j,3]
  lifestylefactor_cox$upperCI[j+1]<-X$conf.int[j,4]
  lifestylefactor_cox$hrci[j+1]<-paste(round(X$conf.int[j,1],2),"(",round(X$conf.int[j,3],2),",",round(X$conf.int[j,4],2),")",sep = "",collapse = "")
  lifestylefactor_cox$pvalue[j+1]<-X$coefficients[j,5]
  rm(ss)
}

lifestylefactor_cox<-as.data.frame(lifestylefactor_cox)
write.csv(lifestylefactor_cox,file = "lifestyle_PRS_cox_20230424.csv")

write.csv(depression_sur_lifestyle_PRS,file = "depression_sur_lifestyle_PRS_20230411.csv")
write.csv(depression_sur_lifestyle,file = "depression_sur_lifestyle_20230411.csv")


####Association of PRS class with risk of depression
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(class, level = c(3,2,1)) + Sex + Townsend_index + BMI  + Age +Qualifications, data = depression_sur_lifestyle_PRS)
cox.zph(res.cox)
X<-summary(res.cox)
X

##results output
lifestyle_cox = list()
t=1
i=20
lifestyle_cox$variable[t]<-"High_PRS"
lifestyle_cox$outcome[t]<-"depression"
lifestyle_cox$HR[t]<-1
lifestyle_cox$lowerCI[t]<-" "
lifestyle_cox$upperCI[t]<-" "
lifestyle_cox$num[t]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle_PRS$depression_status[which(depression_sur_lifestyle_PRS[,i]==3)]
lifestyle_cox$num2[t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[t]<-" "
lifestyle_cox$pvalue[t]<-" "

zz=1
z=1
lifestyle_cox$variable[z+t]<-"intermediate_PRS"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle_PRS$depression_status[which(depression_sur_lifestyle_PRS[,i]==2)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

zz = zz+1
z = z+1    
lifestyle_cox$variable[z+t]<-"Low_PRS"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-depression_sur_lifestyle_PRS$depression_status[which(depression_sur_lifestyle_PRS[,i]==1)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_PRSclass_20230408.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)


###Stratified analyses_PRS group 
highprs<-depression_sur_lifestyle_PRS[which(depression_sur_lifestyle_PRS$class==3),]
intermediateprs<-depression_sur_lifestyle_PRS[which(depression_sur_lifestyle_PRS$class==2),]
lowprs<-depression_sur_lifestyle_PRS[which(depression_sur_lifestyle_PRS$class==1),]

###High PRS
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify,level = c(1,2,3)) + Sex + Townsend_index + BMI  + Age +Qualifications, data = highprs)
cox.zph(res.cox)
X<-summary(res.cox)
X

##Results output
lifestyle_cox = list()
t=1
i=17
lifestyle_cox$variable[t]<-"unfavourable"
lifestyle_cox$outcome[t]<-"depression"

lifestyle_cox$HR[t]<-1
lifestyle_cox$lowerCI[t]<-" "
lifestyle_cox$upperCI[t]<-" "
lifestyle_cox$num[t]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-highprs$depression_status[which(highprs[,i]==1)]
lifestyle_cox$num2[t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[t]<-" "
lifestyle_cox$pvalue[t]<-" "

zz=1
z=1
lifestyle_cox$variable[z+t]<-"intermediate"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-highprs$depression_status[which(highprs[,i]==2)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

zz = zz+1
z = z+1    
lifestyle_cox$variable[z+t]<-"favourable"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-highprs$depression_status[which(highprs[,i]==3)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_highprslifestyleclass_20230408.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)


###Intermediate PRS
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify,level = c(1,2,3)) + Sex + Townsend_index + BMI  + Age +Qualifications, data =intermediateprs)
cox.zph(res.cox)
X<-summary(res.cox)
X

##Results output
lifestyle_cox = list()
t=1
i=17
lifestyle_cox$variable[t]<-"unfavourable"
lifestyle_cox$outcome[t]<-"depression"
lifestyle_cox$HR[t]<-1
lifestyle_cox$lowerCI[t]<-" "
lifestyle_cox$upperCI[t]<-" "
lifestyle_cox$num[t]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-intermediateprs$depression_status[which(intermediateprs[,i]==1)]
lifestyle_cox$num2[t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[t]<-" "
lifestyle_cox$pvalue[t]<-" "

zz=1
z=1
lifestyle_cox$variable[z+t]<-"intermediate"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-intermediateprs$depression_status[which(intermediateprs[,i]==2)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

zz = zz+1
z = z+1    
lifestyle_cox$variable[z+t]<-"favourable"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-intermediateprs$depression_status[which(intermediateprs[,i]==3)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]
lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_intermediateprslifestyleclass_20230408.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)

###Low PRS
res.cox <- coxph(Surv(depression_days,depression_status) ~ factor(lifeclassify,level = c(1,2,3)) + Sex + Townsend_index + BMI  + Age +Qualifications, data =lowprs)
cox.zph(res.cox)
X<-summary(res.cox)
X

##Results output
lifestyle_cox = list()
t=1
i=17
lifestyle_cox$variable[t]<-"unfavourable"
lifestyle_cox$outcome[t]<-"depression"
lifestyle_cox$HR[t]<-1
lifestyle_cox$lowerCI[t]<-" "
lifestyle_cox$upperCI[t]<-" "
lifestyle_cox$num[t]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-lowprs$depression_status[which(lowprs[,i]==1)]
lifestyle_cox$num2[t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[t]<-" "
lifestyle_cox$pvalue[t]<-" "

zz=1
z=1
lifestyle_cox$variable[z+t]<-"intermediate"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-lowprs$depression_status[which(lowprs[,i]==2)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]

zz = zz+1
z = z+1    
lifestyle_cox$variable[z+t]<-"favourable"
lifestyle_cox$outcome[z+t]<-"depression"
lifestyle_cox$HR[z+t]<-X$conf.int[zz,1]
lifestyle_cox$lowerCI[z+t]<-X$conf.int[zz,3]
lifestyle_cox$upperCI[z+t]<-X$conf.int[zz,4]
lifestyle_cox$num[t+z]<-paste(X$nevent,"/",X$n,sep = "",collapse = "")
ss<-lowprs$depression_status[which(lowprs[,i]==3)]
lifestyle_cox$num2[z+t]<-paste(length(ss[which(ss==1)]),"/",length(ss),sep = "",collapse = "")
lifestyle_cox$hrci[z+t]<-paste(round(X$conf.int[zz,1],2),"(",round(X$conf.int[zz,3],2),",",round(X$conf.int[zz,4],2),")",sep = "",collapse = "")
lifestyle_cox$pvalue[z+t]<-X$coefficients[zz,5]
lifestyle_cox<-as.data.frame(lifestyle_cox)
filename<-paste("lifestyle_cox_lowprslifestyleclass_20230408.csv",sep = "", collapse = "")
write.csv(lifestyle_cox,file = filename)
