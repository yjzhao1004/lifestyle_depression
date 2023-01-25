setwd("/Users/yujiezhao/Desktop/SEM_1108/")
rm(list = ls())
library(lavaan)
Sem_data0<-read.csv("regre_mergedata_notiv_1129.csv")

##CFA
cfamodel<-'    depression =~ 1*phq1_ins0 + phq2_ins0 + phq3_ins0 + phq4_ins0 
               lifestyle  =~ 1*lifescore
               vol        =~ 1*Left_Pallidum+rh_precentral_volume+lh_precentral_volume+Left_Thalamus_Proper+lh_superiorfrontal_volume+Right_Pallidum+Right_Hippocampus+rh_insula_volume+lh_parahippocampal_volume+Right_Thalamus_Proper+rh_parsorbitalis_volume+Left_Hippocampus+lh_medialorbitofrontal_volume+rh_medialorbitofrontal_volume+lh_lateralorbitofrontal_volume+rh_parahippocampal_volume+lh_parsorbitalis_volume+rh_lateralorbitofrontal_volume+lh_lingual_volume+lh_paracentral_volume
               immu       =~ 1*Glucose + C.reactive.protein + Triglycerides +Glycated.haemoglobin..HbA1c. 
               PRSL       =~ 1*Pt_0.05

               depression~~lifestyle + vol + immu +PRSL
               vol ~~ vol + immu + PRSL
               immu~~PRSL
'

fit <- cfa(cfamodel, data = Sem_data0,std.lv = TRUE)
summary(fit,fit.measures=T)

###SEM 
sem_model_0 <- '
# measurement model
  lifestyle  =~ lifescore
  vol        =~ Left_Pallidum+rh_precentral_volume+lh_precentral_volume+Left_Thalamus_Proper+lh_superiorfrontal_volume+Right_Pallidum+Right_Hippocampus+rh_insula_volume+lh_parahippocampal_volume+Right_Thalamus_Proper+rh_parsorbitalis_volume+Left_Hippocampus+lh_medialorbitofrontal_volume+rh_medialorbitofrontal_volume+lh_lateralorbitofrontal_volume+rh_parahippocampal_volume+lh_parsorbitalis_volume+rh_lateralorbitofrontal_volume+lh_lingual_volume+lh_paracentral_volume
  depression =~ phq1_ins0 + phq2_ins0 + phq3_ins0 + phq4_ins0 
  immu       =~ Glucose + C.reactive.protein + Triglycerides +Glycated.haemoglobin..HbA1c. 
  PRSL       =~ Pt_0.05
  
# structural model
  depression ~ vol + lifestyle  +PRSL +immu 
  lifestyle  ~ PRSL  
  immu       ~ lifestyle + PRSL 
  vol        ~ PRSL + lifestyle+immu
'

fit_model <- sem(sem_model_0, data = Sem_data0, std.lv = TRUE)
X<-summary(fit_model, fit.measures=TRUE,standard=TRUE)



pfdr=list()
pfdr$pvalue<-X$pe$pvalue[c(31:40)]
pfdr$num<-c(1:10)

### FDR correction
pfdr<-as.data.frame(pfdr)
Data = pfdr[order(pfdr$pvalue),]
Data$fdr =
  p.adjust(Data$pvalue,
           method = "fdr")

Data2 = Data[order(Data$num),]
Data2




