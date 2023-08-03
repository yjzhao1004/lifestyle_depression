setwd("/Users/yujiezhao/Desktop/Lifestyle_Depression/SEM_20230406")
rm(list = ls())
library(lavaan)
Sem_data0<-read.csv("regre_mergedata_20230408_fullbrain.csv")


###CFA
cfamodel<-'    depression =~ 1*phq1_ins0 + phq2_ins0 + phq3_ins0 + phq4_ins0 
               lifestyle  =~ 1*lifescore
               vol        =~ 1*Left_Pallidum+lh_precentral_volume+Right_Pallidum+rh_precentral_volume+lh_superiorfrontal_volume+Left_Thalamus_Proper+lh_middletemporal_volume+lh_parahippocampal_volume+rh_insula_volume+Left_Amygdala+lh_posteriorcingulate_volume+Right_Thalamus_Proper+lh_insula_volume+rh_middletemporal_volume+rh_parahippocampal_volume+lh_medialorbitofrontal_volume+lh_fusiform_volume+lh_lateraloccipital_volume+rh_medialorbitofrontal_volume+rh_inferiorparietal_volume                                           
               immu       =~ 1*Glucose + C.reactive.protein + Triglycerides +Glycated.haemoglobin..HbA1c. 
               PRSL       =~ 1*PRS

               depression~~lifestyle + vol + immu +PRSL
               vol ~~ vol + immu + PRSL
               immu~~PRSL
'
fit <- cfa(cfamodel, data = Sem_data0,std.lv = TRUE)
summary(fit,fit.measures=T)



###SEM 
'
sem_model_0 <- '
# measurement model
  lifestyle  =~ lifestylescore
  vol        =~ Left_Pallidum+lh_precentral_volume+Right_Pallidum+rh_precentral_volume+lh_superiorfrontal_volume+Left_Thalamus_Proper+lh_middletemporal_volume+lh_parahippocampal_volume+rh_insula_volume+Left_Amygdala+lh_posteriorcingulate_volume+Right_Thalamus_Proper+lh_insula_volume+rh_middletemporal_volume+rh_parahippocampal_volume+lh_medialorbitofrontal_volume+lh_fusiform_volume+lh_lateraloccipital_volume+rh_medialorbitofrontal_volume+rh_inferiorparietal_volume                                           
  depression =~ phq1_ins0 + phq2_ins0 + phq3_ins0 + phq4_ins0 
  immu       =~ C.reactive.protein + Triglycerides + Glycated.haemoglobin..HbA1c.  +  Glucose
  PRSL       =~ PRS
  
# structural model
  depression ~ PRSL+ immu +vol +lifestyle
  lifestyle  ~ PRSL 
  immu       ~ PRSL + lifestyle
  vol        ~ PRSL+lifestyle+immu
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




