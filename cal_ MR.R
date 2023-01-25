rm(list = ls())
library(TwoSampleMR)
library(data.table)
library(RadialMR)

Lifestyle<- fread('/Users/yujiezhao/Desktop/GWAS/GWAS_chr_full_glm_linear.txt',header=T)
Depression<-fread('/Users/yujiezhao/Desktop/GWAS/daner_pgc_mdd_meta_w2_no23andMe_rmUKBB.txt',header=T)
Depression$BETA <- log(Depression$OR) #

Lifestyle$phenotype <- 'exposure' 
Depression$phenotype <- 'outcome' 

Lifestyle<-Lifestyle[Lifestyle$P<1e-7,]
Lifestyle <- format_data(Lifestyle,
                         type ='exposure',
                         snp_col = "ID",
                         beta_col = "BETA",
                         se_col = "SE",
                         effect_allele_col = "ALT",
                         other_allele_col = "REF",
                         pval_col = "P",
                         chr_col = "CHROM",
                         phenotype_col = "phenotype")

Depression<-format_data(Depression,type='outcome',
                        snp_col = "SNP",beta_col = "BETA",
                        se_col = "SE",effect_allele_col = "A1",
                        other_allele_col = "A2",
                        pval_col = "P",
                        chr_col = "CHR",
                        phenotype_col = "phenotype")


Lifestyle0 <- clump_data(Lifestyle,clump_r2=0.01,clump_kb=1000)

MR_function <- function (exposure.data,outcome.data){
  final.data = harmonise_data(exposure.data, exposure.data,action = 3) 
  final.data = final.data[which(!duplicated(final.data)),]
  results = mr(final.data,method_list= c("mr_ivw_mre","mr_weighted_median","mr_weighted_mode","mr_ivw_fe","mr_simple_median"))
  results$SNPs = dim(final.data)[1]
  results$OR = exp(results$b)
  results$LCI = exp(results$b - 1.96*results$se)
  results$UCI = exp(results$b + 1.96*results$se)
  heterogeneity = mr_heterogeneity(final.data) #heterogeneity test
  pleiotropy = mr_pleiotropy_test(final.data)
  results$IVW.Qpval <- heterogeneity$Q_pval[2]
  results$IVW.Q <- heterogeneity$Q[2]
  results$pleiotropy.pval<- pleiotropy$pval#pleiotropy test 1
  results$pleiotropy.intercept <- pleiotropy$egger_intercept
  results$pleiotropy.se <- pleiotropy$se
  return(results)
}

results<-MR_function(Depression,Lifestyle0)

final = harmonise_data(Lifestyle0, Depression,action = 3) 

radial_data<-format_radial(final$beta.exposure,final$beta.outcome,
                           final$se.exposure,final$se.outcome,
                           final$SNP)

ivw.model<-ivw_radial(radial_data,0.05,3,0.0001)
ivw.model$outliers

Lifestyle1<-setdiff(Lifestyle0$SNP,ivw.model$outliers$SNP)
Lifestyle11<-Lifestyle[which(is.element(Lifestyle$SNP,Lifestyle1)),]

final.data = harmonise_data(Lifestyle11, Depression,action = 3)

results0<-MR_function(Lifestyle11,Depression)

rm(list = ls())

res_single <- mr_singlesnp(final,all_method = c("mr_simple_median","mr_weighted_mode","mr_weighted_median","mr_ivw_fe"))
pp<-mr_forest_plot(res_single)

pp$yJoXdr.Lby8N1 + xlab('MR effect size for lifestyle on depression,β')  +
  theme_classic(base_size = 17)


library(ggplot2)
p<-mr_scatter_plot(results[c(2:5),],final.data)
p$o4Vugd.RRR2fS
p$o4Vugd.RRR2fS +
  xlab('SNP effect on depression') + ylab('SNP effect on lifestyle ') +
  theme_classic(base_size = 17)+
  scale_x_continuous(limits= c(0.045, 0.06))

