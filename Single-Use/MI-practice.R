# missing responses may be imputed using multiple imputation by a fully conditional specification (FCS) algorithm
# conditional specification model. The imputation models will be based on control group and the baseline stratification variables of 
# celiac serologic status at Visit 1, mild to moderate (Vh:Cd 1.5 to <2.5) versus moderate to severe (Vh:Cd <1.5) histologic injury at baseline, 
# and use of proton pump inhibitor (PPIs) or histamine 2 antagonists (yes or no) at Visit 1

library(readr)
library(tidyverse)
library(mice)
# for.sing <- read_csv("C:/Users/eri7441/OneDrive - Takeda/Documents - MARCS-Validation-Data/Output/Single Timepoint Association/2024-11-22 TAK-062-2001 GC/8 analysis.data.in-ML-BVA.csv")
# Assuming analysis.data.in arising now from formed-merged-data-sets
for.sing <- analysis.data.in


# We exclude VCIEL and Agg_histology from the imputation model because these are functionally related to other endpoints
df <- for.sing %>%
  dplyr::mutate(ARM=sample(c("Placebo", "TAK-062"), replace=TRUE, size=n())) %>%
  # Consider need for SEX, RACE, ETHNICITY 
  dplyr::select(STUDYID, SITE, SUBJID, SEX, RACE, ETHNICITY, pairing, ARM, BVA, SEROSTATUS, HISTINJURY, PPIHIST2ANT, VHCD, IELCOUNT, GISS)
df$SEROSTATUS <- as.factor(df$SEROSTATUS)
df$HISTINJURY <- as.factor(df$HISTINJURY)
df$PPIHIST2ANT <- as.factor(df$PPIHIST2ANT)  
# Separate data into placebo and treatment arms
placebo_data <- subset(df, ARM == "PBO")
treatment_data <- subset(df, ARM == "TRT")
predMat <- make.predictorMatrix(placebo_data)
tictoc::tic() # 304.07 sec elapsed for 1000 iterations
imputed_placebo <- mice(placebo_data, m=10, maxit=50, method=c(rep("", 9), rep("polyreg", 3),  rep('pmm',3)), seed=500, predictorMatrix=predMat, printFlag=FALSE)
imputed_treatment <- mice(treatment_data, m=10, method=c(rep("", 9), rep("polyreg", 3),  rep('pmm',3)), predictorMatrix=predMat,  printFlag=FALSE)
tictoc::toc()
imps <- rbind(imputed_placebo, imputed_treatment)
temp <- complete(imps, 'long', include=TRUE) %>%
  dplyr::mutate(VCIEL = VHCD/sd(VHCD, na.rm=TRUE) - IELCOUNT/sd(IELCOUNT,na.rm=TRUE),
                VHCD.s = (VHCD-mean(VHCD, na.rm=TRUE))/sd(VHCD, na.rm=TRUE),
                IELCOUNT.s = (IELCOUNT-mean(IELCOUNT, na.rm=TRUE))/sd(IELCOUNT, na.rm=TRUE),
                GISS.s = (GISS-mean(GISS, na.rm=TRUE))/sd(GISS, na.rm=TRUE),
                Agg_Histology = sqrt(2)/2*(VHCD.s + IELCOUNT.s),
                Disease_Burden = sqrt(2)/2*(VHCD.s + IELCOUNT.s) + GISS) %>% 
  dplyr::select(-VHCD.s, -IELCOUNT.s, -GISS.s)
new_imps <- as.mids(temp)

complete(new_imps, 1)
result.pearson <- miceadds::micombine.cor(mi.res=new_imps, variables=c(9, 13:18) ) %>% dplyr::filter(variable1=="BVA") %>% dplyr::mutate(Method="Pearson") %>%
  dplyr::select(Method, variable1, variable2, r, lower95, upper95) %>% 
  dplyr::rename(`VCE Endpoint` = variable1, `Comparator` = variable2, Estimate=r, LCI=lower95, UCI=upper95)
result.spearman <- miceadds::micombine.cor(mi.res=new_imps, variables=c(9, 13:18), method="spearman") %>% dplyr::filter(variable1=="BVA") %>% dplyr::mutate(Method="Spearman")%>%
  dplyr::select(Method, variable1, variable2, r, lower95, upper95) %>% 
  dplyr::rename(`VCE Endpoint` = variable1, `Comparator` = variable2, Estimate=r, LCI=lower95, UCI=upper95)
bind_rows(result.pearson, result.spearman) 
#-------------------------

tictoc::tic() # 38.22133 minutes for 10!!
Obuchowski_imps <- list()
for(i in 1:10){ 
Obuchowski_imps[[i]] <- return.Obuchowski(df.in = complete(new_imps, i), this_endpoint = "BVA")
}
Obuchowski_imps <- bind_rows(Obuchowski_imps) 
tictoc::toc()

# See if this works
as.mira(Obuchowski_imps)

apply(matrix(1:length(unique(Obuchowski_imps$Comparator))), 1, function(x) {
  comparator <- unique(Obuchowski_imps$Comparator)[x]
  temp <- Obuchowski_imps %>% dplyr::filter(Comparator==comparator)
  MI.out <- pool.scalar(Q = temp$Estimate, U=temp$variance , n=temp$n, k=1)
  data.frame(Estimate=MI.out$qbar, LCI=MI.out$qbar - qnorm(0.975)*sqrt(MI.out$ubar), UCI=MI.out$qbar + qnorm(0.975)*sqrt(MI.out$ubar)) %>%
               dplyr::mutate(this_endpoint = Obuchowski_imps$this_endpoint[1], Comparator=comparator)
}) %>% bind_rows()


#-------------------------

library(Rcpp)
setwd("C:/ForGit/MARCS/Clinical-Biological-Validation/association/functions")
sourceCpp("ROC-speedup.cpp")

return.Obuchowski2 <- function(df.in, this_endpoint) {
  for.return <- lapply(comparators, function(comparator) {
    temp <- df.in %>%
      dplyr::select(STUDYID, SITE, SUBJID, pairing, !!sym(comparator), !!sym(this_endpoint)) %>%
      dplyr::rename(comparator = !!sym(comparator), this_endpoint = !!sym(this_endpoint)) %>% na.omit()
    
    nrow.temp <- nrow(temp)
    
    if (comparator %in% c("VHCD", "VCIEL")) temp$comparator <- -temp$comparator
    
    sum.phi <- calculate_sum_phi(temp$comparator, temp$this_endpoint, nrow.temp)
    theta.hat <- sum(sum.phi) / (nrow.temp * (nrow.temp - 1))
    
    struct.components <- calculate_struct_components(temp$comparator, temp$this_endpoint, nrow.temp)
    variance <- sum((struct.components - theta.hat)^2) / ((nrow.temp / 2) * (nrow.temp / 2 - 1))
    
    data.frame(
      Comparator = comparator,
      this_endpoint = this_endpoint,
      Estimate = theta.hat,
      n = nrow.temp,
      variance = variance,
      p.value = 1 - pnorm((theta.hat - 0.5) / sqrt(variance)),
      LCL = theta.hat - qnorm(0.975) * sqrt(variance),
      UCL = theta.hat + qnorm(0.975) * sqrt(variance),
      Statistic = "Obuchowski"
    )
  }) %>%
    bind_rows() %>%
    dplyr::mutate(Comparator = factor(Comparator, levels = comparators, labels = sapply(comparators, label_for_doc))) %>%
    dplyr::arrange(Comparator) %>%
    dplyr::select(this_endpoint, Comparator, Statistic, n, everything())
  
  return(for.return)
}

tictoc::tic() # 1.28 sec elapsed
Obuchowski_imps2 <- list()
for(i in 1:10){ 
  Obuchowski_imps2[[i]] <- return.Obuchowski2(df.in = complete(new_imps, i), this_endpoint = "BVA")
}
Obuchowski_imps2 <- bind_rows(Obuchowski_imps2) 
tictoc::toc()


apply(matrix(1:n_distinct(Obuchowski_imps2$Comparator)), 1, function(x) {
  comparator <- unique(Obuchowski_imps2$Comparator)[x]
  temp <- Obuchowski_imps2 %>% dplyr::filter(Comparator==comparator)
  MI.out <- pool.scalar(Q = temp$Estimate, U=temp$variance , n=temp$n, k=1)
  data.frame(Estimate=MI.out$qbar, LCI=MI.out$qbar - qnorm(0.975)*sqrt(MI.out$ubar), UCI=MI.out$qbar + qnorm(0.975)*sqrt(MI.out$ubar)) %>%
    dplyr::mutate(this_endpoint = Obuchowski_imps2$this_endpoint[1], Comparator=comparator)
}) %>% bind_rows()
pool.scalar(Q=Obuchowski_imps2$Estimate, U=Obuchowski_imps2$variance, n = Inf, k = 1, rule = "rubin1987")
