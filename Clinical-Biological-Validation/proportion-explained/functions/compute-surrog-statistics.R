# Here we compute three generations of statistics:
# 1. In 1989 Prentice formalized a set of criteria to address treatment-adjusted association between the
#    surrogate and the true endpoint. 
# 2. In 1992, Freedman added Proportion Explained (PE).
# 3. In 2014, Buyse and Molenberghs further added the relative effect (RE) defined as the ratio of the
#    effects of treatment upon the true clinical and the surrogate endpoints. 
#

compute.surrog.statistics <- function(data.in, this_endpoint, comparatorT, reader_type, dataset.descript) {
  for.return <- list()
  write.csv(data.in, path(output_dir, paste0("9a compute.surrog.statistics data.in-", reader_type, "-", this_endpoint, ".csv")), row.names=FALSE)

  # this_endpoint is surrogate; comparatorT is presumed true endpoint
  f0 <- as.formula(paste0(comparatorT, "~", this_endpoint))
  f1 <- as.formula(paste0(this_endpoint, "~", "ARM"))
  f2 <- as.formula(paste0(comparatorT, "~", "ARM"))
  f3 <- as.formula(paste0(comparatorT, "~", "ARM +", this_endpoint))
  
  # Fit linear model with comparatorT as response and this_endpoint as predictor
  fit.lm.summary.compT.vce <- lm(f0, data=data.in)
  # Fit linear model with comparatorT as response and this_endpoint and ARM as predictor
  fit.lm.summary.compT.vce.arm <- lm(f3, data=data.in)
  
  # Get CI for lm model
  CI.fit.lm.summary.compT.vce <- confint(fit.lm.summary.compT.vce)
  CI.fit.lm.summary.compT.vce <- CI.fit.lm.summary.compT.vce %>% as.data.frame() %>% rownames_to_column() %>% dplyr::rename(term=rowname) %>% dplyr::select(term, `2.5 %`, `97.5 %`)
  
  CI.fit.lm.summary.compT.vce.arm <- confint(fit.lm.summary.compT.vce.arm)
  CI.fit.lm.summary.compT.vce.arm <- CI.fit.lm.summary.compT.vce.arm %>% as.data.frame() %>% rownames_to_column() %>% dplyr::rename(term=rowname) %>% dplyr::select(term, `2.5 %`, `97.5 %`)
  
  fit.lm.summary.compT.vce.out <- tidy(summary(fit.lm.summary.compT.vce)) %>% left_join(CI.fit.lm.summary.compT.vce, by="term") %>% 
    dplyr::relocate(p.value, .after=last_col()) %>%
    dplyr::rename(
      Term=term, Estimate=estimate, Std.Error=std.error,
      Statistic=statistic, LCL=`2.5 %`, UCL=`97.5 %`, `p-value`=p.value) %>%
    dplyr::mutate(Approach="Prentice Criteria", model_type="Linear Model without treatment")
  
  fit.lm.summary.compT.vce.arm.out <- tidy(summary(fit.lm.summary.compT.vce.arm)) %>% left_join(CI.fit.lm.summary.compT.vce.arm, by="term") %>% 
    dplyr::relocate(p.value, .after=last_col()) %>%
    dplyr::rename(
      Term=term, Estimate=estimate, Std.Error=std.error,
      Statistic=statistic, LCL=`2.5 %`, UCL=`97.5 %`, `p-value`=p.value) %>%
    dplyr::mutate(Approach="Prentice Criteria", model_type="Linear Model with treatment")
  
  # Fit Seemingly Unrelated Regression (SUR) model
  fit.SUR <- systemfit(list(Surrogate=f1, True=f2), data=data.in, method="SUR")
  fit.SUR.summary.out <- tidy(fit.SUR) %>%
    dplyr::relocate(p.value, .after=last_col()) %>%
    dplyr::rename(
      Term=term, Estimate=estimate, Std.Error=std.error,
      Statistic=statistic, LCL=conf.low, UCL=conf.high, `p-value`=p.value) %>%
    dplyr::mutate(Approach="Prentice Criteria", model_type="Bivariate Regression Model")
  
  # Define function to compute the extensions, and a companion for bootstrap based on a resampled dataset
  compute_PE_RE <- function(data.in) {
    fit.SUR <- systemfit(list(Surrogate=f1, True=f2), data=data.in, method="SUR") # will already have been computed for point estimates
    beta_hat <- fit.SUR$coefficients[4]
    alpha_hat <- fit.SUR$coefficients[2]
    beta_S_hat <- beta_hat - fit.SUR$residCov[1, 2] / fit.SUR$residCov[1, 1] * fit.SUR$coefficients[2]
    gamma <- (fit.SUR$residCov[1, 2]) / (fit.SUR$residCov[1, 1])
    PE <- (beta_hat - beta_S_hat) / beta_hat
    RE <- beta_hat / alpha_hat
    rho <- (fit.SUR$residCov[1, 2]) / (sqrt(fit.SUR$residCov[1, 1] * fit.SUR$residCov[2, 2]))
    return(list(PE=PE, RE=RE, beta_S_hat=beta_S_hat, gamma=gamma, rho=rho))
  }
  
  # Coefficient extraction
  # use the function to compute point estimates...
  compute_PE_RE_results <- compute_PE_RE(data.in)
  PE <- compute_PE_RE_results$PE
  RE <- compute_PE_RE_results$RE
  beta_S_hat <- compute_PE_RE_results$beta_S_hat
  gamma <- compute_PE_RE_results$gamma
  rho <- compute_PE_RE_results$rho
  
  # ...and then use it to perform bootstrap resampling for CIs
  #set.seed(123) # For reproducibility
  if (nrow(data.in) > 15) {
    compute_PE_RE_for_bootstrap <- function(data, indices) {
      resampled_data <- data[indices, ]
      compute_PE_RE_results <- compute_PE_RE(resampled_data)
      return(unlist(compute_PE_RE_results))
    }
    boot_out <- boot::boot(data=data.in, statistic=compute_PE_RE_for_bootstrap, R=1000) # R is the number of bootstrap samples
    # Not well behaved for mock-study data
    # hist(boot_out$t[,1], breaks=100)
    # hist(boot_out$t[,2], breaks=100)
    # Extract bootstrap confidence intervals
    PE_ci <- boot::boot.ci(boot_out, type="bca", index=1)
    RE_ci <- boot::boot.ci(boot_out, type="bca", index=2)
    beta_S_hat_ci <- boot::boot.ci(boot_out, type="bca", index=3)
    gamma_ci <- boot::boot.ci(boot_out, type="bca", index=4)
    rho_ci <- boot::boot.ci(boot_out, type="bca", index=5)
    Freedman <- data.frame(
        Term=c("Proportion Explained"),
        Estimate=c(PE),
        LCL=c(PE_ci$bca[4]),
        UCL=c(PE_ci$bca[5])) %>%
      dplyr::mutate(Approach="Freedman's Extension")

    Buyse <- data.frame(
        Term=c("Relative Efficiency",  "Treatment adjusted assoc btw surrogate & true endpoint"),
        Estimate=c(RE, rho),
        LCL=c(RE_ci$bca[4],  rho_ci$bca[4]),
        UCL=c(RE_ci$bca[5],  rho_ci$bca[5])) %>%
      dplyr::mutate(Approach="Buyse and Molenberghs' Extensions")

  } else {
    Freedman <- data.frame(
        Term=c("Proportion Explained"),
        Estimate=c(PE),
        LCL=c(NA),
        UCL=c(NA)) %>%
      dplyr::mutate(Approach="Freedman's Extension")

    Buyse <- data.frame(
        Term=c("Relative Efficiency", "Treatment adjusted assoc btw surrogate & true endpoint"),
        Estimate=c(RE, rho),
        LCL=c(NA, NA),
        UCL=c(NA, NA)) %>%
      dplyr::mutate(Approach="Buyse and Molenberghs' Extensions")
  }
  
  model.report <- bind_rows(fit.lm.summary.compT.vce.out, fit.lm.summary.compT.vce.arm.out, fit.SUR.summary.out)
  
  # Return the output
  Prentice <- bind_rows(
    model.report %>% dplyr::filter(model_type == "Linear Model without treatment") %>% slice(2) %>% mutate(hypothesis="Test that Surrogate (explanatory) has significant impact on Comparator (response) from simple linear model"),
    model.report %>% dplyr::filter(model_type == "Bivariate Regression Model") %>% slice(2) %>% mutate(hypothesis="Test that Treatment has significant impact on Surrogate from bivariate regression model"),
    model.report %>% dplyr::filter(model_type == "Bivariate Regression Model") %>% slice(4) %>% mutate(hypothesis="Test that Treatment has significant impact on Comparator from bivariate regression model"),
    model.report %>% dplyr::filter(model_type == "Linear Model with treatment") %>% slice(3) %>% mutate(hypothesis="Test that Surrogate has significant impact on Comparator after adjusting for treatment from extended linear model"),
    model.report %>% dplyr::filter(model_type == "Linear Model with treatment") %>% slice(2) %>% mutate(hypothesis="Test that Treatment has significant impact on Comparator after adjusting for Surrogate from extended linear model")
  )
  
  for.return$fit.lm.summary.compT.vce <- fit.lm.summary.compT.vce
  for.return$fit.lm.summary.compT.vce.arm <- fit.lm.summary.compT.vce.arm
  for.return$fit.SUR <- fit.SUR
  for.return$model.report <- model.report
  for.return$altogether <- bind_rows(Prentice, Freedman, Buyse) %>%
      dplyr::mutate(VCE=this_endpoint, comparatorT=comparatorT, data_set=dataset.descript, reader_type=reader_type)

  write.csv(for.return$altogether, path(output_dir, paste0("9b compute.surrog.statistics altogether-", reader_type, "-", this_endpoint, ".csv")), row.names=FALSE)
  return(for.return)
}
