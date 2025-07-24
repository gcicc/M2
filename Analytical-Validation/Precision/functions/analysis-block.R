# analysis block for precision

main_analysis <- function(analysis.data.in, reader_type, this_endpoint) {

  # A function to create dummy data
  simulate_data <- function(n_subjects = 30, overall.mean = 1.5, n_traversals = 2, n_retests = 2,
                            sd_subject = 0.7499632, sd_traversal = 0.1379959, sd_interaction = 0.1318115, sd_error = .1) {
    
    # Create empty data frame to store results
    data <- data.frame(
      Subject = rep(1:n_subjects, each = n_traversals * n_retests),
      Traversal = rep(rep(1:n_traversals, each = n_retests), times = n_subjects),
      Retest = rep(1:n_retests, times = n_subjects * n_traversals)
    )
    
    # Simulate subject random effects
    data$SubjectEffect <- rnorm(n_subjects, mean = 0, sd = sd_subject)[data$Subject]
    
    # Simulate retest random effects
    data$TraversalEffect <- rnorm(n_traversals, mean = 0, sd = sd_traversal)[data$Traversal]
    
    # Simulate interaction effects
    interaction_matrix <- matrix(rnorm(n_subjects * n_traversals, mean = 0, sd = sd_interaction), 
                                 nrow = n_subjects, ncol = n_traversals)
    data$InteractionEffect <- interaction_matrix[cbind(data$Subject, data$Traversal)]
    
    # Simulate intra-reader random error
    data$Error <- rnorm(nrow(data), mean = 0, sd = sd_error)
    
    # Calculate the response variable
    data$Response <- data$SubjectEffect + data$TraversalEffect + data$InteractionEffect + data$Error + overall.mean
    data$Response <- ifelse(data$Response < 0, 0, data$Response)
    data$Response <- ifelse(data$Response > 3, 3, data$Response)
    
    return(data)
  }
  
  VCA.total.analytic.variation <- function(VCA.fit = vca.anova.fit, VCA.data.in = simulated_data, alpha = .05) {
    
    # Get the number of unique subjects
    n <- length(unique(VCA.data.in$Subject))
    S <- length(unique(VCA.data.in$Traversal))
    J <- length(unique(VCA.data.in$Retest))
    # Extract the ANOVA table
    AOV <- data.frame(VCA.fit)
    AOV$source <- rownames(AOV)
    
    # Calculate the variance components and other necessary metrics - page 28
    for.RDC <- data.frame(
      VC_Subject = AOV[AOV$source == "Subject", ]$SD^2,
      # This turns out to be zero in some cases
      VC_Traversal = AOV[AOV$source == "Traversal", ]$SD^2,
      VC_SubjectTraversal = AOV[AOV$source == "Subject:Traversal", ]$SD^2,
      VC_error = AOV[AOV$source == "error", ]$SD^2
    ) %>%
      dplyr::mutate(
        total_analytic_variation = VC_Traversal + VC_SubjectTraversal + VC_error,
        # RDC is defined here under the assumption of normality as 1.96 times the SD of a difference
        # between two measurements yisj and yis0 j0 taken on the same case i but at different site s and s0. The SD
        # is equal to square root of two times the sum of all the variance components except for 2 , the
        # random case effects variance.
        RDC = 2.77 * sqrt(total_analytic_variation)) %>%
      dplyr::mutate(
        n = n, J = J, S = S,
        k_delta = 1 / (n * S),
        k_gamma_delta = (n - 1) / (n * J),
        k_epsilon = (J - 1) / J) %>% 
      dplyr::mutate(
        # Mean squares
        M_delta = AOV[AOV$source == "Traversal", ]$SS/AOV[AOV$source == "Traversal", ]$DF,
        M_gamma_delta = AOV[AOV$source == "Subject:Traversal", ]$SS/AOV[AOV$source == "Subject:Traversal", ]$DF,      
        M_epsilon = AOV[AOV$source == "error", ]$SS/AOV[AOV$source == "error", ]$DF) %>% 
      dplyr::mutate(
        # Coefficients
        p_delta = 1 - 1 / qf(p = 1 - .025, df1 = AOV[AOV$source == "Traversal", ]$DF, df2 = Inf),
        q_delta = 1 / qf(p = .025, df1 = AOV[AOV$source == "Traversal", ]$DF, df2 = Inf) - 1,
        p_gamma_delta = 1 - 1 / qf(p = 1 - .025, df1 = AOV[AOV$source == "Subject:Traversal", ]$DF, df2 = Inf),
        q_gamma_delta = 1 / qf(p = .025, df1 = AOV[AOV$source == "Subject:Traversal", ]$DF, df2 = Inf) - 1,
        p_epsilon = 1 - 1 / qf(p = 1 - .025, df1 = AOV[AOV$source == "error", ]$DF, df2 = Inf),
        q_epsilon = 1 / qf(p = .025, df1 = AOV[AOV$source == "error", ]$DF, df2 = Inf) - 1) %>%
      dplyr::mutate(
        # An unbiased moments-based estimate of RDC may be calculated from the sums of squares output
        # of the ANOVA model as
        V.tilda = k_delta*M_delta + k_gamma_delta*M_gamma_delta + k_epsilon*M_epsilon,
        RCD.tilda = 2.77 * sqrt(V.tilda),
        RCD.LCL = 2.77 * ((V.tilda - sqrt((p_delta * k_delta * M_delta)^2 +
                                            (p_gamma_delta * k_gamma_delta * M_gamma_delta)^2 +
                                            (p_epsilon * k_epsilon * M_epsilon)^2)))^.5,
        RCD.UCL = 2.77 * ((V.tilda + sqrt((q_delta * k_delta * M_delta)^2 +
                                            (q_gamma_delta * k_gamma_delta * M_gamma_delta)^2 +
                                            (q_epsilon * k_epsilon * M_epsilon)^2)))^.5
      ) %>%
      dplyr::select(total_analytic_variation, V.tilda, RDC, RCD.tilda, RCD.LCL, RCD.UCL)
    
    # Calculate repeatability coefficient - page 32
    # The resulting confidence interval does not contain the estimate of RC!!!
    for.RC <- data.frame(RC.tilda = 2.77*AOV$SD[5]) %>% 
      dplyr::mutate(   
        RC.LCL = 2.77*sqrt(n*(S-1)*AOV$MS[5]/qchisq(p = 1 - .025, df= n*(S-1))),
        RC.UCL = 2.77*sqrt(n*(S-1)*AOV$MS[5]/qchisq(p = .025, df= n*(S-1)))
      )
    
    # Combine results into a single data frame
    result <- data.frame(Metric = c("Total Analytic Variability", "Reproducibility Coefficient", "Repeatability Coefficient"),
                         Estimate = c(for.RDC$V.tilda, for.RDC$RCD.tilda, for.RC$RC.tilda),
                         LCL = c((for.RDC$RCD.LCL / 2.77)^2, for.RDC$RCD.LCL, for.RC$RC.LCL),
                         UCL = c((for.RDC$RCD.UCL / 2.77)^2, for.RDC$RCD.UCL, for.RC$RC.UCL)) %>%
      dplyr::mutate(length = UCL - LCL)
    
    return(result)
  }
  get.MDD <- function(VCA.fit) {
    
    # Determine the individual variances from the model (e.g., variance due to subjects, test-retest, re-read, interaction, and residual).
    # Calculate the pooled variance by summing these individual variances.
    # Use the pooled variance in the MDD formula to estimate the minimum detectable difference.
    # Use this formula to match update to protocol
    # alpha = 0.025, 
    (qnorm(1-.025)+qnorm(1-.1)) *sqrt(2*sum(VCA.fit$aov.tab$VC)/REML.fit$Nobs)
  }
  
  
  
  # temp <- analysis.data.in %>% arrange(SUBJID) %>% dplyr::filter( VISIT == "VISIT 2 - Week -4") %>% 
  #   dplyr::mutate(key=paste(STUDYID, SITE, SUBJID)) %>% 
  #   group_by(key)%>% mutate(keycount=n())
  # temp <- temp %>% dplyr::filter(keycount==2)
  
  
  #' # Findings: under current conditions we are likely to encounter very wide confidence intervals for TAV and RDC
  #' # Under current conditions
  #' apply(matrix(1:100), 1, function(x){
  #'   
  #' 
  #' simulated_data <- simulate_data() %>% dplyr::rename(BVA = Response)
  #' vca.anova.fit <- anovaVCA(formula.1, simulated_data)$aov.tab
  #' for.return <- list(vca.anova.fit = vca.anova.fit)
  #' REML.fit <- fitVCA(formula.1, simulated_data, method = "REML")
  #' for.return$REML.fit <- REML.fit
  #' #' Raunig DL, McShane LM, Pennello G, Gatsonis C, Carson PL, Voyvodic JT, Wahl RL, Kurland BF, Schwarz AJ, Gönen M, Zahlmann G. 
  #' #' Quantitative imaging biomarkers: a review of statistical methods for technical performance assessment. 
  #' #' Statistical methods in medical research. 2015 Feb;24(1):27-67.
  #' 
  #' VCA.TAV <- VCA.total.analytic.variation(VCA.fit = vca.anova.fit, VCA.data.in = simulated_data, alpha = .05) 
  #' VCA.TAV <- VCA.TAV %>% mutate(sim=x)
  #' VCA.TAV
  #' }) %>% bind_rows() -> holdit
  #' 
  #' holdit %>% ggplot(aes(x=Estimate)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit %>% ggplot(aes(x=LCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit %>% ggplot(aes(x=UCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit %>% ggplot(aes(x=length)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' 
  #' # Adding a traversal...
  #' apply(matrix(1:100), 1, function(x){
  #'   
  #'   
  #'   simulated_data <- simulate_data(n_traversals=3) %>% dplyr::rename(BVA = Response)
  #'   vca.anova.fit <- anovaVCA(formula.1, simulated_data)$aov.tab
  #'   for.return <- list(vca.anova.fit = vca.anova.fit)
  #'   REML.fit <- fitVCA(formula.1, simulated_data, method = "REML")
  #'   for.return$REML.fit <- REML.fit
  #'   #' Raunig DL, McShane LM, Pennello G, Gatsonis C, Carson PL, Voyvodic JT, Wahl RL, Kurland BF, Schwarz AJ, Gönen M, Zahlmann G. 
  #'   #' Quantitative imaging biomarkers: a review of statistical methods for technical performance assessment. 
  #'   #' Statistical methods in medical research. 2015 Feb;24(1):27-67.
  #'   
  #'   VCA.TAV <- VCA.total.analytic.variation(VCA.fit = vca.anova.fit, VCA.data.in = simulated_data, alpha = .05) 
  #'   VCA.TAV <- VCA.TAV %>% mutate(sim=x)
  #'   VCA.TAV
  #' }) %>% bind_rows() -> holdit2
  #' 
  #' holdit2 %>% ggplot(aes(x=Estimate)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit2 %>% ggplot(aes(x=LCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit2 %>% ggplot(aes(x=UCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit2 %>% ggplot(aes(x=length)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' 
  #' # Adding a patients
  #' apply(matrix(1:100), 1, function(x){
  #'   
  #'   simulated_data <- simulate_data(n_subjects=100) %>% dplyr::rename(BVA = Response)
  #'   vca.anova.fit <- anovaVCA(formula.1, simulated_data)$aov.tab
  #'   for.return <- list(vca.anova.fit = vca.anova.fit)
  #'   REML.fit <- fitVCA(formula.1, simulated_data, method = "REML")
  #'   for.return$REML.fit <- REML.fit
  #'   #' Raunig DL, McShane LM, Pennello G, Gatsonis C, Carson PL, Voyvodic JT, Wahl RL, Kurland BF, Schwarz AJ, Gönen M, Zahlmann G. 
  #'   #' Quantitative imaging biomarkers: a review of statistical methods for technical performance assessment. 
  #'   #' Statistical methods in medical research. 2015 Feb;24(1):27-67.
  #'   
  #'   VCA.TAV <- VCA.total.analytic.variation(VCA.fit = vca.anova.fit, VCA.data.in = simulated_data, alpha = .05) 
  #'   VCA.TAV <- VCA.TAV %>% mutate(sim=x)
  #'   VCA.TAV
  #' }) %>% bind_rows() -> holdit3
  #' 
  #' holdit3 %>% ggplot(aes(x=Estimate)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit3 %>% ggplot(aes(x=LCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit3 %>% ggplot(aes(x=UCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit3 %>% ggplot(aes(x=length)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' 
  #' 
  #' # Adding a retests
  #' apply(matrix(1:100), 1, function(x){
  #'   
  #'   simulated_data <- simulate_data(n_retest=3) %>% dplyr::rename(BVA = Response)
  #'   vca.anova.fit <- anovaVCA(formula.1, simulated_data)$aov.tab
  #'   for.return <- list(vca.anova.fit = vca.anova.fit)
  #'   REML.fit <- fitVCA(formula.1, simulated_data, method = "REML")
  #'   for.return$REML.fit <- REML.fit
  #'   #' Raunig DL, McShane LM, Pennello G, Gatsonis C, Carson PL, Voyvodic JT, Wahl RL, Kurland BF, Schwarz AJ, Gönen M, Zahlmann G. 
  #'   #' Quantitative imaging biomarkers: a review of statistical methods for technical performance assessment. 
  #'   #' Statistical methods in medical research. 2015 Feb;24(1):27-67.
  #'   
  #'   VCA.TAV <- VCA.total.analytic.variation(VCA.fit = vca.anova.fit, VCA.data.in = simulated_data, alpha = .05) 
  #'   VCA.TAV <- VCA.TAV %>% mutate(sim=x)
  #'   VCA.TAV
  #' }) %>% bind_rows() -> holdit4
  #' 
  #' holdit4 %>% ggplot(aes(x=Estimate)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit4 %>% ggplot(aes(x=LCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit4 %>% ggplot(aes(x=UCL)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  #' holdit4 %>% ggplot(aes(x=length)) + geom_histogram() + facet_wrap(~Metric, scales="free")
  
  
 
  
  formula.1 <- as.formula(paste0(this_endpoint, "~ (Subject  + Traversal  + Subject*Traversal)"))
  if(analysis.data.in$STUDYID[1] != "Sheffield") 
  {simulated_data <- simulate_data() %>% dplyr::rename(!!sym(this_endpoint) := Response) } else if(analysis.data.in$STUDYID[1]=="TAK-062-2001"){
    analysis.data.in <- analysis.data.in %>% ungroup() %>% dplyr::select(SUBJID, Retest.Flag, all_of(this_endpoint))
    analysis.data.in <- analysis.data.in %>% dplyr::rename(Subject = SUBJID,
                                                           Traversal = Retest.Flag) 
    analysis.data.in <- analysis.data.in %>%
      dplyr::mutate(Traversal = as.numeric(as.factor(Traversal))) 
    analysis.data.in <- as.data.frame(analysis.data.in)
    analysis.data.in$Subject <- factor(analysis.data.in$Subject)
  }
  else {
    simulated_data.1 <- analysis.data.in %>% 
     dplyr::select(SUBJID, READER, this_endpoint) %>% pivot_wider(names_from=READER, values_from=this_endpoint) %>%
      dplyr::select(STUDYID, SITE, SUBJID, MLV22, HumanAve) %>% 
      dplyr::mutate(Traversal = 1, Retest = 1)
    simulated_data.2 <- analysis.data.in %>% 
      dplyr::select(SUBJID, READER, this_endpoint) %>% pivot_wider(names_from=READER, values_from=this_endpoint) %>%
      dplyr::select(STUDYID, SITE, SUBJID, MLV22, HumanAve) %>% 
      dplyr::mutate(Traversal = 1, Retest = 2) %>% 
      dplyr::mutate(HumanAve = HumanAve + rnorm(n(), mean = 0, sd = .1),
                    MLV22 = MLV22 + rnorm(n(), mean = 0, sd = .05)) %>%
      dplyr::mutate(HumanAve = ifelse(HumanAve < 0, 0, HumanAve),
                    HumanAve = ifelse(HumanAve > 3, 3, HumanAve),
                    MLV22 = ifelse(MLV22 < 0, 0, MLV22),
                    MLV22 = ifelse(MLV22 > 3, 3, MLV22))
    simulated_data.3 <- analysis.data.in %>% 
      dplyr::select(SUBJID, READER, this_endpoint) %>% pivot_wider(names_from=READER, values_from=this_endpoint) %>%
      dplyr::select(STUDYID, SITE, SUBJID, MLV22, HumanAve) %>% 
      dplyr::mutate(Traversal = 2, Retest = 1) %>% 
      dplyr::mutate(HumanAve = HumanAve + rnorm(n(), mean = 0, sd = .2),
                    MLV22 = MLV22 + rnorm(n(), mean = 0, sd = .15)) %>%
      dplyr::mutate(HumanAve = ifelse(HumanAve < 0, 0, HumanAve),
                    HumanAve = ifelse(HumanAve > 3, 3, HumanAve),
                    MLV22 = ifelse(MLV22 < 0, 0, MLV22),
                    MLV22 = ifelse(MLV22 > 3, 3, MLV22))
    simulated_data.4 <- simulated_data.3 %>% 
      dplyr::mutate(Retest = 2) %>% 
      dplyr::mutate(HumanAve = HumanAve + rnorm(n(), mean = 0, sd = .1),
                    MLV22 = MLV22 + rnorm(n(), mean = 0, sd = .05)) %>%
      dplyr::mutate(HumanAve = ifelse(HumanAve < 0, 0, HumanAve),
                    HumanAve = ifelse(HumanAve > 3, 3, HumanAve),
                    MLV22 = ifelse(MLV22 < 0, 0, MLV22),
                    MLV22 = ifelse(MLV22 > 3, 3, MLV22))
    simulated_data <- bind_rows(simulated_data.1, simulated_data.2, simulated_data.3, simulated_data.4)
    # simulated_data <- simulated_data %>% dplyr::rename(!!sym(this_endpoint) := HumanAve)
    simulated_data <- simulated_data %>% dplyr::rename(!!sym(this_endpoint) := MLV22)
    simulated_data <- simulated_data %>% dplyr::rename(Subject =SUBJID  )
    simulated_data <- as.data.frame(simulated_data) %>% dplyr::mutate(Subject = factor(Subject))
  }
  
  # replacing simulated_data with analysis_data_in for TAK-062-2001
  vca.anova.fit <- anovaVCA(formula.1, analysis.data.in, NegVC=TRUE)$aov.tab
  for.return <- list(vca.anova.fit = vca.anova.fit)
  REML.fit <- fitVCA(formula.1, analysis.data.in, method = "REML")
  for.return$REML.fit <- REML.fit
  #' Raunig DL, McShane LM, Pennello G, Gatsonis C, Carson PL, Voyvodic JT, Wahl RL, Kurland BF, Schwarz AJ, Gönen M, Zahlmann G.
  #' Quantitative imaging biomarkers: a review of statistical methods for technical performance assessment.
  #' Statistical methods in medical research. 2015 Feb;24(1):27-67.

  for.return$VCA.total.analytic.variation <- VCA.total.analytic.variation(VCA.fit = vca.anova.fit, VCA.data.in = analysis.data.in, alpha = .05)
  for.return$MDD <- get.MDD(VCA.fit = REML.fit)
  # replace with actual data
  for.return$data <- analysis.data.in %>% dplyr::mutate(endpoint = this_endpoint, reader = reader_type)
  for.return
}




  