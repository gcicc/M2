# main_analysis
# This function performs the majority of the analysis calculations. it is called multiple
# times with filtered data to support different configurations, e.g., differing IQ levels,
# ML vs. Human, etc.
#
# Note that the lower triangular is implemented in a separate function (defined at the
# bottom of this file) as it should not be run on all combinations of input data but
# instead on a chosen "global analysis set".
#

#' Perform Main Analysis
#'
#' This function performs the main analysis, including correlation reports, scatterplots, and ROC analysis.
#' It also conducts stratified analyses and optionally runs multiple imputation for missing data.
#'
#' @param analysis.data.in Data frame containing the input data.
#' @param reader_type The type of reader used, for use in filenames.
#' @param this_endpoint The endpoint of interest in the analysis.
#' @param runMI Logical indicating whether to run multiple imputation.
#' @return A list containing various analysis results.
#' @import dplyr
#' @importFrom ggplot2 ggsave
#' @importFrom miceadds micombine.cor
#' @importFrom tictoc tic toc
#' @importFrom Hmisc corr.test
main_analysis <- function(analysis.data.in, reader_type, this_endpoint, runMI=FALSE) {
  for.return <- list()
  
  # Save the input data to a CSV file
  write.csv(analysis.data.in, path(output_dir, paste0("8 analysis.data.in-", reader_type, "-", this_endpoint, ".csv")), row.names=FALSE)
  
  # Perform the summary analyses
  for.return$cor.report <- report.correlation(df.in=analysis.data.in, group1=this_endpoint, group2=comparators) %>% 
    dplyr::mutate(reader_type=reader_type)
  for.return$scatterplots <- report.correlation.scatter(df.in=analysis.data.in, this_endpoint=this_endpoint)
  for.return$roc.report <- return.Obuchowski(df.in=analysis.data.in, this_endpoint=this_endpoint) %>% 
    dplyr::mutate(reader_type=reader_type)
  for.return$roc.figure <- get.Obuchowski.figure(Obuchowski.report.table.in=for.return$roc.report, this_endpoint=this_endpoint)
  
  for (stratifier in c("AggHist", "MO", "qM", "RandStat", "Pairing", "Arm")) {
    if (stratifier=="AggHist") {
      analysis.data.in$stratum <- ifelse(analysis.data.in$Agg_Histology >= median(analysis.data.in$Agg_Histology, na.rm=TRUE), "More Diseased", "Less Diseased")
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byAggHist <- NULL
        for.return$scatterplots.byAggHist <- NULL
        for.return$roc.report.byAggHist <- NULL
        for.return$roc.figure.byAggHist <- NULL
        next
      }
    } else if (stratifier=="MO") {
      analysis.data.in$stratum <- factor(analysis.data.in$MO)
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byMO <- NULL
        for.return$scatterplots.byMO <- NULL
        for.return$roc.report.byMO <- NULL
        for.return$roc.figure.byMO <- NULL
        next
      }
    } else if (stratifier=="qM") {  # Moved "else if" to separate line
      analysis.data.in$stratum <- factor(analysis.data.in$qM)
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byqM <- NULL
        for.return$scatterplots.byqM <- NULL
        for.return$roc.report.byqM <- NULL
        for.return$roc.figure.byqM <- NULL
        next
      }
    } else if (stratifier=="RandStat") {
      analysis.data.in$stratum <- ifelse(is.na(analysis.data.in$RANDDATE), "Not Randomized", "Randomized")
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byRandStat <- NULL
        for.return$scatterplots.byRandStat <- NULL
        for.return$roc.report.byRandStat <- NULL
        for.return$roc.figure.byRandStat <- NULL
        next
      }
    } else if (stratifier=="Pairing") {
      analysis.data.in$stratum <- analysis.data.in$pairing
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byPairing <- NULL
        for.return$scatterplots.byPairing <- NULL
        for.return$roc.report.byPairing <- NULL
        for.return$roc.figure.byPairing <- NULL
        next
      }
    } else if (stratifier=="Arm") {
      analysis.data.in$stratum <- analysis.data.in$ARM
      uniques <- unique(na.omit(analysis.data.in$stratum))
      if (length(uniques[!is.na(uniques)]) < 2) {
        for.return$cor.report.byArm <- NULL
        for.return$scatterplots.byArm <- NULL
        for.return$roc.report.byArm <- NULL
        for.return$roc.figure.byArm <- NULL
        next
      }
    }
    
    cor.report <- apply(matrix(1:length(unique(na.omit(analysis.data.in$stratum)))), 1, function(y) {
      report.correlation(df.in=(analysis.data.in %>%
                                  dplyr::filter(stratum==unique(na.omit(analysis.data.in$stratum))[y])), group1=this_endpoint, group2=comparators) %>%
        dplyr::mutate(stratum=unique(na.omit(analysis.data.in$stratum))[y])
    }) %>% bind_rows() %>%
      dplyr::select(Endpoint, Comparator, stratum, Statistic, n, Estimate, `p.value`, LCL, UCL) %>%
      dplyr::mutate(reader_type=reader_type)
    
    scatterplots <- apply(matrix(1:length(unique(na.omit(analysis.data.in$stratum)))), 1, function(y) {
      report.correlation.scatter(df.in=(analysis.data.in %>%
                                          dplyr::filter(stratum==unique(na.omit(analysis.data.in$stratum))[y])), this_endpoint=this_endpoint)
    })
    
    names(scatterplots) <- unique(na.omit(analysis.data.in$stratum))
    
    if (length(scatterplots) > 0) {
      for (i in 1:length(scatterplots)) {
        for (j in 1:length(scatterplots[[i]])) {
          scatterplots[[i]][[j]]$labels$subtitle <- paste0(scatterplots[[i]][[j]]$labels$subtitle, reader_type, "-", names(scatterplots)[i])
          scatterplots[[i]][[j]] %>% ggsave(filename=path(figure_path, paste0("sp-", scatterplots[[i]][[j]]$labels$y, " vs ", scatterplots[[i]][[j]]$labels$x, "-", reader_type, "-", names(scatterplots)[i], ".png")))
        }
      }
    }
    
    roc.report <- apply(matrix(1:length(unique(na.omit(analysis.data.in$stratum)))), 1, function(y) {
      return.Obuchowski(df.in=(analysis.data.in %>%
                                 dplyr::filter(stratum==unique(na.omit(analysis.data.in$stratum))[y])), this_endpoint=this_endpoint) %>%
        dplyr::mutate(stratum=unique(na.omit(analysis.data.in$stratum))[y])
    }) %>% bind_rows() %>% mutate(reader_type=reader_type)
    
    roc.figure <- get.Obuchowski.figure(Obuchowski.report.table.in=roc.report, this_endpoint=this_endpoint)
    
    if (stratifier=="AggHist") {
      for.return$cor.report.byAggHist <- cor.report
      for.return$scatterplots.byAggHist <- scatterplots
      for.return$roc.report.byAggHist <- roc.report
      for.return$roc.figure.byAggHist <- roc.figure
    } else if (stratifier=="MO") {
      for.return$cor.report.byMO <- cor.report
      for.return$scatterplots.byMO <- scatterplots
      for.return$roc.report.byMO <- roc.report
      for.return$roc.figure.byMO <- roc.figure
    } else if (stratifier=="qM") {
      for.return$cor.report.byqM <- cor.report
      for.return$scatterplots.byqM <- scatterplots
      for.return$roc.report.byqM <- roc.report
      for.return$roc.figure.byqM <- roc.figure
    } else if (stratifier=="RandStat") {
      for.return$cor.report.byRandStat <- cor.report
      for.return$scatterplots.byRandStat <- scatterplots
      for.return$roc.report.byRandStat <- roc.report
      for.return$roc.figure.byRandStat <- roc.figure
    } else if (stratifier=="Pairing") {
      for.return$cor.report.byPairing <- cor.report
      for.return$scatterplots.byPairing <- scatterplots
      for.return$roc.report.byPairing <- roc.report
      for.return$roc.figure.byPairing <- roc.figure
    } else if (stratifier=="Arm") {
      for.return$cor.report.byArm <- cor.report
      for.return$scatterplots.byArm <- scatterplots
      for.return$roc.report.byArm <- roc.report
      for.return$roc.figure.byArm <- roc.figure
    }
  }
  
  # Multiple imputation
  if(runMI == TRUE) {
    if (study == "TAK-062-2001") {
      temp <- analysis.data.in %>%
        dplyr::select(STUDYID, SITE, SUBJID, SEX, RACE, ETHNICITY, pairing, ARM, BVA, SEROSTATUS, HISTINJURY, PPIHIST2ANT, VHCD, IELCOUNT, GISS)
      
      # Separate data into placebo and treatment arms
      placebo_data <- subset(temp, ARM == "Placebo")
      treatment_data <- subset(temp, ARM == "TAK-062")
      
      predMat <- make.predictorMatrix(placebo_data)
      
      cat("Starting MI\n")
      tictoc::tic() 
      imputed_placebo <- futuremice(placebo_data, n.core = 10, m = 1000, maxit = 50, method = c(rep("", 9), rep("polyreg", 3), rep('pmm', 3)), predictorMatrix = predMat, parallelseed = 1234)
      imputed_treatment <- futuremice(treatment_data, n.core = 10, m = 1000, method = c(rep("", 9), rep("polyreg", 3), rep('pmm', 3)), predictorMatrix = predMat, parallelseed = 5687)
      tictoc::toc()
      
      imps <- rbind(imputed_placebo, imputed_treatment)
      temp <- complete(imps, 'long', include = TRUE) %>%
        dplyr::mutate(
          VCIEL = VHCD / sd(VHCD, na.rm = TRUE) - IELCOUNT / sd(IELCOUNT, na.rm = TRUE),
          VHCD.s = (VHCD - mean(VHCD, na.rm = TRUE)) / sd(VHCD, na.rm = TRUE),
          IELCOUNT.s = (IELCOUNT - mean(IELCOUNT, na.rm = TRUE)) / sd(IELCOUNT, na.rm = TRUE),
          GISS.s = (GISS - mean(GISS, na.rm = TRUE)) / sd(GISS, na.rm = TRUE),
          Agg_Histology = sqrt(2) / 2 * (VHCD.s + IELCOUNT.s),
          Disease_Burden = sqrt(2) / 2 * (VHCD.s + IELCOUNT.s) + GISS
        ) %>%
        dplyr::select(-VHCD.s, -IELCOUNT.s, -GISS.s)
      
      new_imps <- as.mids(temp)
      
      # Build correlation report summary
      result.pearson <- miceadds::micombine.cor(mi.res = new_imps, variables = c(9, 13:18)) %>% 
        dplyr::filter(variable1 == "BVA") %>%
        dplyr::mutate(Method = "Pearson") %>%
        dplyr::select(Method, variable1, variable2, r, lower95, upper95, p) %>%
        dplyr::rename(Endpoint = variable1, Comparator = variable2, Estimate = r, LCI = lower95, UCI = upper95, p.value = p)
      
      result.spearman <- miceadds::micombine.cor(mi.res = new_imps, variables = c(9, 13:18), method = "spearman") %>% 
        dplyr::filter(variable1 == "BVA") %>%
        dplyr::mutate(Method = "Spearman") %>%
        dplyr::select(Method, variable1, variable2, r, lower95, upper95, p) %>%
        dplyr::rename(Endpoint = variable1, Comparator = variable2, Estimate = r, LCI = lower95, UCI = upper95, p.value = p)
      
      for.return$cor.report.MI <- bind_rows(result.pearson, result.spearman) %>% 
        dplyr::rename(Statistic = Method) %>%
        dplyr::mutate(
          n = nrow(complete(new_imps, 1)),
          reader_type = analysis.data.in$READER[1]
        ) %>%
        dplyr::select(Endpoint, Comparator, Statistic, n, Estimate, p.value, LCI, UCI, reader_type)
      
      # Build ROC summary
      cat("Starting MI ROC\n")
      tictoc::tic() 
      Obuchowski_imps <- list()
      for (i in 1:1000) {
        Obuchowski_imps[[i]] <- return.Obuchowski(df.in = complete(new_imps, i), this_endpoint = this_endpoint)
      }
      Obuchowski_imps <- bind_rows(Obuchowski_imps)
      tictoc::toc()
      
      for.return$roc.report.MI <- apply(matrix(1:n_distinct(Obuchowski_imps$Comparator)), 1, function(x) {
        comparator <- unique(Obuchowski_imps$Comparator)[x]
        temp <- Obuchowski_imps %>% dplyr::filter(Comparator == comparator)
        MI.out <- pool.scalar(Q = temp$Estimate, U = temp$variance, n = temp$n, k = 1)
        data.frame(
          Estimate = MI.out$qbar,
          variance = MI.out$ubar,
          LCI = MI.out$qbar - qnorm(0.975) * sqrt(MI.out$ubar),
          UCI = MI.out$qbar + qnorm(0.975) * sqrt(MI.out$ubar)
        ) %>%
          dplyr::mutate(this_endpoint = Obuchowski_imps$this_endpoint[1], Comparator = comparator)
      }) %>% bind_rows() %>%
        dplyr::mutate(p.value = 1 - pnorm((Estimate - 0.5) / sqrt(variance))) %>%
        dplyr::select(this_endpoint, Comparator, Estimate, p.value, LCI, UCI) %>%
        dplyr::mutate(reader_type = analysis.data.in$READER[1])
    }
  }
  
  results_list <- list()
  
  # Cross-sectional summary statistics
  visit.pairing <- ifelse("VISIT" %in% colnames(analysis.data.in), "VISIT", "pairing")
  
  # Loop through each comparator
  for (endpoint in c(this_endpoint, comparators)) {
    result <- analysis.data.in %>%
      group_by(STUDYID, ARM, !!sym(visit.pairing)) %>%
      dplyr::summarise(
        n = n(),
        mean = mean(!!sym(endpoint), na.rm = TRUE),
        sd = sd(!!sym(endpoint), na.rm = TRUE),
        median = median(!!sym(endpoint), na.rm = TRUE),
        min = min(!!sym(endpoint), na.rm = TRUE),
        max = max(!!sym(endpoint), na.rm = TRUE),
        LCL = mean(!!sym(endpoint), na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(!!sym(endpoint), na.rm = TRUE) / sqrt(n()),
        UCL = mean(!!sym(endpoint), na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(!!sym(endpoint), na.rm = TRUE) / sqrt(n())
      ) %>% 
      dplyr::mutate(reader_type = reader_type, endpoint = endpoint)
    results_list[[endpoint]] <- result
  }
  
  for.return$sumstats <- bind_rows(results_list)
  
  for.return$analysis.data <- analysis.data.in
  
  # Comparison of groups at each visit
  endpoints <- c(this_endpoint, comparators)
  
  # Pivot data to long format
  long_data <- analysis.data.in %>%
    dplyr::select(STUDYID, SITE, SUBJID, all_of(endpoints), ARM, !!sym(visit.pairing)) %>%
    pivot_longer(cols = c(this_endpoint, all_of(comparators))) %>%
    dplyr::rename(endpoint = name, value = value)
  
  if(visit.pairing == "VISIT") {
    t_test_result <- list()
    for(endpoint in endpoints) {
      for(visit in c("VISIT 2 - Week -4", "VISIT 6 - Week 24", if(endpoint %in% c(this_endpoint, "GISS")) "VISIT 5 - Week 12")) {
        filtered_data <- long_data %>%
          dplyr::filter(endpoint == !!endpoint, VISIT == visit) %>% 
          dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
        filtered_data$ARM <- factor(filtered_data$ARM, levels = c("Placebo", "TAK-062"))
        t_test <- t.test(value ~ ARM, data = filtered_data)
        t_test_tidied <- tidy(t_test) %>%
          mutate(
            VISIT = visit, 
            endpoint = !!endpoint,
            n_group1 = sum(!is.na(filtered_data$value[filtered_data$ARM == levels(filtered_data$ARM)[1]])),
            n_group2 = sum(!is.na(filtered_data$value[filtered_data$ARM == levels(filtered_data$ARM)[2]]))
          )
        t_test_result[[length(t_test_result) + 1]] <- t_test_tidied
      }
    }
  }
  
  if(visit.pairing == "pairing") {
    t_test_result <- list()
    for(endpoint in endpoints) {
      # First pairing: "VISIT 2 - Week -4:VISIT 5 - Week 12"
      filtered_data <- long_data %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(endpoint == !!endpoint, pairing == "VISIT 2 - Week -4:VISIT 5 - Week 12") %>% 
        dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
      filtered_data$ARM <- factor(filtered_data$ARM, levels = c("Placebo", "TAK-062"))
      if (length(unique(filtered_data$ARM)) == 2) {
        t_test_result[[length(t_test_result) + 1]] <- tidy(t.test(value ~ ARM, data = filtered_data)) %>%
          mutate(pairing = "VISIT 2 - Week -4:VISIT 5 - Week 12", endpoint = !!endpoint)
      }
      
      # Second pairing: "VISIT 5 - Week 12:VISIT 6 - Week 24"
      filtered_data <- long_data %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(endpoint == !!endpoint, pairing == "VISIT 5 - Week 12:VISIT 6 - Week 24") %>% 
        dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
      filtered_data$ARM <- factor(filtered_data$ARM, levels = c("Placebo", "TAK-062"))
      if (length(unique(filtered_data$ARM)) == 2) {
        t_test_result[[length(t_test_result) + 1]] <- tidy(t.test(value ~ ARM, data = filtered_data)) %>%
          mutate(pairing = "VISIT 5 - Week 12:VISIT 6 - Week 24", endpoint = !!endpoint)
      }
      
      # Third pairing: "VISIT 2 - Week -4:VISIT 6 - Week 24"
      filtered_data <- long_data %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(endpoint == !!endpoint, pairing == "VISIT 2 - Week -4:VISIT 6 - Week 24") %>% 
        dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
      filtered_data$ARM <- factor(filtered_data$ARM, levels = c("Placebo", "TAK-062"))
      if (length(unique(filtered_data$ARM)) == 2) {
        t_test_result[[length(t_test_result) + 1]] <- tidy(t.test(value ~ ARM, data = filtered_data)) %>%
          mutate(pairing = "VISIT 2 - Week -4:VISIT 6 - Week 24", endpoint = !!endpoint)
      }
      
      if (endpoint %in% c(this_endpoint, "GISS")) {
        filtered_data <- long_data %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::filter(endpoint == !!endpoint, pairing == "VISIT 2 - Week -4:VISIT 6 - Week 24")
      }
    }
  }
  
  for.return$cross.sectional.tests <- bind_rows(t_test_result) %>% 
    dplyr::rename(Difference = estimate,
                  PBO = estimate1,
                  `TAK-062` = estimate2,
                  LCL = conf.low,
                  UCL = conf.high) %>%
    dplyr::select(endpoint, all_of(visit.pairing), PBO, `TAK-062`, Difference, LCL, UCL, p.value) %>%
    dplyr::mutate(reader_type = reader_type) %>% 
    dplyr::mutate(endpoint = factor(endpoint, levels = c(this_endpoint, comparators))) %>%
    dplyr::arrange(!!visit.pairing, endpoint)
  
  return(for.return)
}

#' Lower Triangle Function
#'
#' This function generates the lower triangle correlation report for the input data.
#'
#' @param df.in Data frame containing the input data.
#' @param reader_type The type of reader used, for use in filenames.
#' @param this_endpoint The endpoint of interest in the analysis.
#' @return A list containing the lower triangle correlation report.
lower.triangle_func <- function(df.in, reader_type, this_endpoint) {
  
  for.return <- list()
  
  # Generate correlation report for comparators
  comp.cor.report <- report.correlation(df.in=df.in, group1=comparators, group2=comparators)
  
  # Create lower triangle correlation matrix
  for.return$lower.triangle <- create.lower.triangle(comp.cor.report)
  
  return(for.return)
}