#' Report Correlation
#'
#' This function calculates Pearson and Spearman correlations for pairs of variables from two groups.
#' It processes the data, performs correlation tests, and returns a data frame with the results.
#'
#' @param df.in Data frame containing the input data.
#' @param group1 A vector of column names representing the first group of variables.
#' @param group2 A vector of column names representing the second group of variables.
#' @return Data frame with correlation results.
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom Hmisc corr.test
report.correlation <- function(df.in, group1, group2) {
  
  # Generate all unique pairs of elements from group1 and group2
  pairs <- expand.grid(group1, group2)
  
  # Calculate correlations for all pairs
  for.return <- apply(matrix(1:nrow(pairs)), 1, function(x) {
    
    # Subset the data
    temp <- df.in %>%
      ungroup() %>%
      dplyr::select(pairs$Var1[x], pairs$Var2[x]) %>%
      na.omit()
    
    nrow.temp <- nrow(temp)
    
    if (nrow(temp) < 4) { # Need more than 4 rows for correlation calculation
      data.frame(Endpoint = pairs$Var1[x], Comparator = pairs$Var2[x], Statistic = c("Spearman", "Pearson"), n = nrow.temp, estimate = NA, `p.value` = NA, conf.low = NA, conf.high = NA)
      
    } else {
      temp.endpoint <- temp %>% dplyr::pull(pairs$Var1[x])
      temp.comparator <- temp %>% dplyr::pull(pairs$Var2[x])
      
      # Calculate Pearson and Spearman correlation using cor.test
      Pearson <- corr.test(temp.endpoint, temp.comparator, method = "pearson", ci = TRUE)
      Spearman <- corr.test(temp.endpoint, temp.comparator, method = "spearman", ci = TRUE)
      
      Pearson <- data.frame(n = Pearson$n, estimate = Pearson$ci$r[1], p.value = Pearson$ci$p[1], conf.low = Pearson$ci$lower[1], conf.high = Pearson$ci$upper[1])
      Spearman <- data.frame(n = Spearman$n, estimate = Spearman$ci$r[1], `p.value` = Spearman$ci$p[1], conf.low = Spearman$ci$lower[1], conf.high = Spearman$ci$upper[1])  # Spearman does not return CI
      
      bind_rows(Pearson %>% mutate(Statistic = "Pearson"), Spearman %>% mutate(Statistic = "Spearman")) %>%
        mutate(Endpoint = pairs$Var1[x], Comparator = pairs$Var2[x]) %>%
        dplyr::mutate(n = nrow.temp)
    }
  }) %>%
    bind_rows() %>%
    dplyr::rename(LCL = conf.low, UCL = conf.high, Estimate = estimate) %>%
    dplyr::select(Endpoint, Comparator, Statistic, n, everything())
  
  # Factorize Comparator and Endpoint columns with appropriate labels
  for.return$Comparator <- factor(for.return$Comparator, levels = c(comparators, vce_endpoints), labels = c(comparators, vce_endpoints))
  for.return$Endpoint <- factor(for.return$Endpoint, levels = c(comparators, vce_endpoints), labels = c(comparators, vce_endpoints))
  
  return(for.return)
}

#' Create Lower Triangle Correlation Matrix
#'
#' This function takes a correlation report and generates a lower triangle correlation matrix.
#' It processes the data to create a matrix with only the lower triangle filled with Spearman correlation estimates.
#'
#' @param cor.report.in Data frame containing the correlation report.
#' @return Data frame with the lower triangle correlation matrix.
#' @import dplyr
#' @importFrom tidyr pivot_wider
create.lower.triangle <- function(cor.report.in) {
  
  # Filter for Spearman correlation estimates and reshape the data to wide format
  lower.triangle <- cor.report.in %>%
    dplyr::filter(Statistic == "Spearman") %>%
    dplyr::select(Endpoint, Comparator, Estimate) %>%
    pivot_wider(names_from = Comparator, values_from = Estimate)
  
  # Convert to data frame and set row names
  lower.triangle <- as.data.frame(lower.triangle)
  rownames(lower.triangle) <- lower.triangle$Endpoint
  
  # Remove the Endpoint column and convert to matrix
  lower.triangle <- lower.triangle %>% dplyr::select(-Endpoint)
  lower.triangle <- as.matrix(lower.triangle)
  
  # Set upper triangle values to NA
  lower.triangle[upper.tri(lower.triangle)] <- NA
  
  return(data.frame(lower.triangle))
}

# Not currently used
report.comparator.correlation <- function(df.in) {
  
  pairs <- data.frame(t(combn(comparators, 2, simplify=TRUE)))
  
  for.return <- apply(matrix(1:nrow(pairs)), 1, function(x) {
    # subset the data
    temp <- df.in  %>% ungroup() %>% dplyr::select(pairs$X1[x], pairs$X2[x]) %>% na.omit()
    nrow.temp <- nrow(temp)
    if(nrow(temp) < 4) { # need more than 4 rows
      data.frame(Endpoint=pairs$X1[x], Comparator=pairs$X2[x], Statistic=c("Spearman", "Pearson"), n=nrow.temp, estimate=NA,  `p.value`=NA, conf.low=NA, conf.high=NA)
    } else {
      temp.Endpoint <- temp[,pairs$X1[x]] %>% dplyr::pull(1)
      temp.Comparator <- temp[,pairs$X2[x]] %>% dplyr::pull(1)
      
      # Calculate Pearson and Spearman correlation using cor.test
      Pearson <- tidy(cor.test(temp.Endpoint, temp.Comparator, method="pearson")) %>%
        dplyr::select(estimate, p.value, conf.low, conf.high)
      # cor.test does not offer CIs for Spearman
      Spearman <- tidy(cor.test(temp.Endpoint, temp.Comparator, method="spearman", exact=TRUE)) %>%
        dplyr::select(estimate, p.value)
      
      # Approach Spearman with bootstrap sampling
      spearman <- function(d, i) {
        rho <- cor(d[i, ], method="spearman")
        rho[lower.tri(rho)]
      }
      boot_out <- boot::boot(temp, spearman, R=10000)
      spearman.ci <-  tryCatch({
        spearman_ci_result <- boot::boot.ci(boot_out, type="bca")
        spearman_ci_result
      }, error=function(e) {
        # Print the error message
        cat("An error occurred:", conditionMessage(e), "\n Introduced by small sample sizes in correlation reporting.")
        # Return NULL or any other default value
        NULL
      })
      if (is.null(spearman.ci)) {
        Spearman$conf.low <- NA
        Spearman$conf.high <- NA
      } else {
        Spearman$conf.low <- spearman.ci$bca[4]
        Spearman$conf.high <- spearman.ci$bca[5]
      }
      # cor.test does not return confidence intervals for Spearman...
      bind_rows(Pearson %>% mutate(Statistic="Pearson"), Spearman %>% mutate(Statistic="Spearman")) %>%
        mutate(Endpoint=pairs$X1[x], Comparator=pairs$X2[x]) %>% dplyr::mutate(n=nrow.temp)
    } 
  }) %>% bind_rows() %>% 
    
    dplyr::rename(LCL=conf.low, UCL=conf.high, Estimate=estimate)%>%
    dplyr::select(Endpoint, Comparator, Statistic, n, everything())
  
  for.return$Comparator <- factor(for.return$Comparator, levels=comparators, labels=comparator.labels)
  for.return$Endpoint <- factor(for.return$Endpoint, levels=comparators, labels=comparator.labels)
  return(for.return)
}


#' Generate Correlation Report
#' 
#' This function takes an input dataset and formats it to generate a report 
#' with Pearson and Spearman correlations. It renames specific columns,
#' arranges the data, and formats the correlation values to three decimal places.
#' 
#' @param analysis.data.in A data frame containing the input data for the report with Pearson and Spearman correlations.
#' 
#' @return A gt table report formatted with Pearson and Spearman correlations.
get.cor.report <- function(cor.report.in, this_endpoint) {
  # If visit.Name is not present, then the report is not by visit
  if(!("pairing" %in% colnames(cor.report.in))){
    reader_type <- cor.report.in$reader_type[1]
    for.return <- cor.report.in %>% 
      dplyr::select(-reader_type) %>%
      dplyr::rename(`this_endpoint`=this_endpoint, `p-value`=p.value) %>%
      gt()  %>%
      cols_merge(columns=c(Estimate, LCL, UCL), pattern="{1} [{2}, {3}]") %>%
      cols_label(Estimate="Estimate [95% CI]") %>%
      cols_align(align="center", columns=everything()) %>%
      fmt_number(columns=c(Estimate,  `p-value`), decimals=2) %>%
      tab_header(title=paste("Report on Pearson and Spearman Correlations for ", label_for_doc(this_endpoint)),
                 subtitle=paste("for", reader_type, "reads"))
  } else {
    for.return <-  cor.report.in %>%
     dplyr::rename(`this_endpoint`=this_endpoint, `Pairing`=pairing, `p-value`=p.value) %>%
      dplyr::select(-reader_type) %>%
      gt() %>%
      cols_merge(columns=c(Estimate, LCL, UCL), pattern="{1} [{2}, {3}]") %>%
      cols_label(Estimate="Estimate [95% CI]") %>%
      cols_align(align="center", columns=everything()) %>%
      fmt_number(columns=c(Estimate,  `p-value`), decimals=2) %>%
      tab_header(title=paste("Report on Pearson and Spearman Correlations for ", label_for_doc(this_endpoint)),
                 subtitle=paste("for", reader_type, "reads"))
  }
  return(for.return)
}

#' Generate Scatter Plots with Pearson and Spearman Correlations
#' 
#' This function generates scatter plots between specified columns and a given endpoint,
#' calculating Pearson and Spearman correlations for each combination of columns and visits.
#' 
#' @param df.in A data frame containing the input data for the scatter plots.
#' @param comparators A character vector of column names to be plotted against the endpoint.
#' @param this_endpoint A character string representing the endpoint column to be compared against the specified columns.
#' 
#' @return A list of ggplot objects representing scatter plots with calculated Pearson and Spearman correlations.
report.correlation.scatter <- function(df.in, this_endpoint) {
  figures <- apply(matrix(1:length(comparators)), 1, function(x) {
    # subset the data
    temp <- df.in
    # Calculate Pearson and Spearman
    Pearson <- NA
    Spearman <- NA
    # tryCatch was needed to avoid an error when defining scatterplots.byVisit in analysis-block.R
    tryCatch({
      Pearson <- cor(temp[, comparators[x]], temp[, this_endpoint], method="pearson", use="pairwise.complete.obs")[1, 1]
      Spearman <- cor(temp[, comparators[x]], temp[, this_endpoint], method="spearman", use="pairwise.complete.obs")[1, 1]
    }, error=function(e){}) # if insufficient data, keep the NAs
    
    # Scatterplot
    if (length(unique(temp[, this_endpoint])) > 0) {
      temp %>% ggplot(aes(x=eval(parse(text=this_endpoint)), y=eval(parse(text=comparators[x])))) +
        geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
        geom_smooth(method="lm", se=FALSE) +
        labs(
          title="Association",
          x=label_for_doc(this_endpoint), y=label_for_doc(comparators[x]),
          caption=paste0("Pearson Correlation: ", round(Pearson, 2), "\n",
                         "Spearman Correlation: ", round(Spearman, 2)))
    }
  })
    names(figures) <- comparators
  return(figures)
}
