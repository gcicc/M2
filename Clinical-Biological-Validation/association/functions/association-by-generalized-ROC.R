#' phi function for Obuchowski estimator
#'
#' @param i base subject
#' @param j comparator subject
#' @param temp data.frame containing comparator and this_endpoint values
#'
#' @return 1 if the Comparator and this_endpoint values are in the same order, 0.5 if they are equal, 0 otherwise
#' @export
#'
#' @examples
phi <- function(i=1, j=2, temp) {
  for.return <- case_when(i == j ~ 0,
    (temp$comparator[i] > temp$comparator[j] & temp$this_endpoint[i] > temp$this_endpoint[j]) 
                          | (temp$comparator[j] > temp$comparator[i] & temp$this_endpoint[j] > temp$this_endpoint[i]) ~ 1,
                          temp$comparator[i] == temp$comparator[j] | temp$this_endpoint[i] == temp$this_endpoint[j] ~ 0.5,
                          .default=0)
  return(for.return)
}

#' return.Obuchowski
#'
#' @param df.in data.frame containing Comparators and vce_endpoints
#' @param visit visit at which to filter data
#' @param comparators Comparator column name
#' @param this_endpoint this_endpoint column name
#'
#' @return data.frame containing the Obuchowski estimator and confidence intervals
#' @export
#'
#' @examples
# This version makes use of the C++ code in ROC-speedup.cpp
# For MI imputation old code took 45 minute for 10 imputations. New code took 1.23 sec!!
return.Obuchowski<- function(df.in, this_endpoint) {
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
 
 
# This is the version that we've previously used
return.Obuchowski.old <- function(df.in, this_endpoint){
  for.return <- apply(matrix(1:length(comparators)), 1, function(x) {
    comparator <- comparators[x]
    temp <- df.in %>%
            dplyr::select(STUDYID, SITE, SUBJID, pairing, !!comparator, !!this_endpoint) %>%
            dplyr::rename(comparator=!!comparator, this_endpoint=!!this_endpoint)  %>% na.omit()
    nrow.temp <- nrow(temp)
    # If the comparator is VHCD, then reverse the order so that Obuchoswki Estimator makes sense
    if(comparator %in% c("VHCD", "VCIEL")) temp$comparator <- -temp$comparator
    
    # Compute theta-hat - Table 1 Continuous Comparator
    sum.phi <- c()
    for (i in 1:nrow(temp)) {
      for (j in 1:nrow(temp)) {
        sum.phi <- c(sum.phi, phi(i, j, temp))
      }
    }
    
    theta.hat <- sum(sum.phi) / (nrow(temp) * (nrow(temp) - 1))
    
    # Formula 4
    get.struct.components <- function(temp, i) {
      struct.components <- c()
      for (j in 1:nrow(temp)) {
        struct.components <- c(struct.components, ifelse(i != j, phi(i, j, temp), 0))
      }
      1 / (nrow(temp) - 1) * sum(struct.components)
    }
    
    # Formula 3
    for.variance <- data.frame(
      stuctural.components =
        apply(matrix(1:nrow(temp)), 1, function(x) {
          get.struct.components(temp, x)
        }), theta.hat=theta.hat
    ) %>%
      dplyr::mutate(summand=(stuctural.components - theta.hat)^2) %>%
      dplyr::summarize(variance=sum(summand) * 1 / ((nrow(temp) / 2) * (nrow(temp) / 2 - 1)))
    
    for.return <- data.frame(
      Estimate=theta.hat) %>% mutate(
      n = nrow.temp,
      variance=for.variance$variance,
      p.value = 1 - pnorm((Estimate - 0.5) / sqrt(for.variance$variance)),
      LCL=Estimate - qnorm(.975) * sqrt(for.variance$variance),
      UCL=Estimate + qnorm(.975) * sqrt(for.variance$variance)
    ) %>% mutate(comparator=comparator, this_endpoint=this_endpoint)
    for.return
  }) %>%  
  bind_rows() %>%
  # Can't have spaces here if we are to write to .csv and append...
  dplyr::rename(Comparator=comparator, this_endpoint=this_endpoint) %>% dplyr::mutate(Statistic = "Obuchowski")
  
  for.return$Comparator <- factor(for.return$Comparator, 
                                     levels=comparators,
                                     labels=lapply(comparators, label_for_doc))
  for.return <- for.return %>% arrange(Comparator)  %>%
    dplyr::select(this_endpoint, Comparator, Statistic, n, everything())
  return(for.return)
}

#' Generate Obuchowski Report
#' 
#' This function takes an input dataset and formats it to generate a report 
#' with Obuchowski Estimator and Confidence Intervals. It renames specific columns,
#' arranges the data, and formats the numbers to three decimal places.
#' 
#' @param Obuchowski.report.in A data frame containing the input data for the report.
#' 
#' @return A gt table report formatted with Obuchowski Estimators and Confidence Intervals.
get.Obuchowski.report <- function(Obuchowski.report.in, this_endpoint) {
  if ("pairing" %in% colnames(Obuchowski.report.in)) {
    reader_type <- Obuchowski.report.in$reader_type[1]
    Obuchowski.report.in %>%
      dplyr::rename(`Comparator`=Comparator, `MARCS Score`=this_endpoint, `Pairing`=pairing, `p-value`=p.value) %>%
      arrange( `Pairing`, `Comparator`) %>%
      dplyr::select(-Statistic, -reader_type) %>%
      dplyr::select(`MARCS Score`, `Comparator`, `Pairing`, everything()) %>%
      gt() %>%
      cols_merge(
        columns = c(LCL, UCL),
        pattern = "[{1}, {2}]"
      ) %>%
      cols_label(
        LCL = "Confidence Interval"
      ) %>%
      fmt_number(
        columns = c(Estimate, LCL, UCL, `p-value`),
        decimals = 2
      ) %>%
      tab_header(title=paste0("Obuchowski Estimators and 95% Confidence Intervals by Visit for ", label_for_doc(this_endpoint)),
                 subtitle=paste("for", reader_type, "reads"))
  } else {
    reader_type <- Obuchowski.report.in$reader_type[1]
    
    Obuchowski.report.in %>%
      dplyr::rename(`Comparator`=Comparator, `MARCS Score`=this_endpoint, `p-value`=p.value) %>%
      arrange(`Comparator`) %>%
      dplyr::select(-Statistic, -reader_type) %>%
      dplyr::select(`MARCS Score`, `Comparator`, everything()) %>%
      gt()  %>%
      cols_merge(
        columns = c(LCL, UCL),
        pattern = "[{1}, {2}]"
      ) %>%
      cols_label(
        LCL = "Confidence Interval"
      ) %>%
      fmt_number(
        columns = c(Estimate, LCL, UCL, `p-value`),
        decimals = 2
      ) %>%
      fmt_number(columns=c(Estimate, `p-value`), decimals=2) %>%
      tab_header(title=paste0("Obuchowski Estimators and 95% Confidence Intervals for ", label_for_doc(this_endpoint)),
                 subtitle=paste("for", reader_type, "reads"))
  }
}

#' Generate Obuchowski Figure
#' 
#' This function takes an input dataset and formats it to generate a ggplot2-based 
#' figure with Obuchowski Estimator and Confidence Intervals. It creates a new 
#' column by combining 'Comparator' and 'Pairing', then plots the data 
#' with points and horizontal error bars.
#' 
#' @param Obuchowski.report.table.in A data frame containing the input data for the figure.
#' @param this_endpoint A character string representing the diagnostic endpoint to be included in the subtitle.
#' 
#' @return A ggplot object representing the Obuchowski Estimators and 95% Confidence Intervals.
get.Obuchowski.figure <- function(Obuchowski.report.table.in, this_endpoint) {
  if("stratum" %in% colnames(Obuchowski.report.table.in)) {
    Obuchowski.report.table.in %>%
      dplyr::mutate(Endpoint=paste(`Comparator`, `stratum`)) %>%
      ggplot(aes(y=Endpoint, x=Estimate, xmin=LCL, xmax=UCL)) +
      geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
      geom_errorbarh(height=0.2) +
      labs(
        title="Obuchowski Estimators and 95% CI",
        subtitle=paste0("this_endpoint: ", label_for_doc(this_endpoint)),
        x="Obuchowski Estimator", y="Endpoint"
      )
  } else {
    Obuchowski.report.table.in %>%
      dplyr::mutate(Endpoint=paste(`Comparator`)) %>%
      ggplot(aes(y=Endpoint, x=Estimate, xmin=LCL, xmax=UCL)) +
      geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
      geom_errorbarh(height=0.2) +
      labs(
        title="Obuchowski Estimators and 95% CI",
        subtitle=paste0("this_endpoint: ", label_for_doc(this_endpoint)),
        x="Obuchowski Estimator", y="Endpoint"
      )
  }
}
    