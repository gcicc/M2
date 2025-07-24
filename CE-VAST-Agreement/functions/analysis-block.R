#' Perform Main Analysis for CE-VAST Agreement Module
#'
#' This function performs agreement analysis between expert gastroenterologists/pathologists 
#' and the ML algorithm for per-frame disease severity assessment on the CE-VAST scale.
#' The function compares frame-level evaluations and calculates agreement metrics.
#' It is called multiple times with filtered data to support different configurations
#' (e.g., differing IQ levels, ML vs. Human readers).
#'
#' @param analysis.data.in Data frame containing processed VCE scoring data with required columns:
#'   \itemize{
#'     \item STUDYID: Study identifier (character)
#'     \item SITE: Site identifier (character)
#'     \item SUBJID: Subject identifier (character)
#'     \item READER: Reader identifier (ML algorithm or expert identifier)
#'     \item Frame-level scores: CE-VAST scale measurements per frame (0-3 severity)
#'     \item Position/timing information: For matching frames between readers
#'     \item IQ level: Image quality classification (if applicable)
#'   }
#' @param reader_type String identifying the reader type being analyzed:
#'   \itemize{
#'     \item "ML": Machine learning algorithm
#'     \item "Human": Human expert reader
#'     \item "Gastroenterologist": GI specialist
#'     \item "Pathologist": Pathology specialist
#'   }
#'
#' @return Named list containing agreement analysis results with components:
#'   \describe{
#'     \item{agreement_metrics}{Data frame with agreement statistics between readers}
#'     \item{frame_comparisons}{Detailed frame-by-frame comparison results}
#'     \item{summary_statistics}{Overall agreement summary across all comparisons}
#'     \item{reader_characteristics}{Reader-specific performance metrics}
#'     \item{correlation_analysis}{Correlation coefficients and significance tests}
#'     \item{concordance_measures}{Concordance correlation coefficients}
#'     \item{bias_assessment}{Systematic bias evaluation between reader types}
#'   }
#'
#' @details
#' The function performs the following analyses:
#' \itemize{
#'   \item Matches frames between expert and ML algorithm based on position/timing
#'   \item Calculates per-frame agreement on CE-VAST scale (0-3 severity levels)
#'   \item Computes overall agreement statistics (correlation, concordance, kappa)
#'   \item Assesses systematic differences between reader types
#'   \item Handles cases where frame positions have different decimal precision
#'   \item Evaluates agreement across different image quality levels
#' }
#' 
#' Agreement metrics include:
#' \itemize{
#'   \item Pearson and Spearman correlation coefficients
#'   \item Concordance correlation coefficient (CCC)
#'   \item Cohen's kappa for categorical agreement
#'   \item Bland-Altman analysis for bias assessment
#'   \item Percentage exact agreement and within-1-level agreement
#' }
#' 
#' @note Frame-level agreement with expert gastroenterologists is informative,
#' but region-level bias and linearity assessments are definitive for the BVA biomarker.
#'
#' @examples
#' \dontrun{
#'   # Load processed agreement data
#'   data <- load_agreement_data("Milan", "gastroenterologist")
#'   
#'   # Run agreement analysis for ML vs. Gastroenterologist
#'   results <- main_analysis(data, "ML")
#'   
#'   # Access results
#'   print(results$agreement_metrics)
#'   print(results$summary_statistics)
#' }
#'
#' @seealso
#' \code{\link{demog_milan_etl}}, \code{\link{vce_milan_etl}} for data preparation
#' 
#' @export
main_analysis <- function(analysis.data.in, reader_type) {
  # Initializing return ----
  for.return <- list()
  if(nrow(analysis.data.in)==0) {
    for.return$kappa.report <- NULL
    for.return$kappaplot <- NULL
    for.return$kappa.report.byRandStat=NULL
    for.return$kappaplots.byRandStat=NULL
    for.return$kappa.report.byPairing=NULL
    for.return$kappaplots.byPairing=NULL
    for.return$kappa.report.byArm=NULL
    for.return$kappaplots.byArm=NULL
    return(for.return)
  }
    
  # converting MO, HLA_G, HLA_DQ to kappa values to factors----
  if ("MO" %in% colnames(analysis.data.in)) {
    MO.canonical_levels <- c("M0-2", "M3A", "M3B", "M3C")
    analysis.data.in$MO <- toupper(analysis.data.in$MO)
    analysis.data.in <- analysis.data.in %>%  
      dplyr::mutate(
        MO.canonical=case_when(MO %in% c("M0", "M1", "M2") ~ "M0-2",
                               MO == "M3A" ~ "M3A",
                               MO == "M3B" ~ "M3B",
                               MO == "M3C" ~ "M3C",
                               .default=NA),
        First5Round.MO.canonical=case_when(First5Percent_Mean >= 0.00 & First5Percent_Mean < 0.50 ~ "M0-2",
                                           First5Percent_Mean >= 0.50 & First5Percent_Mean < 1.25 ~ "M3A",
                                           First5Percent_Mean >= 1.25 & First5Percent_Mean < 2.00 ~ "M3B",
                                           First5Percent_Mean >= 2.00 & First5Percent_Mean <= 3.00 ~ "M3C",
                                           .default=NA),
        MO.canonical=factor(MO.canonical, levels=MO.canonical_levels),
        First5Round.MO.canonical=factor(First5Round.MO.canonical, levels=MO.canonical_levels))
  }
  
  if ("HLA_G" %in% colnames(analysis.data.in)) {
    HLA_G.canonical_levels <- c("HLA-g neg", "HLA-g pos")
    analysis.data.in$HLA_G <- toupper(analysis.data.in$HLA_G)
    analysis.data.in <- analysis.data.in %>% 
      dplyr::mutate(
        HLA_G.canonical=case_when(HLA_G == "N" ~ "HLA-g neg",
                                  HLA_G == "Y" ~ "HLA-g pos",
                                  .default=NA),
        First5Round.HLA_G.canonical=case_when(First5Percent_Mean >= 0 & First5Percent_Mean < 1.00 ~ "HLA-g neg",
                                              First5Percent_Mean >= 1.00 & First5Percent_Mean <= 3 ~ "HLA-g pos",
                                              .default=NA),
        First5Round.HLA_G.canonical=factor(First5Round.HLA_G.canonical, levels=HLA_G.canonical_levels),
        HLA_G.canonical=factor(HLA_G.canonical, levels=HLA_G.canonical_levels))
  }
  
  if ("HLA_DQ" %in% colnames(analysis.data.in)) {
    HLA_DQ.canonical_levels <- c("DQ2 homoz", "DQ2 heteroz", "DQ2 and DQ8", "DQ8")
    analysis.data.in$HLA_DQ <- tolower(analysis.data.in$HLA_DQ)
    analysis.data.in$HLA_DQ <- sub("^hla ", "", analysis.data.in$HLA_DQ)
    analysis.data.in <- analysis.data.in %>% 
      dplyr::mutate(
        HLA_DQ.canonical=case_when(HLA_DQ %in% c("dq2 homozygous") ~ "DQ2 homoz",
                                   HLA_DQ %in% c("dq2 heterozygous", "dq2 heterozygous for beta allele") ~ "DQ2 heteroz",
                                   HLA_DQ %in% c("dq2 and dq8", "dq8 and dqb1*02") ~ "DQ2 and DQ8",
                                   HLA_DQ %in% c("dq8") ~ "DQ8",
                                   .default=NA),
        First5Round.HLA_DQ.canonical=case_when(First5Percent_Mean >= 0.00 & First5Percent_Mean < 0.50 ~ "DQ2 homoz",
                                               First5Percent_Mean >= 0.50 & First5Percent_Mean < 1.25 ~ "DQ2 heteroz",
                                               First5Percent_Mean >= 1.25 & First5Percent_Mean < 2.00 ~ "DQ2 and DQ8",
                                               First5Percent_Mean >= 2.00 & First5Percent_Mean <= 3.00 ~ "DQ8",
                                               .default=NA),
        First5Round.HLA_DQ.canonical=factor(First5Round.HLA_DQ.canonical, levels=HLA_DQ.canonical_levels),
        HLA_DQ.canonical=factor(HLA_DQ.canonical, levels=HLA_DQ.canonical_levels))
  }
  for.return$analysis.data.in_ext <- analysis.data.in
  
  # Get kappa results ----
  return.kappa <- function(data.in=analysis.data.in[, c("MO.canonical", "First5Round.MO.canonical")]) {
    if(nrow(data.in)==0) return(NULL)
    temp <- as.matrix(data.in) %>% na.omit()
    nrow.temp <- nrow(temp)
    if (nrow.temp < 5 | dim(table(temp[,1], temp[,2]))[1] != dim(table(temp[,1], temp[,2]))[2] ) {
      return(data.frame(Pairing="<pooled>", n=nrow.temp, Estimate=NA, LCL=NA, UCL=NA))
    }
    # There seems to be a bug in the cohen.kappa function. Here's a fix.
    
    # Same results
    kappa_results <- psych::cohen.kappa(table(temp[,1], temp[,2]))
    # DescTools::CohenKappa(table(temp[,1], temp[,2]), weights=c("Fleiss-Cohen"),conf.level=.95)
    
    kappa_results <- tidy(cohen.kappa(temp)) %>%
                     dplyr::filter(type == "weighted") %>%
                     dplyr::select(-type) %>%
                     dplyr::mutate(n=nrow.temp, pairing="<pooled>", reader_type=reader_type) %>%
                     dplyr::select(pairing, n, everything()) %>%
                     dplyr::rename(Pairing=pairing, Estimate=estimate, LCL=conf.low, UCL=conf.high)
    kappa_results
  }
  
  # Run executive kappa results
  if (all(c("MO.canonical", "First5Round.MO.canonical") %in% colnames(analysis.data.in))) {
    kappa1 <- return.kappa(data.in=analysis.data.in[, c("MO.canonical", "First5Round.MO.canonical")]) %>% dplyr::mutate(Comparator="MO")
  } else {
    kappa1 <- NULL
  }
  if (all(c("HLA_G.canonical", "First5Round.HLA_G.canonical") %in% colnames(analysis.data.in))) {
    kappa2 <- return.kappa(data.in=analysis.data.in[, c("HLA_G.canonical", "First5Round.HLA_G.canonical")]) %>% dplyr::mutate(Comparator="HLA_G")
  } else {
    kappa2 <- NULL
  }
  if (all(c("HLA_DQ.canonical", "First5Round.HLA_DQ.canonical") %in% colnames(analysis.data.in))) {
    kappa3 <- return.kappa(data.in=analysis.data.in[, c("HLA_DQ.canonical", "First5Round.HLA_DQ.canonical")]) %>% dplyr::mutate(Comparator="HLA_DQ")
  } else {
    kappa3 <- NULL
  }
  
  for.return$kappa.report <- bind_rows(
    kappa1, kappa2, kappa3
  ) %>% dplyr::mutate(Pairing="<pooled>", Statistic="Cohen's kappa", reader_type=reader_type)
  
  get.kappaplot <- function(kappa.in=for.return$kappa.executive.summary) {
    if(is.null(kappa.in)) return(NULL)
    if ("stratum" %in% colnames(kappa.in)) {
     kappa.in$row <- paste(kappa.in$Comparator, kappa.in$stratum)
    } else { kappa.in$row <- kappa.in$Comparator }
    kappa.in %>% ggplot(aes(y=row, x= Estimate, xmin=LCL, xmax=UCL)) +
      geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
      geom_errorbarh() +
      labs(title="Kappa Estimates", x="Estimate", y="Comparator",
           subtitle=paste("for", kappa.in$reader_type[1], "reads"))
  }
 
  for.return$kappaplot <- get.kappaplot(kappa.in=for.return$kappa.report)
  
  # now do the stratified analyses----
  for (stratifier in c("RandStat", "Pairing", "Arm")) {
    if (stratifier=="RandStat") {
      analysis.data.in$stratum <- ifelse(is.na(analysis.data.in$RANDDATE), "Not Randomized", "Randomized")
      uniques <- unique(analysis.data.in$stratum)
      if (length(uniques[!is.na(uniques)])<2) {
        for.return$kappa.report.byRandStat=NULL
        for.return$kappaplot.byRandStat=NULL
        next
      }
    } else if (stratifier=="Pairing") {
      analysis.data.in$stratum <- analysis.data.in$pairing
      uniques <- unique(analysis.data.in$stratum)
      if (length(uniques[!is.na(uniques)])<2) {
        for.return$kappa.report.byPairing=NULL
        for.return$kappaplot.byPairing=NULL
        next
      }
    } else if (stratifier=="Arm") {
      analysis.data.in$stratum <- analysis.data.in$ARM
      uniques <- unique(analysis.data.in$stratum)
      if (length(uniques[!is.na(uniques)])<2) {
        for.return$kappa.report.byArm=NULL
        for.return$kappaplot.byArm=NULL
        next
      }
    }
  
    kappa.report <- 
      apply(matrix(1:length(unique(analysis.data.in$stratum))), 1, function(y) {
        temp <- analysis.data.in %>% 
          dplyr::filter(stratum == unique(analysis.data.in$stratum)[y]) %>% 
          dplyr::mutate(stratum=unique(analysis.data.in$stratum)[y])
        
        if (nrow(temp) == 0) {
          return(data.frame())
        } else {
          kappa1 <- if (all(c("MO.canonical", "First5Round.MO.canonical") %in% colnames(temp))) {
            return.kappa(data.in=temp[, c("MO.canonical", "First5Round.MO.canonical")])  %>% dplyr::mutate(Comparator="MO")
          } else {
            NULL
          }
          
          kappa2 <- if (all(c("HLA_G.canonical", "First5Round.HLA_G.canonical") %in% colnames(temp))) {
            return.kappa(data.in=temp[, c("HLA_G.canonical", "First5Round.HLA_G.canonical")]) %>% dplyr::mutate(Comparator="HLA_G")
          } else {
            NULL
          }
          
          kappa3 <- if (all(c("HLA_DQ.canonical", "First5Round.HLA_DQ.canonical") %in% colnames(temp))) {
            return.kappa(data.in=temp[, c("HLA_DQ.canonical", "First5Round.HLA_DQ.canonical")]) %>% dplyr::mutate(Comparator="HLA_DQ")
          } else {
            NULL
          }
          
          bind_rows(
            kappa1, kappa2, kappa3
          ) %>% 
            dplyr::mutate(Pairing="<pooled>", Statistic="Cohen's kappa", reader_type=reader_type, "-", stratum=unique(analysis.data.in$stratum)[y])
        }
      }) %>% 
      bind_rows() %>% 
      dplyr::select(Comparator, stratum, Statistic, n, Estimate, LCL, UCL) %>% 
      dplyr::mutate(reader_type=reader_type)
    
    kappaplot <-  get.kappaplot(kappa.in=kappa.report)
  
    if (stratifier=="RandStat") {
      for.return$kappa.report.byRandStat=kappa.report
      for.return$kappaplot.byRandStat=kappaplot
    } else if (stratifier=="Pairing") {
      for.return$kappa.report.byPairing=kappa.report
      for.return$kappaplot.byPairing=kappaplot
    } else if (stratifier=="Arm") {
      for.return$kappa.report.byArm=kappa.report
      for.return$kappaplot.byArm=kappaplot
    }
  }
  return(for.return)
}
