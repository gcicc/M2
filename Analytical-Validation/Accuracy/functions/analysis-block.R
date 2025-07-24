#' Perform Main Analysis for Accuracy Module
#'
#' This function performs the core accuracy analysis by processing input data, applying 
#' study-specific transformations, and generating Deming regression analysis results.
#' The function handles multiple studies and creates expert panel data for comparison
#' with ML algorithm or human reader results.
#'
#' @param analysis.data.in Data frame containing analysis input data with required columns:
#'   \itemize{
#'     \item STUDYID: Study identifier (character)
#'     \item SITE: Site identifier (character)  
#'     \item SUBJID: Subject identifier (character)
#'     \item READER: Reader type identifier (character, e.g., "MLV21", "HumanAve")
#'     \item Endpoint columns: Numeric endpoint values (e.g., BVA, First5Percent_Mean)
#'     \item ARM: Treatment arm (for TAK-062-2001)
#'     \item VISIT: Visit identifier (for TAK-062-2001)
#'   }
#' @param this_endpoint String specifying which endpoint to analyze. Must be one of the 
#'   valid endpoint names: "BVA", "First5Percent_Mean", "All_Mean", "All_Max", etc.
#'
#' @return Named list containing analysis results with the following components:
#'   \describe{
#'     \item{data}{Processed data frame with Expert_panel and Reader columns}
#'     \item{deming.report}{Data frame with Deming regression coefficients and statistics}
#'     \item{Linearity.check}{Data frame with linearity assessment results}
#'     \item{truth.panel.CI}{Confidence intervals for paired differences}
#'     \item{cubic.fit}{Cubic regression model object}
#'     \item{quadratic.fit}{Quadratic regression model object}
#'     \item{CIs}{Combined confidence intervals for truth panel and coverage probability}
#'   }
#'
#' @details
#' The function performs study-specific data processing:
#' \itemize{
#'   \item Milan: Creates mock expert panel using 3 simulated readers with outlier detection
#'   \item TAK-062-2001: Samples 72 subjects and creates mock human expert panel
#'   \item Sheffield: Uses human average as expert panel reference
#' }
#' 
#' Analysis includes:
#' \itemize{
#'   \item Deming regression with error ratio = 1
#'   \item Linearity assessment using cubic and quadratic terms
#'   \item Truth panel analysis with paired differences
#'   \item Coverage probability calculation (proportion within 0.5 units)
#' }
#'
#' @examples
#' \dontrun{
#'   # Load prepared data
#'   data <- form_merged_data_sets("Accuracy", "TAK-062-2001", "BVA", output_dir)
#'   analysis_data <- bind_rows(data$merged_data.ML, data$merged_data.Human)
#'   
#'   # Run accuracy analysis
#'   results <- main_analysis(analysis_data, "BVA")
#'   
#'   # Access results
#'   print(results$deming.report)
#'   print(results$Linearity.check)
#' }
#'
#' @seealso
#' \code{\link{form_merged_data_sets}} for data preparation
#' \code{\link{mcr::mcreg}} for Deming regression implementation
#' 
#' @export
main_analysis <- function(analysis.data.in=analysis.data.in, this_endpoint=this_endpoint) {
  
  # Placeholder code...
  # data manipulation steps to separate expert panel from MLA/human reader
  reader_type = unique(analysis.data.in$READER)[grepl(x=unique(analysis.data.in$READER), pattern="MLV")]
  set.seed(123)
  if(analysis.data.in$STUDYID[1] == "Milan"){
  for.deming <- analysis.data.in %>% dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, READER, !!sym(this_endpoint)) %>%
    pivot_wider(names_from = READER, values_from = !!sym(this_endpoint)) %>%
    # Create mock readers - this will need to be updated to reflect the actual readers
    dplyr::mutate(LE2 = LE + rnorm(n(), 0, .025),
                  LE3 = LE + rnorm(n(), 0, .025)) %>%
    dplyr::mutate(LE2 = if_else(LE2 < 0, 0, LE2),
                  LE3 = if_else(LE3 < 0, 0, LE3),
                  LE2 = if_else(LE2 > 3, 3, LE2),
                  LE3 = if_else(LE3 > 3, 3, LE3)) %>%
    dplyr::rename_with(~paste0("Panelist_", seq_along(.)), tail(names(.), 3)) %>%
    rowwise() %>%
    mutate(
      Row_Median = median(c(Panelist_1, Panelist_2, Panelist_3), na.rm = TRUE),
      Row_MAD = median(abs(c(Panelist_1, Panelist_2, Panelist_3) - median(c(Panelist_1, Panelist_2, Panelist_3), na.rm = TRUE)), na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    dplyr::mutate(Panelist_1.z = .6745*(Panelist_1 - Row_Median)/Row_MAD,
                  Panelist_2.z = .6745*(Panelist_3 - Row_Median)/Row_MAD,
                  Panelist_3.z = .6745*(Panelist_3 - Row_Median)/Row_MAD) %>%
    rowwise() %>%
    dplyr::mutate(
      Expert_panel = mean(c(
        if_else(abs(Panelist_1.z) < 3.5, Panelist_1, NA_real_),
        if_else(abs(Panelist_2.z) < 3.5, Panelist_2, NA_real_),
        if_else(abs(Panelist_3.z) < 3.5, Panelist_3, NA_real_)
      ), na.rm = TRUE)
    ) %>% 
    dplyr::mutate(Expert_panel = if_else(is.na(Expert_panel) == TRUE & Row_MAD == 0, mean(c(Panelist_1, Panelist_2, Panelist_3), na.rm=TRUE), Expert_panel )) %>%
    ungroup() %>% dplyr::select(-Row_Median, -Row_MAD, -Panelist_1.z, -Panelist_2.z, -Panelist_3.z)
  }
  if(analysis.data.in$STUDYID[1] == "TAK-062-2001"){
    # First, let's subset data to mimick size anticipated in the final dataset
    set.seed(123)
    analysis.data.in.ML <- analysis.data.in %>% 
      dplyr::filter(READER == "MLV21_RetrainAll") %>%
      dplyr::filter(VISIT == "VISIT 2 - Week -4") %>%
      group_by(STUDYID, SITE, SUBJID, ARM, READER) %>%
      arrange(STUDYID, SITE, SUBJID, ARM, READER, OBSDATE.VCE) %>%
      ungroup() %>%
      sample_n(72)  %>%
      dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, READER, OBSDATE.VCE, !!sym(this_endpoint)) 
    # Create mock readers - this will need to be updated to reflect the actual readers
    analysis.data.in.Human <- bind_rows(
      analysis.data.in.ML %>% dplyr::mutate(READER="E1", !!sym(this_endpoint)  := !!sym(this_endpoint) + rnorm(n(), -.1, .25)),
      analysis.data.in.ML %>% dplyr::mutate(READER="E2", !!sym(this_endpoint)  := !!sym(this_endpoint) + rnorm(n(), .1, .25)),
      analysis.data.in.ML %>% dplyr::mutate(READER="E3", !!sym(this_endpoint)  := !!sym(this_endpoint) + rnorm(n(), 0, .25))) %>%
      dplyr::mutate(!!sym(this_endpoint) := ifelse(!!sym(this_endpoint) < 0, 0, !!sym(this_endpoint)),
                    !!sym(this_endpoint) := ifelse(!!sym(this_endpoint) > 3, 3, !!sym(this_endpoint)))
    
    temp <- bind_rows(analysis.data.in.ML, analysis.data.in.Human) 
    for.deming <- temp %>% dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, READER,OBSDATE.VCE, !!sym(this_endpoint)) %>%
      group_by(STUDYID, SITE, SUBJID, ARM, VISIT, READER, OBSDATE.VCE) %>%
      pivot_wider(names_from = READER, values_from = !!sym(this_endpoint)) %>%
      # Update logic to select human readers post pivot_wider
    dplyr::rename_with(~paste0("Panelist_", seq_along(.)), tail(names(.), 3)) %>%
      rowwise() %>%
      mutate(
        Row_Median = median(c(Panelist_1, Panelist_2, Panelist_3), na.rm = TRUE),
        Row_MAD = median(abs(c(Panelist_1, Panelist_2, Panelist_3) - median(c(Panelist_1, Panelist_2, Panelist_3), na.rm = TRUE)), na.rm = TRUE)
      ) %>%
      ungroup() %>% 
      dplyr::mutate(Panelist_1.z = .6745*(Panelist_1 - Row_Median)/Row_MAD,
                    Panelist_2.z = .6745*(Panelist_3 - Row_Median)/Row_MAD,
                    Panelist_3.z = .6745*(Panelist_3 - Row_Median)/Row_MAD) %>%
      rowwise() %>%
      dplyr::mutate(
        Expert_panel = mean(c(
          if_else(abs(Panelist_1.z) < 3.5, Panelist_1, NA_real_),
          if_else(abs(Panelist_2.z) < 3.5, Panelist_2, NA_real_),
          if_else(abs(Panelist_3.z) < 3.5, Panelist_3, NA_real_)
        ), na.rm = TRUE)
      ) %>% 
      dplyr::mutate(Expert_panel = if_else(is.na(Expert_panel) == TRUE & Row_MAD == 0, mean(c(Panelist_1, Panelist_2, Panelist_3), na.rm=TRUE), Expert_panel )) %>%
      ungroup() %>% dplyr::select(-Row_Median, -Row_MAD, -Panelist_1.z, -Panelist_2.z, -Panelist_3.z)
  }
  if(analysis.data.in$STUDYID[1] == "Sheffield"){
    # Let's treat Human Average as Expert Panel...
    for.deming <- analysis.data.in %>% dplyr::select(STUDYID, SITE, SUBJID, READER, Tertile1_Mean)
    for.deming <- for.deming %>% dplyr::filter(READER %in% c("MLV22", "HumanAve"))
    for.deming <- for.deming %>% pivot_wider(names_from = READER, values_from = Tertile1_Mean)
    for.deming <- for.deming %>% dplyr::rename(Expert_panel = HumanAve, MLV22 = MLV22)
    
  }
  # Creating a dummy data set for BVA-------------
   # Extract columns from dataset
  x <- for.deming %>% dplyr::pull(Expert_panel) # Expert panel
  y <- for.deming %>% dplyr::pull(contains("MLV")) # ML Algorithm or human reader
  # Additional steps to ensure that SUBJIDs are lined up
  temp.data <- data.frame(
    `Expert panel` = x,
    `Reader` = y,
    reader_type = reader_type,
    this_endpoint = this_endpoint)
  

  # Deming regression fit.----------------
  # The confidence intervals for regression coefficients
  # are calculated with analytical method
  deming <- mcr::mcreg(
    x,
    y,
    error.ratio = 1,
    method.reg = "Deming",
    method.ci = "analytical",
    mref.name = "Expert Panel",
    mtest.name = reader_type,
    na.rm = TRUE
  )

  deming.report <- data.frame(coef(deming)) %>% mutate(
    Statistic = EST / SE,
    p = 2 * pt(abs(Statistic), df = length(x) - 2, lower.tail = FALSE),
    Endpoint = this_endpoint,
    reader_type = reader_type
  )
  deming.report <- deming.report %>%
    mutate(Term = rownames(.)) %>%
    dplyr::select(Term, Endpoint, reader_type, everything()) %>%
    dplyr::rename(LCL = LCI, UCL = UCI)

  # Linearity check --------------------
  # Polynomial fit
  x2 <- x^2
  x3 <- x^3
  cubic.fit <- lm(y ~ x + x2 + x3)
  cubic.summary <- tidy(summary(cubic.fit))
  cubic.check <- cubic.summary$p.value[4] > .05

  quadratic.fit <- lm(y ~ x + x2)
  quadratic.summary <- tidy(summary(quadratic.fit))
  quad.check <- quadratic.summary$p.value[3] > .05

  Linearity.check <- data.frame(
    Check = c(
      "Insignificant Cubic Term",
      "Insignificant Quadratic Term",
      "Linearity Intercept Check",
      "Linearity Slope Check",
      "Linearity Check"
    ),
    Value = c(
      cubic.check,
      quad.check,
      abs(deming.report$EST[1]) < 0.5,
      deming.report$EST[2] > 0.8 & deming.report$EST[2] < 1.25,
      quad.check &
        cubic.check &
        abs(deming.report$EST[1]) < 0.5 &
        (deming.report$EST[2] > 0.8 & deming.report$EST[2] < 1.25)
    ),
    Endpoint = this_endpoint,
    reader_type = reader_type
  )

  # Truth panel------------------
  truth.panel.CI <- data.frame(x = x, y = y, diff = y - x) %>%
    dplyr::summarize(
      `Estimate` = mean(diff),
      `SE` = sd(diff) / sqrt(n()),
      n = n(),
      LCL = mean(diff) - qt(p = .975, df = n() - 1) * sd(diff) / sqrt(n()),
      UCL = mean(diff) + qt(p = .975, df = n() - 1) * sd(diff) / sqrt(n()),
      Endpoint = this_endpoint,
      reader_type = reader_type
    ) %>%
    dplyr::mutate(metric = "Paired Differences") %>%
    dplyr::select(metric, Endpoint, reader_type, everything())

  coverage.prob <- data.frame(x = x, y = y, diff = y - x) %>%
    dplyr::summarize(n = n(), Estimate = mean(abs(diff) < .5)) %>%
    dplyr::mutate(
      SE = sqrt(Estimate * (1 - Estimate) / n),
      LCL = Estimate - 1.96 * sqrt(Estimate * (1 - Estimate) / n),
      UCL = Estimate + 1.96 * sqrt(Estimate * (1 - Estimate) / n),
      Endpoint = this_endpoint,
      reader_type = reader_type
    ) %>%
    dplyr::mutate(metric = "Coverage Probability") %>%
    dplyr::select(metric, Endpoint, reader_type, everything())


  list(
    data = temp.data,
    deming.report = deming.report,
    Linearity.check = Linearity.check,
    truth.panel.CI = truth.panel.CI,
    cubic.fit = cubic.fit,
    quadratic.fit = quadratic.fit,
    CIs = bind_rows(truth.panel.CI, coverage.prob)
  )
}
